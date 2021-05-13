{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeApplications #-}
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Test.Hspec
import TypeableMock

main :: IO ()
main = hspec $ do
  let withMock :: Typeable a => MockConfig -> a -> String -> a
      withMock conf f key = fromMaybe f (lookupMockFunction conf key)
      printWithMock :: (Show a, Typeable a) => MockConfig -> a -> IO ()
      printWithMock conf = withMock conf print "print"
  printStringMock <- runIO $ makeMock "print" (const $ pure () :: String -> IO ())

  let mockConf :: MockConfig
      mockConf = defaultMockConfig `addMocksToConfig` [printStringMock]

  describe "Mock" $ before_ (resetAllCallRecords mockConf) $ do
    it "mocks a single argument function" $ do
      printWithMock mockConf "some string"
      flip assertHasCalls printStringMock [expectCall "some string"]

    it "mocks a multiple arguments function" $ do
      let print2 :: Int -> Int -> IO ()
          print2 a b = print (a, b)
          print2WithMock conf = withMock conf print2 "print2" 1 2
      
      print2Mock <- makeMock "print2" ((\_ _ -> pure ()) :: Int -> Int -> IO ())
      print2WithMock $ defaultMockConfig `addMocksToConfig` [print2Mock]
      flip assertHasCalls print2Mock [expectCall (1 :: Int) (2 :: Int)]
    
    it "mocks multiple calls" $ do
      printWithMock mockConf "some string"
      printWithMock mockConf "another string"
      flip assertHasCalls printStringMock [
        expectCall "some string",
        expectCall "another string"
        ]

    it "shows correctly" $ do
      mock <- makeMock "print" (const $ pure () :: Int -> IO ())
      show mock `shouldBe` "Mock (print :: Int -> IO ())"

    it "can dispatch mocks with the same name and different types" $ do
      printIntMock <- makeMock "print" (const $ pure () :: Int -> IO ())
      let mockConf' = mockConf `addMocksToConfig` [printIntMock]
      printWithMock mockConf' "some string"
      printWithMock mockConf' (1 :: Int)
      flip assertHasCalls printStringMock [expectCall "some string"]
      flip assertHasCalls printIntMock [expectCall (1 :: Int)]

    it "fails when there are more calls than expected" $ do
      printWithMock mockConf "some string"
      printWithMock mockConf "another string"
      flip assertHasCalls printStringMock [expectCall "some string"] `shouldThrow` \case
        MockFailure {mfReason=MockFailureUnexpectedCall {}} -> True; _ -> False

    it "fails when there are fewer calls than expected" $ do
      printWithMock mockConf "some string"
      flip assertHasCalls printStringMock [
        expectCall "some string",
        expectCall "another string"
        ] `shouldThrow` \case
          MockFailure {mfReason=MockFailureNotCalled {}} -> True; _ -> False
    
    it "fails when a call is with unexpected arguments" $ do
      printWithMock mockConf "some string"
      flip assertHasCalls printStringMock [expectCall "another string"] `shouldThrow` \case
        MockFailure {mfReason=MockFailureArgumentValueMismatch {}} -> True; _ -> False
    
    it "ignores expected argument when it is AnyArg" $ do
      printWithMock mockConf "some string"
      flip assertHasCalls printStringMock [expectCall AnyArg]

    it "checks predicate for argument" $ do
      printWithMock mockConf "some string"
      flip assertHasCalls printStringMock [
        expectCall (PredicateArg (== "some string"))]
      flip assertHasCalls printStringMock [
        expectCall (PredicateArg (== "another string"))
        ] `shouldThrow` \case
          MockFailure {mfReason=MockFailureArgumentPredicateFailure {}} -> True; _ -> False

    it "fails when mcFailOnLookup is set" $ do
      let mockConf' = mockConf { mcFailOnLookup = True }
      withMock mockConf' print "printDoesNotExist" () `shouldThrow` anyErrorCall
    
    it "works inside of a polymorphic monad" $ do
      -- Polymorphic types cannot be used with Typeable typeOf. This library has a workaround for monads.
      let printInMonadIO :: forall m . MonadIO m => MockConfig -> String -> m ()
          printInMonadIO conf s = do
            fromMaybe (liftIO . print) (unMockMonadIO1 <$> lookupMockFunction conf "print") s
      let printInIO :: MockConfig -> String -> IO ()
          printInIO conf s = do
            fromMaybe print (fromMockMonadIO <$> lookupMockFunction conf "print") s
      
      printPoly <- makeMock "print" (const $ pure () :: String -> MockMonadIO ())

      let mockConf' :: MockConfig
          mockConf' = mockConf `addMocksToConfig` [printPoly]
      printInMonadIO mockConf' "some string"
      printInIO mockConf' "some string"
      flip assertHasCalls printPoly [expectCall "some string", expectCall "some string"]
    
    describe "lookup mock" $ do
      it "lookupMock finds mock by name" $ do
        show (lookupMock mockConf "print") `shouldBe` show printStringMock

      it "lookupMock fails when there are multiple mocks with the same name" $ do
        printIntMock <- makeMock "print" ((\_  -> pure ()) :: Int -> IO ())
        let mockConf' = mockConf `addMocksToConfig` [printIntMock]
        (lookupMock mockConf' "print" `seq` pure ()) `shouldThrow`
          errorCall "lookupMock: There are 2 mocks under the name \"print\". Use lookupMockTyped to disambiguate."
      
      it "lookupMockTyped finds mock by name and type" $ do
        printIntMock <- makeMock "print" ((\_  -> pure ()) :: Int -> IO ())
        let mockConf' = mockConf `addMocksToConfig` [printIntMock]
        let mock = lookupMockTyped mockConf' "print" (undefined :: p (Int -> IO ()))
        show mock `shouldBe` show printIntMock

    describe "assertNotCalled" $ do
      it "succeeds when mock was not called" $ do
        assertNotCalled printStringMock

      it "fails when mock was called" $ do
        printWithMock mockConf "some string"
        assertNotCalled printStringMock `shouldThrow` \case
          MockFailure {mfReason=MockFailureUnexpectedCall {}} -> True; _ -> False
