{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Test.Hspec
import TypeableMock

main :: IO ()
main = hspec $ do
  let runWithMock :: (Show a, Typeable a) => MockConfig -> a -> IO ()
      runWithMock mocks a = fromMaybe print (useMockClass mocks "print" ) a
  printStringMock <- runIO $ makeMock "print" $ mockClass (const $ pure () :: String -> IO ())

  let mockConf :: MockConfig
      mockConf = defaultMockConfig `addMocksToConfig` [printStringMock]

  describe "Mock" $ before_ (resetCallRecords printStringMock) $ do
    it "mocks a single argument function" $ do
      runWithMock mockConf "some string"
      assertHasCalls printStringMock [call "some string"]

    it "mocks a multiple arguments function" $ do
      let print2 :: Int -> Int -> IO ()
          print2 a b = print (a, b)
      let myfunc mocks = do
            fromMaybe print2 (useMockClass mocks "print2") 1 2
      print2Mock <- makeMock "print2" $ mockClass ((\_ _ -> pure ()) :: Int -> Int -> IO ())

      myfunc $ defaultMockConfig `addMocksToConfig` [print2Mock]
      assertHasCalls print2Mock [call (1 :: Int) (2 :: Int)]
    
    it "mocks multiple calls" $ do
      runWithMock mockConf "some string"
      runWithMock mockConf "another string"
      assertHasCalls printStringMock [
        call "some string",
        call "another string"
        ]

    it "can dispatch mocks with the same name and different types" $ do
      printIntMock <- makeMock "print" $ mockClass (const $ pure () :: Int -> IO ())
      let mockConf' = mockConf `addMocksToConfig` [printIntMock]
      runWithMock mockConf' "some string"
      runWithMock mockConf' (1 :: Int)
      assertHasCalls printStringMock [call "some string"]
      assertHasCalls printIntMock [call (1 :: Int)]

    it "fails when there are more calls than expected" $ do
      runWithMock mockConf "some string"
      runWithMock mockConf "another string"
      assertHasCalls printStringMock [call "some string"] `shouldThrow` \(MockFailure _ reason) ->
          case reason of MockFailureUnexpectedCall _ -> True; _ -> False
    
    it "fails when there are fewer calls than expected" $ do
      runWithMock mockConf "some string"
      assertHasCalls printStringMock [
        call "some string",
        call "another string"
        ] `shouldThrow` \(MockFailure _ reason) ->
          case reason of MockFailureNotCalled _ -> True; _ -> False
    
    it "fails when a call is with unexpected arguments" $ do
      runWithMock mockConf "some string"
      assertHasCalls printStringMock [call "another string"] `shouldThrow` \case
        MockFailure _ (MockFailureArgumentValueMismatch _ _) -> True
        _ -> False
    
    it "ignores expected argument when it is unit" $ do
      runWithMock mockConf "some string"
      assertHasCalls printStringMock [call ()]
    
    it "fails when mockConfigFailOnLookup is set" $ do
      let mockConf' = mockConf { mockConfigFailOnLookup = True }
      fromMaybe print (useMockClass mockConf' "printDoesNotExist") () `shouldThrow` anyErrorCall
    
    it "works inside of a polymorphic monad" $ do
      -- Polymorphic types cannot be used with Typeable typeOf. This library has a workaround for monads.
      let printInMonadIO :: forall m . MonadIO m => MockConfig -> String -> m ()
          printInMonadIO mocks s = do
            let mockMonadIO = useMockClass mocks "print" :: Maybe (String -> MockMonadIO ())
                forallMock = fromMockMonadIO1 <$> mockMonadIO
            fromMaybe (liftIO . print) forallMock s
      let printInIO :: MockConfig -> String -> IO ()
          printInIO mocks s = do
            fromMaybe print (fromMockMonadIO <$> useMockClass mocks "print") s
      
      printPoly <- makeMock "print" $ mockClass (\(_ :: String) -> MockMonadIO $ pure ())

      let mockConf' :: MockConfig
          mockConf' = mockConf `addMocksToConfig` [printPoly]
      printInMonadIO mockConf' "some string"
      printInIO mockConf' "some string"
      assertHasCalls printPoly [call "some string", call "some string"]
      
    describe "assertNotCalled" $ do
      it "succeeds when mock was not called" $ do
        assertNotCalled printStringMock

      it "fails when mock was called" $ do
        runWithMock mockConf "some string"
        assertNotCalled printStringMock `shouldThrow` \(MockFailure _ reason) ->
            case reason of MockFailureUnexpectedCall _ -> True; _ -> False