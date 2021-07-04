{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromMaybe, isNothing)
import Data.Typeable (Typeable)
import Test.Hspec
import Test.TypeableMock

main :: IO ()
main = hspec $ do
  let withMock :: Typeable a => MockConfig -> a -> String -> a
      withMock conf f key = fromMaybe f (lookupMockFunction conf key)
      printWithMock :: (Show a, Typeable a) => MockConfig -> a -> IO ()
      printWithMock conf = withMock conf print "print"
  printStringMock <- runIO $ makeMock "print" (const $ pure () :: String -> IO ())

  let mockConf :: MockConfig
      mockConf = defaultMockConfig `addMocksToConfig` [printStringMock]

  before_ (resetAllCallRecords mockConf) $ do
    describe "Assertions" $ do
      describe "expectCall" $ do
        it "mocks a single argument function" $ do
          printWithMock mockConf "one"
          assertHasCalls [expectCall "one"] printStringMock

        it "mocks a multiple arguments function" $ do
          let print2 :: Int -> Int -> IO ()
              print2 a b = print (a, b)
              print2WithMock conf = withMock conf print2 "print2" 1 2

          print2Mock <- makeMock "print2" ((\_ _ -> pure ()) :: Int -> Int -> IO ())
          print2WithMock $ defaultMockConfig `addMocksToConfig` [print2Mock]
          assertHasCalls [expectCall (1 :: Int) (2 :: Int)] print2Mock

        it "mocks polymorphic monads with MockMonadIO" $ do
          -- Polymorphic types cannot be used with Typeable typeOf. This library has a workaround for monads.
          let print5 a b c d e = liftIO $ print (a, b, c, d, e)
          let printInMonadIO :: forall m. MonadIO m => MockConfig -> Bool -> Char -> Char -> Char -> Char -> m ()
              printInMonadIO conf =
                maybe print5 unMockMonadIO5 (lookupMockFunction conf "print5")
          let printInIO :: MockConfig -> Bool -> Char -> Char -> Char -> Char -> IO ()
              printInIO conf =
                maybe print5 fromMockMonadIO (lookupMockFunction conf "print5")

          print5Mock <- makeMock "print5" (constN $ pure () :: Bool -> Char -> Char -> Char -> Char -> MockMonadIO ())

          let mockConf' :: MockConfig
              mockConf' = mockConf `addMocksToConfig` [print5Mock]
          printInMonadIO mockConf' True 'a' 'b' 'c' 'd'
          printInIO mockConf' True 'a' 'b' 'c' 'd'
          assertHasCalls [expectCall True 'a' 'b' 'c' 'd', expectCall True 'a' 'b' 'c' 'd'] print5Mock

      describe "assertHasCalls" $ do
        it "mocks multiple calls" $ do
          printWithMock mockConf "one"
          printWithMock mockConf "two"
          assertHasCalls
            [ expectCall "one",
              expectCall "two"
            ]
            printStringMock

        it "fails when there are more calls than expected" $
          do
            printWithMock mockConf "one"
            printWithMock mockConf "two"
            assertHasCalls [expectCall "one"] printStringMock
            `shouldThrow` \case
              MockFailure {mfReason = MockFailureUnexpectedCall {}} -> True
              _ -> False

        it "fails when there are fewer calls than expected" $ do
          printWithMock mockConf "one"
          assertHasCalls
            [ expectCall "one",
              expectCall "two"
            ]
            printStringMock
            `shouldThrow` \case
              MockFailure {mfReason = MockFailureNotCalled {}} -> True
              _ -> False

        it "fails when a call is with different arguments" $ do
          printWithMock mockConf "one"
          assertHasCalls [expectCall "two"] printStringMock `shouldThrow` \case
            MockFailure {mfReason = MockFailureArgumentValueMismatch {}} -> True
            _ -> False

        it "fails when a call returns different result" $ do
          let doubleFunc :: Int -> IO Int
              doubleFunc a = print a >> pure (a * 2)
          doubleMock <- makeMock "double" ((\a -> pure (a * 2)) `asTypeOf` doubleFunc)
          let mockConf' = mockConf `addMocksToConfig` [doubleMock]

          void $ withMock mockConf' doubleFunc "double" 5
          assertHasCalls [expectCall AnyArg `withResult` (10 :: Int)] doubleMock
          assertHasCalls [expectCall AnyArg `withResult` (11 :: Int)] doubleMock
            `shouldThrow` \case
              MockFailure {mfReason = MockFailureArgumentValueMismatch {}} -> True
              _ -> False

      describe "assertNotCalled" $ do
        it "succeeds when mock was not called" $ do
          assertNotCalled printStringMock

        it "fails when mock was called" $ do
          printWithMock mockConf "one"
          assertNotCalled printStringMock `shouldThrow` \case
            MockFailure {mfReason = MockFailureUnexpectedCall {}} -> True
            _ -> False

      describe "assertNotCalled" $ do
        it "fails when mock was not called" $ do
          printWithMock mockConf "two"
          assertAnyCall (expectCall "one") printStringMock
            `shouldThrow` \case
              MockFailure {mfReason = MockFailureNotCalled {}} -> True
              _ -> False

    describe "Config" $ do
      it "addMocksToConfig overrides mocks" $ do
        printIntMock1 <- makeMock "print" (const $ pure () :: Int -> IO ())
        printIntMock2 <- makeMock "print" (const $ pure () :: Int -> IO ())
        let mockConf' = mockConf `addMocksToConfig` [printIntMock1, printIntMock2]
        printWithMock mockConf' (1 :: Int)
        assertHasCalls [expectCall (1 :: Int)] printIntMock2

        printIntMock3 <- makeMock "print" (const $ pure () :: Int -> IO ())
        let mockConf'' = mockConf `addMocksToConfig` [printIntMock3]
        printWithMock mockConf'' (1 :: Int)
        assertHasCalls [expectCall (1 :: Int)] printIntMock3

    describe "Arguments comparison" $ do
      it "checks argument count" $ do
        printWithMock mockConf "one"
        assertHasCalls [expectCall "a" "b"] printStringMock `shouldThrow` \case
          MockFailure {mfReason = MockFailureArgumentCountMismatch {}} -> True
          _ -> False

      it "checks argument types" $ do
        printWithMock mockConf "one"
        assertHasCalls [expectCall ()] printStringMock `shouldThrow` \case
          MockFailure {mfReason = MockFailureArgumentTypeMismatch {}} -> True
          _ -> False

      it "checks predicate for argument" $ do
        printWithMock mockConf "one"
        assertHasCalls [expectCall (PredicateVal (== "one"))] printStringMock
        assertHasCalls [expectCall (PredicateVal (== "two"))] printStringMock
          `shouldThrow` \case
            MockFailure {mfReason = MockFailureArgumentPredicateFailure {}} -> True
            _ -> False

      it "ignores expected argument when it is AnyArg" $ do
        printWithMock mockConf "one"
        assertHasCalls [expectCall AnyArg] printStringMock

    describe "Retrieving mocks" $ do
      describe "lookupMock" $ do
        it "finds mock" $ do
          show (lookupMock mockConf "print") `shouldBe` show printStringMock

        it "finds mocks with different types" $ do
          printIntMock <- makeMock "print" (const $ pure () :: Int -> IO ())
          let mockConf' = mockConf `addMocksToConfig` [printIntMock]
          printWithMock mockConf' "one"
          printWithMock mockConf' (1 :: Int)
          assertHasCalls [expectCall "one"] printStringMock
          assertHasCalls [expectCall (1 :: Int)] printIntMock

        it "fails for multiple mocks with the same name" $ do
          printIntMock <- makeMock "print" ((\_ -> pure ()) :: Int -> IO ())
          let mockConf' = mockConf `addMocksToConfig` [printIntMock]
          (lookupMock mockConf' "print" `seq` pure ())
            `shouldThrow` errorCall "lookupMock: There are 2 mocks under the name \"print\". Use lookupMockTyped to disambiguate."

        it "fails when there is no mock" $ do
          (lookupMock mockConf "noSuchMock" `seq` pure ())
            `shouldThrow` errorCall "lookupMock: Mock noSuchMock not found"

      describe "lookupMockTyped" $ do
        it "finds mock" $ do
          printIntMock <- makeMock "print" ((\_ -> pure ()) :: Int -> IO ())
          let mockConf' = mockConf `addMocksToConfig` [printIntMock]
          let Just mock = lookupMockTyped @(Int -> IO ()) mockConf' "print"
          show mock `shouldBe` show printIntMock

        it "fails when mcShouldFailOnNotFound and there are no mocks with the given name " $ do
          let mockConf' = mockConf {mcShouldFailOnNotFound = \_ _ -> True}
          withMock mockConf' print "printDoesNotExist" ()
            `shouldThrow` errorCall "lookupMockTyped: cannot find mock printDoesNotExist :: () -> IO (). There are no mocks under this name."

        it "fails when mcShouldFailOnNotFound and there are other mocks with the same name" $ do
          let mockConf' = mockConf {mcShouldFailOnNotFound = \_ _ -> True}
          (lookupMockTyped @(Int -> IO ()) mockConf' "print" `seq` pure ())
            `shouldThrow` errorCall "lookupMockTyped: cannot find mock print :: Int -> IO (). There are mocks with other types under the same name:\nMock (print :: [Char] -> IO ())\n"

        it "returns Nothing when there are no mocks with the given name" $ do
          lookupMockTyped @(Int -> IO ()) mockConf "noSuchMock"
            `shouldSatisfy` isNothing

        it "returns Nothing when there are other mocks with the same name" $ do
          lookupMockTyped @(Int -> IO ()) mockConf "print"
            `shouldSatisfy` isNothing

    describe "Display" $ do
      it "shows Mock correctly" $ do
        mock <- makeMock "print" (const $ pure () :: Int -> IO ())
        show mock `shouldBe` "Mock (print :: Int -> IO ())"
