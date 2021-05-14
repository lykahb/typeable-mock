{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeableMock
  ( 
    -- * Types
    Mock (..),
    MockConfig (..),
    defaultMockConfig,
    addMocksToConfig,
    -- * Creating and calling mocks
    makeMock,
    lookupMockFunction,
    constN,
    -- * Checking calls
    MockFailure (..),
    MockFailureReason (..),
    ExpectedCallRecord (..),
    ExpectedArg (..),
    ActualCallRecord (..),
    ActualArg (..),
    lookupMock,
    lookupMockTyped,
    assertHasCalls,
    assertNotCalled,
    assertAnyCall,
    getCalls,
    expectCall,
    callMatches,
    resetMockCallRecords,
    resetAllCallRecords,
    -- * Mocking polymorphic monads
    MockMonadIO (..),
    fromMockMonadIO,
    unMockMonadIO1,
    unMockMonadIO2,
    unMockMonadIO3,
    unMockMonadIO4,
    unMockMonadIO5
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.CallStack (HasCallStack, SrcLoc, callStack)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, cast, typeOf, typeRep, eqT, (:~:)(..))
import Prelude
import TypeableMock.Types

data ActualCallRecord = forall a. Typeable a => ActualCallRecord [ActualArg] a

data ActualArg = forall a. Typeable a => ActualArg a

data Mock = forall x. Typeable x => Mock {
  mockKey :: String,
  mockCallRecord :: IORef [ActualCallRecord],
  mockFunction :: IORef [ActualCallRecord] -> x
  }

instance Show Mock where
  show Mock {..} = "Mock (" <> mockKey <> " :: " <> show tRep <> ")" where
    tRep = typeRep mockFunction

data MockConfig = MockConfig
  { mcStorage :: Map String (Map TypeRep Mock),
    mcFailOnLookup :: Bool
  }
  deriving stock (Show)

defaultMockConfig :: MockConfig
defaultMockConfig = MockConfig mempty False

data MockFailure = MockFailure {
  mfMock :: Mock,
  mfLocation :: Maybe SrcLoc,
  mfReason :: MockFailureReason
}
  
data MockFailureReason
  = MockFailureArgumentCountMismatch ExpectedCallRecord ActualCallRecord
  | MockFailureArgumentTypeMismatch TypeRep TypeRep
  | forall a. Show a => MockFailureArgumentValueMismatch a a
  | forall a. Show a => MockFailureArgumentPredicateFailure a
  | MockFailureUnexpectedCall ActualCallRecord
  | MockFailureNotCalled ExpectedCallRecord

instance Show MockFailure where
  show MockFailure {..} = intercalate "\n" $ show mfLocation:case mfReason of
    MockFailureArgumentCountMismatch (ExpectedCallRecord expArgs _) (ActualCallRecord actArgs _) ->
      ["Number of arguments:", "expected: " ++ show (length expArgs), "but got: " ++ show (length actArgs)]
    MockFailureArgumentTypeMismatch expected actual -> 
      ["Value type does not match:", "expected: " ++ show expected, "but got: " ++ show actual]
    MockFailureArgumentValueMismatch expected actual -> 
      ["Expected: " ++ show expected, "but got: " ++ show actual]
    MockFailureArgumentPredicateFailure actual  -> 
      ["Predicate failed:", show actual]
    MockFailureUnexpectedCall _ ->
      ["Unexpected call:", "TODO: print details"]
    MockFailureNotCalled (ExpectedCallRecord expArgs _)  -> 
      ["Expected call with arguments: " ++ show expArgs, "but was not called"]

instance Exception MockFailure

-- | Wraps the function into a `Mock`. For successful lookup at the call site,
-- the type of the passed function must match the type of the mocked function.
-- 
-- If the mocked function has polymorphic arguments, such as @print :: Show a => a -> IO ()@,
-- create a mock for each case. For example, if an app prints Int and Strings, create
-- two mocks:
-- 
-- @
-- mockConf \<- 'addMocksToConfig' defaultConf \<$> sequence
--   [ makeMock "print" (const $ pure () :: Int -> IO ()),
--   , makeMock "print" (const $ pure () :: String -> IO ())
--   ]
-- @
makeMock :: (Function Typeable f args (m x), Typeable f, Typeable x, MonadIO m)
  => String -> f -> IO Mock
makeMock key f = do
  actualCallRecord <- newIORef []
  pure $ Mock key actualCallRecord (`recordArgs` f)

-- | Constant function for an arbitrary number of arguments.
-- In combination with `asTypeOf` it lets you omit the type annotation.
--
-- > makeMock "getSomething" (constN "something" `asTypeOf` getSomething)
constN :: EmptyConstraint a => a -> Function EmptyConstraint f args a => a -> f
constN a = createFunction (Proxy :: Proxy EmptyConstraint) const (const a) ()

-- | A helper function to lookup the function. Likely you want to write a wrapper
-- that retrieves the @MockConfig@ from the environment.
-- 
-- > withMock :: String -> f -> AppMonad f
-- > withMock key f = do
-- >   mockConf <- getMockConfig <$> getEnv
-- >   pure $ fromMaybe f (lookupMockFunction mockConf key)
-- >
-- > withMock "getSomething" getSomething >>= \f -> f someArg
lookupMockFunction :: forall f. Typeable f => MockConfig -> String -> Maybe f
lookupMockFunction MockConfig{..} key = do
  let tRep = typeRep (Proxy :: Proxy f)
  case Map.lookup key mcStorage >>= Map.lookup tRep of
    Just Mock {..} -> case cast (mockFunction mockCallRecord) of
      Just val -> Just val
      Nothing -> error $ "lookupMockFunction: impossible happened. Cast failed for " <> key
    Nothing | mcFailOnLookup -> error $ "lookupMockFunction: Mock" <> key <> " not found"
    Nothing -> Nothing

recordArgs :: (Typeable x, MonadIO m, Function Typeable f args (m x)) =>
  IORef [ActualCallRecord] -> f -> f
recordArgs callsRef = transformFunction (Proxy :: Proxy Typeable) fa fr [] where
  fa args a = ActualArg a:args
  fr args mx = do
    x <- mx
    liftIO $ modifyIORef callsRef (ActualCallRecord (reverse args) x:)
    pure x

-- Reuse the mocks between the test items
resetMockCallRecords :: Mock -> IO ()
resetMockCallRecords (Mock _ callsRef _) = writeIORef callsRef []

-- Reuse the mocks between the test items
resetAllCallRecords :: MockConfig -> IO ()
resetAllCallRecords MockConfig{..} = mapM_ (mapM_ resetMockCallRecords) mcStorage

-- | Finds a mock by name. If there are several mocks under the same name, use `lookupMockTyped`.
lookupMock :: HasCallStack => MockConfig -> String -> Mock
lookupMock MockConfig{..} key = case Map.lookup key mcStorage of
  Nothing -> error $ "lookupMock: Mock " <> key <> " not found"
  Just tMap -> case Map.toList tMap of
    [(_, mock)] -> mock
    _ -> error $ "lookupMock: There are " <> show (Map.size tMap) <> " mocks under the name \"" <> key <> "\". Use lookupMockTyped to disambiguate."

lookupMockTyped :: forall t proxy . (HasCallStack, Typeable t) => MockConfig -> String -> proxy t -> Mock
lookupMockTyped MockConfig{..} key t = case Map.lookup key mcStorage of
  Nothing -> error $ "lookupMockTyped: Mock " <> key <> " not found"
  Just tMap -> case Map.lookup (typeRep t) tMap of
    Just mock -> mock
    _ -> error $ "lookupMockTyped: Mock " <> key <> "not found."

addMocksToConfig :: MockConfig -> [Mock] -> MockConfig
addMocksToConfig conf mocks = conf {mcStorage = mockMap}
  where
    mockMap = foldr insertMock (mcStorage conf) mocks
    insertMock mock@(Mock key _ _) = Map.insertWith (<>) key (toTypeMap mock)
    toTypeMap :: Mock -> Map TypeRep Mock
    toTypeMap mock@(Mock _ _ (_ :: IORef [ActualCallRecord] -> f)) =
      Map.singleton (typeRep (Proxy :: Proxy f)) mock

data ExpectedArg
  = AnyArg
  | forall a. (Typeable a, Show a, Eq a) => ExpectedArg a
  | forall a . (Typeable a, Show a) => PredicateArg (a -> Bool)

instance Eq ExpectedArg where
  (==) = error "Eq ExpectedArg not implemented"

data ExpectedCallRecord = ExpectedCallRecord [ExpectedArg] ExpectedArg

instance Show ExpectedArg where
  show AnyArg = "AnyArg"
  show (ExpectedArg a) = show a
  show (PredicateArg p) = "PredicateArg p :: " <> show (typeOf p)

-- | Use this to create `ExpectedCallRecord`
--
-- Examples:
--
-- >>> expectCall "email@example.com" True
expectCall :: (Function (Show & Eq & Typeable) f args ExpectedCallRecord)
  => f
expectCall = createFunction (Proxy :: Proxy (Show & Eq & Typeable)) fa fr [] where
  fa args arg = case cast arg of
    Just arg' -> arg' : args
    Nothing -> ExpectedArg arg : args
  fr args = ExpectedCallRecord (reverse args) AnyArg

checkCallRecord :: HasCallStack => ActualCallRecord -> ExpectedCallRecord -> Maybe MockFailureReason
checkCallRecord actCall@(ActualCallRecord actualArgs _) expCall@(ExpectedCallRecord expectedArgs _) =
  if length expectedArgs /= length actualArgs
    then Just $ MockFailureArgumentCountMismatch expCall actCall
    else listToMaybe $ catMaybes $ zipWith matchArgs expectedArgs actualArgs

matchArgs :: ExpectedArg -> ActualArg -> Maybe MockFailureReason
matchArgs AnyArg _ = Nothing
matchArgs (ExpectedArg expected) (ActualArg actual) = 
  case cast actual of
    Just actual' | expected == actual' -> Nothing
    Just actual' -> Just $
      MockFailureArgumentValueMismatch expected actual'
    Nothing -> Just $
      MockFailureArgumentTypeMismatch (typeOf expected) (typeOf actual)
matchArgs (PredicateArg (p :: a -> Bool)) (ActualArg (actual :: b)) =
  case eqT :: Maybe (a :~: b) of
    Nothing -> Just $ MockFailureArgumentTypeMismatch (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))
    Just Refl -> if p actual
      then Nothing
      else Just $ MockFailureArgumentPredicateFailure actual

-- | Assert that mock has been called, and match the expected calls in a given order.
assertHasCalls :: HasCallStack => [ExpectedCallRecord] -> Mock -> IO ()
assertHasCalls expectedCalls mock = do
  actualCalls <- getCalls mock
  zipEqualLength actualCalls expectedCalls
  where
  throw = throwIO . MockFailure mock location
  zipEqualLength [] [] = pure ()
  zipEqualLength (a:as) (e:es) = do
    maybe (pure ()) throw $ checkCallRecord a e
    zipEqualLength as es
  zipEqualLength (a:_) _ = throw $ MockFailureUnexpectedCall a
  zipEqualLength _ (e:_) = throw $ MockFailureNotCalled e

assertNotCalled :: HasCallStack => Mock -> IO ()
assertNotCalled = assertHasCalls []

callMatches :: ExpectedCallRecord -> ActualCallRecord -> Bool
callMatches expCall actCall = isNothing $ checkCallRecord actCall expCall

assertAnyCall :: ExpectedCallRecord -> Mock -> IO ()
assertAnyCall expCall mock = do
  actualCalls <- getCalls mock
  if any (callMatches expCall) actualCalls
    then pure ()
    else throwIO $ MockFailure mock location $ MockFailureNotCalled expCall

-- | Get list of calls. Use together with 'callMatches' when assert* from 
getCalls :: Mock -> IO [ActualCallRecord]
getCalls Mock {..} = reverse <$> readIORef mockCallRecord

location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing

-- | Helper for making polymorphic mock functions.
newtype MockMonadIO a = MockMonadIO {unMockMonadIO :: forall m. MonadIO m => m a}

instance Functor MockMonadIO where
  fmap f (MockMonadIO m) = MockMonadIO (fmap f m)

instance Applicative MockMonadIO where
  pure a = MockMonadIO (pure a)
  MockMonadIO f <*> MockMonadIO a = MockMonadIO (f <*> a)

instance Monad MockMonadIO where
  MockMonadIO ma >>= f = MockMonadIO $ ma >>= unMockMonadIO . f

instance MonadIO MockMonadIO where
  liftIO m = MockMonadIO (liftIO m)

unMockMonadIO1 :: (a -> MockMonadIO x) -> (forall m. MonadIO m => a -> m x)
unMockMonadIO1 f = unMockMonadIO . f

unMockMonadIO2 :: (a -> b -> MockMonadIO x) -> (forall m. MonadIO m => a -> b -> m x)
unMockMonadIO2 f = unMockMonadIO1 . f

unMockMonadIO3 :: (a -> b -> c -> MockMonadIO x) -> (forall m. MonadIO m => a -> b -> c -> m x)
unMockMonadIO3 f = unMockMonadIO2 . f

unMockMonadIO4 :: (a -> b -> c -> d -> MockMonadIO x) -> (forall m. MonadIO m => a -> b -> c -> d -> m x)
unMockMonadIO4 f = unMockMonadIO3 . f

unMockMonadIO5 :: (a -> b -> c -> d -> e -> MockMonadIO x) -> (forall m. MonadIO m => a -> b -> c -> d -> e-> m x)
unMockMonadIO5 f = unMockMonadIO4 . f

-- unMockMonadIO5
--  :: (a -> b -> c -> d -> e -> MockMonadIO x)
--  -> (forall m. (FunctionResult (m x) ~ m x, MonadIO m, FunctionArgs (m x) ~ '[])
--      => a -> b -> c -> d -> e-> m x)
-- unMockMonadIO5 f = composeN unMockMonadIO

-- | Changes the return type of a function from @MockMonadIO x@ to @m x@.
-- The @m@ must be a monomorphic type caller.
-- If the caller is in a polymorphic monad, use one of the @unMockMonadION@ instead.
fromMockMonadIO :: forall m x args f f' .
  (MonadIO m, Function EmptyConstraint f args (MockMonadIO x), Function EmptyConstraint f' args (m x))
  => f -> f'
fromMockMonadIO = composeN unMockMonadIO
