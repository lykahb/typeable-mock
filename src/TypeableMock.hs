{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeableMock
  ( Mock (..),
    MockConfig (..),
    MockMonadIO (..),
    CallRecord (..),
    MockFailure (..),
    MockFailureReason (..),
    MockValue (..),
    makeMock,
    addMocksToConfig,
    defaultMockConfig,
    assertHasCalls,
    assertNotCalled,
    call,
    resetCallRecords,
    mockClass,
    useMockClass,
    fromMockMonadIO0,
    fromMockMonadIO1,
    fromMockMonadIO
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.CallStack (HasCallStack, SrcLoc, callStack)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, cast, typeOf, typeRep)
import Prelude
import Data.Kind ( Type )

newtype CallRecord = CallRecord [Arg]

data Arg = forall a. Typeable a => Arg a

data Mock = Mock String (IORef [CallRecord]) MockValue

instance Show Mock where
  show (Mock key _ _) = "Mock " <> key

type WithCallRecord a = IORef [CallRecord] -> a

data MockValue = forall x. Typeable x => MockValueClass (WithCallRecord x)

data MockConfig = MockConfig
  { mockConfigStorage :: Map String (Map TypeRep Mock),
    mockConfigFailOnLookup :: Bool
  }
  deriving stock (Show)

defaultMockConfig :: MockConfig
defaultMockConfig = MockConfig mempty False

data MockFailure = MockFailure (Maybe SrcLoc) MockFailureReason

instance Show MockFailure where
  show (MockFailure loc reason) = intercalate "\n" [show loc, show reason]

data MockFailureReason
  = MockFailureArgumentTypeMismatch ExpectedArg Arg
  | forall a. Show a => MockFailureArgumentValueMismatch a a
  | MockFailureUnexpectedCall CallRecord
  | MockFailureNotCalled CallWithArgs

instance Show MockFailureReason where
  show reason = intercalate "\n" $ case reason of
    MockFailureArgumentValueMismatch expected actual -> ["expected: " ++ show expected, " but got: " ++ show actual]
    MockFailureArgumentTypeMismatch (ExpectedArg expected) (Arg actual) -> ["expected: " ++ show expected, " but got value of different type: " ++ show (typeOf actual)]
    MockFailureUnexpectedCall (CallRecord _) -> ["TODO"]
    MockFailureNotCalled (CallWithArgs args) -> ["expected call with arguments: " ++ show args, "but was not called"]

instance Exception MockFailure

makeMock :: MonadIO m => String -> MockValue -> m Mock
makeMock key mock = do
  callRecord <- liftIO $ newIORef []
  pure $ Mock key callRecord mock

mockClass :: forall func args r m x. (MonadIO m, Typeable func, Typeable r, MockableFunction func args r, r ~ m x) => func -> MockValue
mockClass f = MockValueClass $ \calls -> recordArgs calls f

useMockClass :: forall f. Typeable f => MockConfig -> String -> Maybe f
useMockClass conf key = do
  let tRep = typeRep (Proxy :: Proxy f)
  case lookupMock conf key tRep of
    Just (Mock _ calls (MockValueClass mock)) -> case cast (mock calls) of
      Just val -> Just val
      -- This should never happen on successful lookup
      Nothing -> error $ "useMockClass: cast failed for " <> key
    _ -> Nothing

type family FunctionArgs f :: [Type] where
  FunctionArgs (a -> f) = a:FunctionArgs f
  FunctionArgs x = '[]

type family FunctionResult f :: Type where
  FunctionResult (a -> f) = FunctionResult f
  FunctionResult x = x

type family ConstructFunction (args :: [Type]) (r :: Type) where
  ConstructFunction '[] r = r
  ConstructFunction (a:as) r = a -> ConstructFunction as r

class (args ~ FunctionArgs f, r ~ FunctionResult f, ConstructFunction args r ~ f) 
  => MockableFunction f args r | args r -> f, f args -> r where
  transformFunction :: (args ~ FunctionArgs f', r' ~ FunctionResult f', ConstructFunction args r' ~ f')
    => (forall a. Typeable a => acc -> a -> acc) -> (acc -> r -> r') -> acc -> f -> f'

instance ('[] ~ FunctionArgs r, r ~ FunctionResult r)
  => MockableFunction r '[] r where
  transformFunction _ fr acc r = fr acc r

instance (Typeable a, MockableFunction f args r)
  => MockableFunction (a -> f) (a:args) r where
  transformFunction fa fr acc f = \a -> transformFunction fa fr (fa acc a) (f a)

recordArgs :: (MonadIO m, MockableFunction f args (m a)) =>
  IORef [CallRecord] -> f -> f
recordArgs callsRef = transformFunction fa fr [] where
  fa args a = Arg a:args
  fr args r = do
    liftIO $ modifyIORef callsRef (CallRecord (reverse args) :)
    r

resetCallRecords :: Mock -> IO ()
resetCallRecords (Mock _ callsRef _) = writeIORef callsRef []

lookupMock :: MockConfig -> String -> TypeRep -> Maybe Mock
lookupMock MockConfig{..} key tRep = case Map.lookup key mockConfigStorage of
  Nothing | mockConfigFailOnLookup -> error $ "TypeableMock: Mock not found: " <> key
  Nothing -> Nothing
  Just tMap -> case Map.lookup tRep tMap of
    Nothing | mockConfigFailOnLookup -> error $ "TypeableMock: Mock not found: " <> key
    result -> result

addMocksToConfig :: MockConfig -> [Mock] -> MockConfig
addMocksToConfig conf mocks = conf {mockConfigStorage = mockMap}
  where
    mockMap = foldr insertMock (mockConfigStorage conf) mocks
    insertMock mock@(Mock key _ _) = Map.insertWith (<>) key (toTypeMap mock)
    toTypeMap :: Mock -> Map TypeRep Mock
    toTypeMap mock@(Mock _ _ (MockValueClass (_ :: WithCallRecord f))) =
      Map.singleton (typeRep (Proxy :: Proxy f)) mock

data ExpectedArg = forall a. (Typeable a, Show a, Eq a) => ExpectedArg a

newtype CallWithArgs = CallWithArgs {unCallWithArgs :: [ExpectedArg]}
  deriving stock (Show)

instance Show ExpectedArg where
  show (ExpectedArg a) = show a

class CalledWith res where
  calledWith :: [ExpectedArg] -> res

instance CalledWith CallWithArgs where
  calledWith = CallWithArgs . reverse

instance (Typeable a, Show a, Eq a, CalledWith res) => CalledWith (a -> res) where
  calledWith args arg = calledWith (ExpectedArg arg : args)

call :: CalledWith res => res
call = calledWith []

checkCallRecord :: CallRecord -> CallWithArgs -> IO ()
checkCallRecord (CallRecord actualArgs) (CallWithArgs expectedArgs) = do
  when (length expectedArgs /= length actualArgs) $ fail $ "Expected " <> show (length expectedArgs) <> " arguments, called with " <> show (length actualArgs)
  forM_ (zip expectedArgs actualArgs) $ \(ExpectedArg expected, Arg actual) -> do
    unless (typeOf expected == typeOf ()) $ case cast actual of
      Nothing -> throwIO $ MockFailure location $ MockFailureArgumentTypeMismatch (ExpectedArg expected) (Arg actual)
      Just actual' -> unless (expected == actual') $ throwIO $ MockFailure location $ MockFailureArgumentValueMismatch expected actual'

assertHasCalls :: HasCallStack => Mock -> [CallWithArgs] -> IO ()
assertHasCalls (Mock _ callsRef _) expectedCalls = do
  actualCalls <- reverse <$> readIORef callsRef
  go actualCalls expectedCalls
  where
    go (a : as) (e : es) = checkCallRecord a e >> go as es
    go [] [] = pure ()
    go [] (e : _) = throwIO $ MockFailure location (MockFailureNotCalled e)
    go (a : _) [] = throwIO $ MockFailure location (MockFailureUnexpectedCall a)

assertNotCalled :: Mock -> IO ()
assertNotCalled (Mock _ callsRef _) = do
  actualCalls <- reverse <$> readIORef callsRef
  case actualCalls of
    [] -> pure ()
    (actualCall : _) -> throwIO $ MockFailure location (MockFailureUnexpectedCall actualCall)

location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing

-- | Enables polymorphic mock functions.
-- The newtype is equivalent to `MockConfig (forall m . MonadIO m => m)` which requires impredicative polymorphism.
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

fromMockMonadIO0 :: MockMonadIO x -> (forall m. MonadIO m => m x)
fromMockMonadIO0 = unMockMonadIO

fromMockMonadIO1 :: (a -> MockMonadIO x) -> (forall m. a -> MonadIO m => m x)
fromMockMonadIO1 f = unMockMonadIO . f

fromMockMonadIO :: forall m x f f' args . 
  (MonadIO m, MockableFunction f args (MockMonadIO x), MockableFunction f' args (m x))
  => f -> f'
fromMockMonadIO = transformFunction const (const unMockMonadIO) ()