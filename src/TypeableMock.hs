{-# LANGUAGE InstanceSigs, UndecidableInstances, FlexibleContexts, DataKinds, ScopedTypeVariables, TypeFamilies #-}
module TypeableMock
  ( Mock (..),
    MockConfig (..),
    MockMonadIO(..),
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
    mockPolyClass,
    useMockClass,
    useMockPolyClass,
    fromMockMonadIO
    -- useMockClassMonadIO
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
import Debug.Trace
-- import Unsafe.Coerce ( unsafeCoerce )

newtype CallRecord = CallRecord [Arg]

data Arg = forall a. Typeable a => Arg a

data Mock = Mock String (IORef [CallRecord]) MockValue

instance Show Mock where
  show (Mock key _ _) = "Mock " <> key

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

type WithCallRecord a = IORef [CallRecord] -> a

-- see https://gitlab.haskell.org/ghc/ghc/-/issues/13655
-- It would be nice to have an abstraction of a type that has a free variable m
data MockValue
  = forall func r. (Function func r, Typeable r, Typeable func) => MockValuePolyClass (WithCallRecord func)
  | forall x. (Typeable x) => MockValueClass (WithCallRecord x)

data MockConfig = MockConfig {
  mockConfigStorage :: Map String (Map TypeRep Mock),
  mockConfigFailOnLookup :: Bool
  } deriving stock (Show)

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

-- | This allows us get a TypeRep even though the monad in MockValuePolyClass does not have `Typeable m`
mockValueTypeRep :: MockValue -> TypeRep
mockValueTypeRep (MockValuePolyClass (_ :: WithCallRecord f)) = typeRep (Proxy :: Proxy f)
mockValueTypeRep (MockValueClass (_ :: WithCallRecord f)) = typeRep (Proxy :: Proxy f)

makeMock :: MonadIO m => String -> MockValue -> m Mock
makeMock key mock = do
  callRecord <- liftIO $ newIORef []
  pure $ Mock key callRecord mock

mockPolyClass :: forall m x func . (MonadIO m, Typeable m, Typeable func, Typeable x, Function func (m x)) => func -> MockValue
mockPolyClass f = MockValuePolyClass $ \calls -> run (undefined :: proxy (m x)) calls [] f

fromMockMonadIO :: (MonadIO m, Function func (MockMonadIO x), Function func' (m x)) => func -> func'
fromMockMonadIO = convertFunc unMockMonadIO

-- mockPolyClassMonadIO :: forall x . (forall m func . (Typeable x, Function func (m x)) => func) -> MockValue
-- mockPolyClassMonadIO _ = error "mockPolyClassMonadIO"
-- mockPolyClassMonadIO f = MockValuePolyClass $ f'' where
--   f'' = \calls -> convertFunc MockMonadIO (f' calls)
--   f' calls = run ((undefined :: (Function func (m x), Typeable x) => func -> proxy (m x)) f ) calls [] f


mockClass :: forall func r m x . (MonadIO m, Typeable func, Typeable r, Function func r, r ~ m x) => func -> MockValue
mockClass f = MockValueClass $ \calls -> run (undefined :: proxy r) calls [] f

useMockPolyClass :: forall func r . (Typeable r, Typeable func, Function func r) => MockConfig -> String -> Maybe func
useMockPolyClass conf key = do
  let tRep = typeRep (Proxy :: Proxy func)
  case lookupMock conf key tRep of
    Just (Mock _ calls (MockValuePolyClass mock)) -> case cast (mock calls) of
      Just val -> Just val
      Nothing -> error $ "useMockPolyClass: cast failed for " <> key
    Just _ -> error $ "useMockPolyClass: expected MockValuePolyClass for " <> key
    _ -> Nothing

useMockClass :: forall func . Typeable func => MockConfig -> String -> Maybe func
useMockClass conf key = do
  let tRep = typeRep (Proxy :: Proxy func)
  case lookupMock conf key tRep of
    Just (Mock _ calls (MockValueClass mock)) -> case cast (mock calls) of
      Just val -> Just val
      Nothing -> error $ "useMockClass: cast failed for " <> key
    Just _ -> error $ "useMockClass: expected MockValueClass for " <> key
    _ -> Nothing


class Function func r | func -> r where
  run :: (MonadIO m, r ~ m x) => proxy r -> IORef [CallRecord] -> [Arg] -> func -> func
  convertFunc :: Function func' r' => (r -> r') -> func -> func'

instance {-# OVERLAPS #-} (MonadIO m, func ~ r, r ~ m x) => Function func r where
  run _ callsRef args result = do
    liftIO $ addCallRecord callsRef (reverse args)
    result
  convertFunc f r = f r

instance  {-# INCOHERENT #-}  (Typeable a, Function func r) => Function (a -> func) r where
  run p callsRef args f = \a -> run p callsRef (Arg a:args) (f a)
  convertFunc f func = \a -> convertFunc f (func a)


-- useMockClassMonadIO :: forall func func' m x . (Typeable func, Function func MockMonadIO x, Function func' m x) => MockConfigockMonadIO -> String -> Maybe func'
-- useMockClassMonadIO = error "useMockClassMonadIO"

-- useMockClassMonadIO :: forall func func' m x . (Typeable func, Function func MockMonadIO x, Function func' m x) => MockConfig -> String -> Maybe func'

-- lookupMockMonadIO = useMockClass

-- useMockClassMonadIO :: forall func func' x . (Function func' MockMonadIO x) => MockConfig -> String -> Maybe func'
-- -- useMockClassMonadIO = error "useMockClassMonadIO"
-- useMockClassMonadIO conf key = convertFunc unMockMonadIO <$> func where
--   -- func :: Maybe func
--   func = lookupMockMonadIO conf key
-- useMockClassMonadIO :: forall func m x. (Typeable x, Function func m x) => MockConfig -> String -> Maybe func
-- useMockClassMonadIO conf key = do
--   let tRep = typeRep (Proxy :: Proxy Int) -- TODO: use func with the MockMonadIO
--   case lookupMock conf key tRep of
--     Just (Mock _ calls (MockValuePolyClass mock)) -> case cast (mock calls) of
--       Just val -> Just $ convertFunc unMockMonadIO val
--       Nothing -> error $ "useMockClass: cast failed for " <> key
--     Just _ -> error $ "useMockClass: expected MockValueClass for " <> key
--     _ -> Nothing

addCallRecord :: MonadIO m => IORef [CallRecord] -> [Arg] -> m ()
addCallRecord callsRef args = liftIO $ modifyIORef callsRef (CallRecord args :)

resetCallRecords :: Mock -> IO ()
resetCallRecords (Mock _ callsRef _) = writeIORef callsRef []

lookupMock :: MockConfig -> String -> TypeRep -> Maybe Mock
lookupMock mockConf key tRep = do
  tMap <- Map.lookup key $ mockConfigStorage mockConf
  traceShow (tRep, tMap) $ Map.lookup tRep tMap

addMocksToConfig :: MockConfig -> [Mock] -> MockConfig
addMocksToConfig conf mocks = conf{mockConfigStorage=mockMap}
  where
    mockMap = foldr insertMock (mockConfigStorage conf) mocks
    insertMock mock@(Mock key _ _) = Map.insertWith (<>) key (toTypeMap mock)
    toTypeMap :: Mock -> Map TypeRep Mock
    toTypeMap mock@(Mock _ _ val) = Map.singleton (mockValueTypeRep val) mock

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
