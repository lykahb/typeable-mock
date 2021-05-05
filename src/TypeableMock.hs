{-# LANGUAGE UndecidableInstances #-}
module TypeableMock
  ( Mock (..),
    MockConfig (..),
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
    mockM0,
    mockM1,
    mockM2,
    mockM3,
    useMockPolyClass,
    useMockM0,
    useMockM1,
    useMockM2,
    useMockM3,
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
import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, cast, eqT, typeOf, typeRep, (:~:) (Refl))
import Prelude
import System.IO.Unsafe (unsafePerformIO)

newtype CallRecord = CallRecord [Arg]

data Arg = forall a. Typeable a => Arg a

data MockMonadTR a

data Mock m = Mock String (IORef [CallRecord]) (MockValue m)

instance Show (Mock m) where
  show (Mock key _ _) = "Mock " <> key

type WithCallRecord a = IORef [CallRecord] -> a

-- see https://gitlab.haskell.org/ghc/ghc/-/issues/13655
-- It would be nice to have an abstraction of a type that has a free variable m
data MockValue m
  = forall x. Typeable x => MockValue0 (WithCallRecord x)
  | forall x a. (Typeable x, Typeable a) => MockValue1 (WithCallRecord (a -> x))
  | forall x a b. (Typeable x, Typeable a, Typeable b) => MockValue2 (WithCallRecord (a -> b -> x))
  | forall x a b c. (Typeable x, Typeable a, Typeable b, Typeable c) => MockValue3 (WithCallRecord (a -> b -> c -> x))
  -- These values have a free variable m that is not typeable.
  | forall func x. (Function func (m x), Typeable x) => MockValuePolyClass (WithCallRecord func)
  | forall x. Typeable x => MockValuePoly0 (WithCallRecord (m x))
  | forall x a. (Typeable x, Typeable a) => MockValuePoly1 (WithCallRecord (a -> m x))
  | forall x a b. (Typeable x, Typeable a, Typeable b) => MockValuePoly2 (WithCallRecord (a -> b -> m x))
  | forall x a b c. (Typeable x, Typeable a, Typeable b, Typeable c) => MockValuePoly3 (WithCallRecord (a -> b -> c -> m x))

data MockConfig m = MockConfig {
  mockConfigStorage :: Map String (Map TypeRep (Mock m)),
  mockConfigFailOnLookup :: Bool
  } deriving stock (Show)

defaultMockConfig :: MockConfig m
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

-- | This allows us get a TypeRep even though the monad in MockValuePolyX does not have `Typeable m`
mockValueTypeRep :: MockValue m -> TypeRep
mockValueTypeRep (MockValuePolyClass _) = typeRep (Proxy :: Proxy Int)
mockValueTypeRep (MockValue0 (_ :: WithCallRecord x)) = typeRep (Proxy :: Proxy x)
mockValueTypeRep (MockValue1 (_ :: WithCallRecord (a -> x))) = typeRep (Proxy :: Proxy (a -> x))
mockValueTypeRep (MockValue2 (_ :: WithCallRecord (a -> b -> x))) = typeRep (Proxy :: Proxy (a -> b -> x))
mockValueTypeRep (MockValue3 (_ :: WithCallRecord (a -> b -> c -> x))) = typeRep (Proxy :: Proxy (a -> b -> c -> x))

mockValueTypeRep (MockValuePoly0 (_ :: WithCallRecord (m x))) = typeRep (Proxy :: Proxy (MockMonadTR x))
mockValueTypeRep (MockValuePoly1 (_ :: WithCallRecord (a -> m x))) = typeRep (Proxy :: Proxy (a -> MockMonadTR x))
mockValueTypeRep (MockValuePoly2 (_ :: WithCallRecord (a -> b -> m x))) = typeRep (Proxy :: Proxy (a -> b -> MockMonadTR x))
mockValueTypeRep (MockValuePoly3 (_ :: WithCallRecord (a -> b -> c -> m x))) = typeRep (Proxy :: Proxy (a -> b -> c -> MockMonadTR x))

makeMock :: (MonadIO m) => String -> MockValue mc -> m (Mock mc)
makeMock key mock = do
  callRecord <- liftIO $ newIORef []
  pure $ Mock key callRecord mock

-- TODO: turn mockMX into a typeclass
mockClass :: forall m x func . (MonadIO m, Typeable x, Function func (m x)) => func -> MockValue m
mockClass f = MockValuePolyClass $ \calls -> run (undefined :: proxy (m x)) calls [] f

mockM0 :: (MonadIO m, Typeable x) => m x -> MockValue m
mockM0 f = MockValuePoly0 $ \calls -> addCallRecord calls [] >> f

mockM1 :: (MonadIO m, Typeable x, Typeable a) => (a -> m x) -> MockValue m
mockM1 f = MockValuePoly1 $ \calls a -> addCallRecord calls [Arg a] >> f a

mockM2 :: (MonadIO m, Typeable x, Typeable a, Typeable b) => (a -> b -> m x) -> MockValue m
mockM2 f = MockValuePoly2 $ \calls a b -> addCallRecord calls [Arg a, Arg b] >> f a b

mockM3 :: (MonadIO m, Typeable x, Typeable a, Typeable b, Typeable c) => (a -> b -> c -> m x) -> MockValue m
mockM3 f = MockValuePoly3 $ \calls a b c -> addCallRecord calls [Arg a, Arg b, Arg c] >> f a b c

addCallRecord :: MonadIO m => IORef [CallRecord] -> [Arg] -> m ()
addCallRecord callsRef args = liftIO $ modifyIORef callsRef (CallRecord args :)

resetCallRecords :: Mock mc -> IO ()
resetCallRecords (Mock _ callsRef _) = writeIORef callsRef []

lookupMock :: MockConfig mc -> String -> TypeRep -> Maybe (Mock mc)
lookupMock mockConf key tRep = do
  tMap <- Map.lookup key $ mockConfigStorage mockConf
  Map.lookup tRep tMap


-- we want to put into mock those and get them back without the need for `Typeable m`.
-- m x
-- a -> m x
-- a -> b -> m x

class Function func result | func -> result where
  run :: proxy result -> IORef [CallRecord] -> [Arg] -> func -> func

instance Function result result where
  run _ callsRef args result = unsafePerformIO (addCallRecord callsRef args) `seq` result

instance (Typeable a, Function func result) => Function (a -> func) result where
  run p callsRef args f = \a -> run p callsRef (Arg a:args) (f a)


castFunction :: (Function func1 result1, Function func2 result2) => func1 -> Maybe func2
castFunction = error "castFunction"

useMockPolyClass :: forall m x func . (Typeable x, Function func (m x)) => MockConfig m -> String -> Maybe func
useMockPolyClass conf key = do
  let tRep = typeRep (Proxy :: Proxy Int)  -- TODO: get real TypeRep
  case lookupMock conf key tRep of
    Just (Mock _ calls (MockValuePolyClass mock)) -> case castFunction (mock calls) of
      Just val -> Just val
      Nothing -> error $ "useMockPolyClass: cast failed for " <> key
    Just _ -> error $ "useMockPolyClass: expected MockValuePolyClass for " <> key
    _ -> Nothing

castPoly0 :: forall m x' x''. (Typeable x', Typeable x'') => (m x') -> Maybe (m x'')
castPoly0 x = fmap (\Refl -> x) (eqT :: Maybe (x' :~: x''))

useMockM0 :: forall m x out. (Typeable x, out ~ m x) => MockConfig m -> String -> Maybe out
useMockM0 conf key = do
  let tRep = typeRep (Proxy :: Proxy (MockMonadTR x))
  case lookupMock conf key tRep of
    Just (Mock _ calls (MockValuePoly0 mock)) -> case castPoly0 (mock calls) of
      Just val -> Just val
      Nothing -> error $ "useMockM0: cast failed for " <> key
    Just _ -> error $ "useMockM0: expected MockValuePoly0 for " <> key
    _ -> Nothing

castPoly1 :: forall m a' a'' x' x''. (Typeable a', Typeable a'', Typeable x', Typeable x'') => (a' -> m x') -> Maybe (a'' -> m x'')
castPoly1 x = fmap (\Refl -> x) (eqT :: Maybe ((a', x') :~: (a'', x'')))

useMockM1 :: forall m x a out. (Typeable x, Typeable a, out ~ (a -> m x)) => MockConfig m -> String -> Maybe out
useMockM1 conf key = do
  let tRep = typeRep (Proxy :: Proxy (a -> MockMonadTR x))
  case lookupMock conf key tRep of
    Just (Mock _ calls (MockValuePoly1 mock)) -> case castPoly1 (mock calls) of
      Just val -> Just val
      Nothing -> error $ "useMockM1: cast failed for " <> key
    Just _ -> error $ "useMockM1: expected MockValuePoly1 for " <> key
    _ -> Nothing

useMockM2 :: forall m x a b. (Typeable x, Typeable a, Typeable b) => MockConfig m -> String -> Maybe (a -> b -> m x)
useMockM2 conf key = do
  let tRep = typeRep (Proxy :: Proxy (a -> b -> MockMonadTR x))
  let myCast :: forall a' a'' b' b'' x' x''. (Typeable a', Typeable a'', Typeable b', Typeable b'', Typeable x', Typeable x'') => (a' -> b' -> m x') -> Maybe (a'' -> b'' -> m x'')
      myCast x = fmap (\Refl -> x) (eqT :: Maybe ((a', b', x') :~: (a'', b'', x'')))
  case lookupMock conf key tRep of
    Just (Mock _ calls (MockValuePoly2 mock)) -> case myCast (mock calls) of
      Just val -> Just val
      Nothing -> error $ "useMockM2: cast failed for " <> key
    Just _ -> error $ "useMockM2: expected MockValuePoly2 for " <> key
    _ -> Nothing

useMockM3 :: forall m x a b c. (Typeable x, Typeable a, Typeable b, Typeable c) => MockConfig m -> String -> Maybe (a -> b -> c -> m x)
useMockM3 conf key = do
  let tRep = typeRep (Proxy :: Proxy (a -> b -> c -> MockMonadTR x))
  let myCast :: forall a' a'' b' b'' c' c'' x' x''. (Typeable a', Typeable a'', Typeable b', Typeable b'', Typeable c', Typeable c'', Typeable x', Typeable x'') => (a' -> b' -> c' -> m x') -> Maybe (a'' -> b'' -> c'' -> m x'')
      myCast x = fmap (\Refl -> x) (eqT :: Maybe ((a', b', c', x') :~: (a'', b'', c'', x'')))
  case lookupMock conf key tRep of
    Just (Mock _ calls (MockValuePoly3 mock)) -> case myCast (mock calls) of
      Just val -> Just val
      Nothing -> error $ "useMockM3: cast failed for " <> key
    Just _ -> error $ "useMockM3: expected MockValuePoly3 for " <> key
    _ -> Nothing

addMocksToConfig :: MockConfig mc -> [Mock mc] -> MockConfig mc
addMocksToConfig conf mocks = conf{mockConfigStorage=mockMap}
  where
    mockMap = foldr insertMock (mockConfigStorage conf) mocks
    insertMock mock@(Mock key _ _) = Map.insertWith (<>) key (toTypeMap mock)
    toTypeMap :: Mock mc -> Map TypeRep (Mock mc)
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

assertHasCalls :: HasCallStack => Mock mc -> [CallWithArgs] -> IO ()
assertHasCalls (Mock _ callsRef _) expectedCalls = do
  actualCalls <- reverse <$> readIORef callsRef
  go actualCalls expectedCalls
  where
    go (a : as) (e : es) = checkCallRecord a e >> go as es
    go [] [] = pure ()
    go [] (e : _) = throwIO $ MockFailure location (MockFailureNotCalled e)
    go (a : _) [] = throwIO $ MockFailure location (MockFailureUnexpectedCall a)

assertNotCalled :: Mock mc -> IO ()
assertNotCalled (Mock _ callsRef _) = do
  actualCalls <- reverse <$> readIORef callsRef
  case actualCalls of
    [] -> pure ()
    (actualCall : _) -> throwIO $ MockFailure location (MockFailureUnexpectedCall actualCall)

location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing
