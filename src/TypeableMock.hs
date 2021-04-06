{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module TypeableMock
  ( Mock (..),
    MockConfig (..),
    CallRecord (..),
    ExpectedArg (..),
    ExpectedArgs (..),
    MockValue (..),
    makeMock,
    mocksToConfig,
    checkCalls,
    mockM0,
    mockM1,
    mockM2,
    mockM3,
    useMockPure,
    useMockM0,
    useMockM1,
    useMockM2,
    useMockM3,
  )
where

import Control.Monad (MonadFail (fail), unless, when)
import Control.Monad.IO.Class
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import Data.Typeable
import Data.IORef

newtype CallRecord = CallRecord [Arg]

-- deriving Show
-- deriving (Eq, Show)

data Arg = forall a. Typeable a => Arg a

-- class CombineConstraints a b

-- deriving instance Eq Arg
-- deriving instance Show Arg

data MockMonadTR a

data Mock m = Mock String (IORef [CallRecord]) (MockValue m)

type WithCallRecord a = IORef [CallRecord] -> a

data MockValue m
  = forall a. Typeable a => PureMock a
  | forall a. Typeable a => RecordingMock (WithCallRecord a)
  | -- https://gitlab.haskell.org/ghc/ghc/-/issues/13655
    forall x. Typeable x => MonadicMock0 (WithCallRecord (m x))
  | forall x a. (Typeable x, Typeable a) => MonadicMock1 (WithCallRecord (a -> m x))
  | forall x a b. (Typeable x, Typeable a, Typeable b) => MonadicMock2 (WithCallRecord (a -> b -> m x))
  | forall x a b c. (Typeable x, Typeable a, Typeable b, Typeable c) => MonadicMock3 (WithCallRecord (a -> b -> c -> m x))

newtype MockConfig m = MockConfig (Map String (Map TypeRep (Mock m)))
  deriving (Monoid, Semigroup)

-- makeMockPure :: (MonadIO m, Coercible mock orig, Typeable orig, Typeable mock) => orig -> mock -> m Mock
-- makeMockPure orig mock = do
--   callRecord <- liftIO $ newIORef []
--   pure $ Mock callRecord (PureMock mock)

-- | This allows us get a TypeRep even though the monad in MonadicMockX does not have `Typeable m`
mockValueTypeRep :: MockValue m -> TypeRep
mockValueTypeRep (PureMock a) = typeOf a
mockValueTypeRep (RecordingMock (_ :: WithCallRecord a)) = typeRep (Proxy :: Proxy a)
mockValueTypeRep (MonadicMock0 (_ :: WithCallRecord (m x))) = typeRep (Proxy :: Proxy (MockMonadTR x))
mockValueTypeRep (MonadicMock1 (_ :: WithCallRecord (a -> m x))) = typeRep (Proxy :: Proxy (a -> MockMonadTR x))
mockValueTypeRep (MonadicMock2 (_ :: WithCallRecord (a -> b -> m x))) = typeRep (Proxy :: Proxy (a -> b -> MockMonadTR x))
mockValueTypeRep (MonadicMock3 (_ :: WithCallRecord (a -> b -> c -> m x))) = typeRep (Proxy :: Proxy (a -> b -> c -> MockMonadTR x))

makeMock :: (MonadIO m) => String -> MockValue mc -> m (Mock mc)
makeMock key mock = do
  callRecord <- liftIO $ newIORef []
  pure $ Mock key callRecord mock

-- TODO: turn mockMX into a typeclass
mockM0 :: (MonadIO m, Typeable x) => m x -> MockValue m
mockM0 f = MonadicMock0 $ \calls -> addCallRecord calls [] >> f

mockM1 :: (MonadIO m, Typeable x, Typeable a) => (a -> m x) -> MockValue m
mockM1 f = MonadicMock1 $ \calls a -> addCallRecord calls [Arg a] >> f a

mockM2 :: (MonadIO m, Typeable x, Typeable a, Typeable b) => (a -> b -> m x) -> MockValue m
mockM2 f = MonadicMock2 $ \calls a b -> addCallRecord calls [Arg a, Arg b] >> f a b

mockM3 :: (MonadIO m, Typeable x, Typeable a, Typeable b, Typeable c) => (a -> b -> c -> m x) -> MockValue m
mockM3 f = MonadicMock3 $ \calls a b c -> addCallRecord calls [Arg a, Arg b, Arg c] >> f a b c

addCallRecord :: MonadIO m => IORef [CallRecord] -> [Arg] -> m ()
addCallRecord callRecords args = liftIO (modifyIORef callRecords (CallRecord args :))

lookupMock :: MockConfig mc -> String -> TypeRep -> Maybe (Mock mc)
lookupMock (MockConfig mocks) key tRep = do
  tMap <- Map.lookup key mocks
  Map.lookup tRep tMap

-- castMock :: Typeable a => Mock monadConstraint -> Maybe a
-- castMock mock = case mock of
--   Mock _ calls (RecordingMock mkVal) -> cast (mkVal calls)
--   Mock _ calls (MonadicMock0 mkVal) -> gcast (mkVal calls)
--   Mock _ calls (MonadicMock1 mkVal) -> cast (mkVal calls)
--   Mock _ _ (PureMock _) -> error "castMock: PureMock"

useMockPure :: forall m a. (Typeable a) => MockConfig m -> String -> Maybe a
useMockPure conf key = do
  let tRep = typeRep (Proxy :: Proxy a)
  case lookupMock conf key tRep of
    Just (Mock _ calls val) -> case val of
      PureMock a -> cast a
      RecordingMock a -> cast (a calls)
      _ -> error $ "useMockPure: cannot be used with MonadicMock " <> key
    _ -> Nothing

-- we want to put into mock those and get them back without the need for `Typeable m`.
-- m x
-- a -> m x
-- a -> b -> m x
useMockM0 :: forall m x. (Typeable x) => MockConfig m -> String -> Maybe (m x)
useMockM0 conf key = do
  let tRep = typeRep (Proxy :: Proxy (MockMonadTR x))
  case lookupMock conf key tRep of
    Just (Mock _ calls (MonadicMock0 mock)) -> case gcast (mock calls) of
      Just val -> Just val
      Nothing -> error $ "useMockM0: cast failed for " <> key
    Just _ -> error $ "useMockM0: expected MonadicMock0 for " <> key
    _ -> Nothing

useMockM1 :: forall m x a. (Typeable x, Typeable a) => MockConfig m -> String -> Maybe (a -> m x)
useMockM1 conf key = do
  let tRep = typeRep (Proxy :: Proxy (a -> MockMonadTR x))
  let myCast :: forall a' a'' x' x''. (Typeable a', Typeable a'', Typeable x', Typeable x'') => (a' -> m x') -> Maybe (a'' -> m x'')
      myCast x = fmap (\Refl -> x) (eqT :: Maybe ((a', x') :~: (a'', x'')))
  case lookupMock conf key tRep of
    Just (Mock _ calls (MonadicMock1 mock)) -> case myCast (mock calls) of
      Just val -> Just val
      Nothing -> error $ "useMockM1: cast failed for " <> key
    Just _ -> error $ "useMockM1: expected MonadicMock1 for " <> key
    _ -> Nothing

useMockM2 :: forall m x a b. (Typeable x, Typeable a, Typeable b) => MockConfig m -> String -> Maybe (a -> b -> m x)
useMockM2 conf key = do
  let tRep = typeRep (Proxy :: Proxy (a -> b -> MockMonadTR x))
  let myCast :: forall a' a'' b' b'' x' x''. (Typeable a', Typeable a'', Typeable b', Typeable b'', Typeable x', Typeable x'') => (a' -> b' -> m x') -> Maybe (a'' -> b'' -> m x'')
      myCast x = fmap (\Refl -> x) (eqT :: Maybe ((a', b', x') :~: (a'', b'', x'')))
  case lookupMock conf key tRep of
    Just (Mock _ calls (MonadicMock2 mock)) -> case myCast (mock calls) of
      Just val -> Just val
      Nothing -> error $ "useMockM2: cast failed for " <> key
    Just _ -> error $ "useMockM2: expected MonadicMock2 for " <> key
    _ -> Nothing

useMockM3 :: forall m x a b c. (Typeable x, Typeable a, Typeable b, Typeable c) => MockConfig m -> String -> Maybe (a -> b -> c -> m x)
useMockM3 conf key = do
  let tRep = typeRep (Proxy :: Proxy (a -> b -> c -> MockMonadTR x))
  let myCast :: forall a' a'' b' b'' c' c'' x' x''. (Typeable a', Typeable a'', Typeable b', Typeable b'', Typeable c', Typeable c'', Typeable x', Typeable x'') => (a' -> b' -> c' -> m x') -> Maybe (a'' -> b'' -> c'' -> m x'')
      myCast x = fmap (\Refl -> x) (eqT :: Maybe ((a', b', c', x') :~: (a'', b'', c'', x'')))
  case lookupMock conf key tRep of
    Just (Mock _ calls (MonadicMock3 mock)) -> case myCast (mock calls) of
      Just val -> Just val
      Nothing -> error $ "useMockM3: cast failed for " <> key
    Just _ -> error $ "useMockM3: expected MonadicMock3 for " <> key
    _ -> Nothing

mocksToConfig :: [Mock mc] -> MockConfig mc
mocksToConfig mocks = MockConfig mockMap
  where
    mockMap = foldr (\mock@(Mock key _ _) keyMap -> Map.insertWith (<>) key (toTypeMap mock) keyMap) mempty mocks
    toTypeMap :: Mock mc -> Map TypeRep (Mock mc)
    toTypeMap mock@(Mock _ _ val) = Map.singleton (mockValueTypeRep val) mock

data ExpectedArg = forall a. (Typeable a, Show a, Eq a) => ExpectedArg a

instance Show ExpectedArg where
  show (ExpectedArg a) = show a

class Show a => ExpectedArgs a where
  toDynArgs :: a -> [ExpectedArg]

instance ExpectedArgs ExpectedArg where
  toDynArgs arg = [arg]

instance (Typeable a, Show a, Eq a, Typeable b, Show b, Eq b) => ExpectedArgs (a, b) where
  toDynArgs (a, b) = [ExpectedArg a, ExpectedArg b]

instance (Typeable a, Show a, Eq a, Typeable b, Show b, Eq b, Typeable c, Show c, Eq c) => ExpectedArgs (a, b, c) where
  toDynArgs (a, b, c) = [ExpectedArg a, ExpectedArg b, ExpectedArg c]

checkCallRecord :: (MonadFail m, ExpectedArgs args) => CallRecord -> args -> m ()
checkCallRecord (CallRecord actual) expected = do
  result <- combineArgs actual (toDynArgs expected)
  let toError idx argComparison = case argComparison of
        Left (_, t2) | t2 == typeOf () -> Nothing -- () means that we ignore that argument
        Left (t1, t2) -> Just $ "Types of argument " <> show idx <> " do not match: " <> show t1 <> " and " <> show t2
        Right (False, val1, val2) -> Just $ "Argument " <> show idx <> " does not match: expected " <> show val2 <> " actual " <> show val1
        _ -> Nothing
  let errors = catMaybes $ zipWith toError [0 :: Int ..] result
  unless (null errors) $ fail (intercalate "\n" errors)

combineArgs :: MonadFail m => [Arg] -> [ExpectedArg] -> m [Either (TypeRep, TypeRep) (Bool, String, String)]
combineArgs actual expected = do
  when (length expected /= length actual) $ fail $ "Expected " <> show (length expected) <> " arguments, called with " <> show (length actual)
  let compareArgs (Arg actArg) (ExpectedArg expArg) = case cast actArg of
        Nothing -> Left (typeOf actArg, typeOf expArg)
        Just actArg' -> Right (actArg' == expArg, show actArg', show expArg)
  pure $ zipWith compareArgs actual expected

checkCalls :: (MonadFail m, MonadIO m, ExpectedArgs args) => Mock mc -> [args] -> m ()
checkCalls (Mock key actualRef _) expectedCalls = do
  actualCalls <- reverse <$> liftIO (readIORef actualRef)
  go actualCalls expectedCalls
  where
    showCombination (Left (t, _)) = show t
    showCombination (Right (_, val, _)) = val
    go (a : as) (e : es) = checkCallRecord a e >> go as es
    go [] [] = pure ()
    go [] es = fail $ "Mock " <> key <> " did not perform expected calls: " <> show es
    go as [] = case expectedCalls of
      (expected:_) -> do
        as' <- mapM (\(CallRecord args) -> combineArgs args (toDynArgs expected)) as
        let showable = fmap (fmap showCombination) as'
        fail $ "Mock " <> key <> " has unexpected calls: " <> show showable
      [] -> fail $ "Mock " <> key <> " has unexpected calls: " <> show (length as) <> " calls"

