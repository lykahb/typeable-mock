{-# LANGUAGE InstanceSigs, UndecidableInstances, KindSignatures, DataKinds, TypeFamilies #-}
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
    mockPolyClass,
    useMockClass,
    useMockPolyClass
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
import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, cast, eqT, mkFunTy, typeOf, typeRep, (:~:) (Refl))
import Prelude
import Unsafe.Coerce ( unsafeCoerce )

newtype CallRecord = CallRecord [Arg]

data Arg = forall a. Typeable a => Arg a

data Mock m = Mock String (IORef [CallRecord]) (MockValue m)

instance Show (Mock m) where
  show (Mock key _ _) = "Mock " <> key

type WithCallRecord a = IORef [CallRecord] -> a

-- see https://gitlab.haskell.org/ghc/ghc/-/issues/13655
-- It would be nice to have an abstraction of a type that has a free variable m
data MockValue m
  = forall func x. (Function func m x, Typeable x) => MockValuePolyClass (WithCallRecord func)
  | forall x. (Typeable x) => MockValueClass (WithCallRecord x)

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
mockValueTypeRep (MockValueClass (_ :: WithCallRecord f)) = typeRep (Proxy :: Proxy f)

makeMock :: (MonadIO m) => String -> MockValue mc -> m (Mock mc)
makeMock key mock = do
  callRecord <- liftIO $ newIORef []
  pure $ Mock key callRecord mock

-- TODO: turn mockMX into a typeclass
mockPolyClass :: forall m x func . (MonadIO m, Typeable x, Function func m x) => func -> MockValue m
mockPolyClass f = MockValuePolyClass $ \calls -> run (undefined :: proxy (m x)) calls [] f

mockClass :: forall m x func . (Typeable func, Typeable x, Function func m x) => func -> MockValue m
mockClass f = MockValueClass $ \calls -> run (undefined :: proxy (m x)) calls [] f


addCallRecord :: MonadIO m => IORef [CallRecord] -> [Arg] -> m ()
addCallRecord callsRef args = liftIO $ modifyIORef callsRef (CallRecord args :)

resetCallRecords :: Mock mc -> IO ()
resetCallRecords (Mock _ callsRef _) = writeIORef callsRef []

lookupMock :: MockConfig mc -> String -> TypeRep -> Maybe (Mock mc)
lookupMock mockConf key tRep = do
  tMap <- Map.lookup key $ mockConfigStorage mockConf
  Map.lookup tRep tMap

class MonadIO m => Function func m x | func -> m, func -> x where
  run :: proxy (m x) -> IORef [CallRecord] -> [Arg] -> func -> func
  -- A function that has polymorphic types is not Typeable. This is a hack to get its TypeRep
  -- funcTypeRep :: TypeRep -> proxy func -> TypeRep

instance {-# OVERLAPS #-} (MonadIO m, func ~ m x) => Function func m x where
  run _ callsRef args result = do
    liftIO $ addCallRecord callsRef (reverse args)
    result
  -- funcTypeRep resultTypeRep _ = resultTypeRep 

instance  {-# INCOHERENT  #-}  (MonadIO m, Typeable a, Function func m x) => Function (a -> func) m x where
  run p callsRef args f = \a -> run p callsRef (Arg a:args) (f a)
  -- funcTypeRep :: forall proxy . TypeRep -> proxy (a -> func) -> TypeRep
  -- funcTypeRep resultTypeRep _ = mkFunTy argType funcType where
  --   argType = typeRep (Proxy :: Proxy a)
  --   funcType = funcTypeRep resultTypeRep (Proxy :: Proxy func)


castPolyFunction :: forall m f1 x1 f2 x2 . (Typeable x1, Typeable x2, Function f1 m x1, Function f2 m x2)
             => f1 -> Maybe f2
castPolyFunction f = Just (unsafeCoerce f)
 -- if type1 == type2
 --  then Just (unsafeCoerce f)
 --  else Nothing
 --  where
 --  type1 = funcTypeRep (typeRep (Proxy :: Proxy x1)) (Proxy :: Proxy f1)
 --  type2 = funcTypeRep (typeRep (Proxy :: Proxy x2)) (Proxy :: Proxy f2)

useMockPolyClass :: forall m x func . (Typeable x, Function func m x) => MockConfig m -> String -> Maybe func
useMockPolyClass conf key = do
  let tRep = typeRep (Proxy :: Proxy Int) --(Proxy :: Proxy func)
  case lookupMock conf key tRep of
    Just (Mock _ calls (MockValuePolyClass mock)) -> case castPolyFunction (mock calls) of
      Just val -> Just val
      Nothing -> error $ "useMockPolyClass: cast failed for " <> key
    Just _ -> error $ "useMockPolyClass: expected MockValuePolyClass for " <> key
    _ -> Nothing

useMockClass :: forall m x func . (Typeable func, Typeable x, Function func m x) => MockConfig m -> String -> Maybe func
useMockClass conf key = do
  let tRep = typeRep (Proxy :: Proxy func)

  case lookupMock conf key tRep of
    Just (Mock _ calls (MockValueClass mock)) -> case cast (mock calls) of
      Just val -> Just val
      Nothing -> error $ "useMockClass: cast failed for " <> key
    Just _ -> error $ "useMockClass: expected MockValueClass for " <> key
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
