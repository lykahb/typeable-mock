module Test.TypeableMock
  ( -- * Mocks and mock configuration
    Mock (..),
    MockConfig (..),
    defaultMockConfig,
    addMocksToConfig,

    -- * Creating and calling mocks
    makeMock,
    lookupMockFunction,
    constN,

    -- * Checking calls
    ActualCallRecord (..),
    ActualVal (..),
    ExpectedCallRecord (..),
    ExpectedVal (..),
    MockFailure (..),
    MockFailureReason (..),
    lookupMock,
    lookupMockTyped,
    assertHasCalls,
    assertNotCalled,
    assertAnyCall,
    getCalls,
    expectCall,
    withResult,
    callMatches,
    resetMockCallRecords,
    resetAllCallRecords,

    -- * Mocking polymorphic monads
    -- $polymorphic
    MockMonadIO (..),
    fromMockMonadIO,
    unMockMonadIO1,
    unMockMonadIO2,
    unMockMonadIO3,
    unMockMonadIO4,
    unMockMonadIO5,
    -- $polymorphicAdvanced
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.CallStack (HasCallStack, SrcLoc (..), callStack)
import Data.Function.Variadic
import Data.Function.Variadic.Utils (composeN, constN)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (foldl', intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, cast, eqT, typeOf, typeRep, (:~:) (..))

data ActualCallRecord = ActualCallRecord [ActualVal] ActualVal

data ActualVal = forall a. Typeable a => ActualVal a

data Mock = forall x.
  Typeable x =>
  Mock
  { mockKey :: String,
    mockCallRecord :: IORef [ActualCallRecord],
    mockFunction :: IORef [ActualCallRecord] -> x
  }

instance Show Mock where
  show Mock {..} = "Mock (" <> mockKey <> " :: " <> show tRep <> ")"
    where
      tRep = typeRep mockFunction

-- | Mock configuration. When running production, use the `defaultMockConfig` without adding mocks to it -
-- it would call the real functions.
--
-- The key or type of the mock created in a test suite may accidentally mismatch the key or type at the place where a mock is used.
-- Silently calling the real functions would make the test suite fragile.
-- So, when running on a test suite, protect against the mismatches by requiring that the mocks are present.
-- Set `mcShouldFailOnNotFound` to return True or allow a few special cases:
--
-- > testMockConfig = defaultMockConfig {
-- >   mcShouldFailOnNotFound = \\key tRep -> key ``notElem`` whitelist where
-- >     -- Functions that are allowed to be called during tests.
-- >     whitelist = ["readFile"]
-- > }
data MockConfig = MockConfig
  { -- | A map of mocks. The key of the inner map is the @TypeRep@ of the function supplied when making a mock.
    mcStorage :: Map String (Map TypeRep Mock),
    -- | Decide whether to throw an error when a mock is not found.
    mcShouldFailOnNotFound :: String -> TypeRep -> Bool
  }

instance Show MockConfig where
  show MockConfig {..} = "MockConfig " <> show mcStorage

defaultMockConfig :: MockConfig
defaultMockConfig = MockConfig mempty (\_ _ -> False)

data MockFailure = MockFailure
  { mfMock :: Mock,
    mfLocation :: Maybe SrcLoc,
    mfReason :: MockFailureReason
  }

data MockFailureReason
  = MockFailureArgumentCountMismatch ActualCallRecord ExpectedCallRecord
  | MockFailureArgumentTypeMismatch TypeRep TypeRep
  | forall a. Show a => MockFailureArgumentValueMismatch a a
  | forall a. Show a => MockFailureArgumentPredicateFailure a
  | MockFailureUnexpectedCall ActualCallRecord
  | MockFailureNotCalled ExpectedCallRecord

instance Show MockFailureReason where
  show reason = intercalate "\n" $ case reason of
    MockFailureArgumentCountMismatch (ActualCallRecord actArgs _) (ExpectedCallRecord expArgs _) ->
      ["Number of arguments does not match:", "expected: " ++ show (length expArgs), "but got: " ++ show (length actArgs)]
    MockFailureArgumentTypeMismatch actual expected ->
      ["Value type does not match:", "expected: " ++ show expected, "but got: " ++ show actual]
    MockFailureArgumentValueMismatch actual expected ->
      ["Value does not match:", "Expected: " ++ show expected, "but got: " ++ show actual]
    MockFailureArgumentPredicateFailure actual ->
      ["Predicate failed:", show actual]
    MockFailureUnexpectedCall _ ->
      ["Unexpected call"] -- We do not know if the arguments have an instance of Show to print them.
    MockFailureNotCalled (ExpectedCallRecord expArgs _) ->
      ["Expected call with arguments: " ++ show expArgs, "but was not called"]

instance Show MockFailure where
  show MockFailure {..} =
    intercalate "\n" $
      ["Assertion failed for " <> show mfMock, show mfReason] <> maybe [] (\loc -> ["at:", prettyLocation loc]) mfLocation
    where
      prettyLocation SrcLoc {..} = srcLocFile ++ ":" ++ show srcLocStartLine ++ ":" ++ show srcLocStartCol ++ " in " ++ srcLocPackage ++ ":" ++ srcLocModule

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
--
-- For mocking functions with many arguments it is convenient to use `constN` and `asTypeOf`.
-- Using `asTypeOf` lets you omit the type annotation. These definitions create the same mock:
--
-- @
-- makeMock "someAction" ((\\_ _ _ -> pure "result") :: Arg1 -> Arg2 -> Arg3 -> SomeMonad ())
-- makeMock "someAction" ('constN' $ pure "result" :: Arg1 -> Arg2 -> Arg3 -> SomeMonad ())
-- makeMock "someAction" ('constN' $ pure "result" \`'asTypeOf'\` someAction)
-- @
makeMock ::
  (Function f args (m x) Typeable, Typeable f, Typeable x, MonadIO m) =>
  String ->
  f ->
  IO Mock
makeMock key f = do
  actualCallRecord <- newIORef []
  pure $ Mock key actualCallRecord (`recordArgs` f)

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
lookupMockFunction conf key = case lookupMockTyped conf key (Proxy :: Proxy f) of
  Just Mock {..} -> case cast (mockFunction mockCallRecord) of
    Just val -> Just val
    Nothing ->
      error $
        "lookupMockFunction: impossible happened. Cast failed for " <> key <> " from "
          <> show (typeOf (mockFunction mockCallRecord))
          <> " to "
          <> show (typeRep (Proxy :: Proxy f))
  Nothing -> Nothing

recordArgs ::
  (Typeable x, MonadIO m, Function f args (m x) Typeable) =>
  IORef [ActualCallRecord] ->
  f ->
  f
recordArgs callsRef = transformFunction (Proxy :: Proxy Typeable) fa fr []
  where
    fa args a = ActualVal a : args
    fr args mx = do
      x <- mx
      liftIO $ modifyIORef callsRef (ActualCallRecord (reverse args) (ActualVal x) :)
      pure x

-- | Reuse the mocks between the test items
resetMockCallRecords :: Mock -> IO ()
resetMockCallRecords (Mock _ callsRef _) = writeIORef callsRef []

-- | Reuse the mocks between the test items
resetAllCallRecords :: MockConfig -> IO ()
resetAllCallRecords MockConfig {..} = mapM_ (mapM_ resetMockCallRecords) mcStorage

-- | Find a mock by name. If there are several mocks under the same name, use `lookupMockTyped`.
lookupMock :: HasCallStack => MockConfig -> String -> Mock
lookupMock MockConfig {..} key = case Map.lookup key mcStorage of
  Nothing -> error $ "lookupMock: Mock " <> key <> " not found"
  Just tMap -> case Map.elems tMap of
    [mock] -> mock
    _ -> error $ "lookupMock: There are " <> show (Map.size tMap) <> " mocks under the name \"" <> key <> "\". Use lookupMockTyped to disambiguate."

-- | Find a mock by name and type.
lookupMockTyped :: forall t proxy. (HasCallStack, Typeable t) => MockConfig -> String -> proxy t -> Maybe Mock
lookupMockTyped MockConfig {..} key proxy = do
  let tMap = Map.lookup key mcStorage
  case tMap >>= Map.lookup tRep of
    Just mock -> Just mock
    Nothing | mcShouldFailOnNotFound key tRep -> case tMap of
      Nothing ->
        error $
          "lookupMockTyped: cannot find mock " <> key <> " :: " <> show tRep <> ". "
            <> "There are no mocks under this name."
      Just tMap' ->
        error $
          "lookupMockTyped: cannot find mock " <> key <> " :: " <> show tRep <> ". "
            <> "There are mocks with other types under the same name:\n"
            <> unlines (map show $ Map.elems tMap')
    Nothing -> Nothing
  where
    tRep = typeRep proxy

addMocksToConfig :: MockConfig -> [Mock] -> MockConfig
addMocksToConfig conf mocks = conf {mcStorage = mockMap}
  where
    mockMap = foldl' insertMock (mcStorage conf) mocks
    insertMock m mock@Mock {..} = Map.insertWith insert mockKey singleMock m
      where
        singleMock = Map.singleton tRep mock
        insert _ = Map.insert tRep mock
        tRep = typeOf $ mockFunction undefined

-- | Description of what value for mock call the assertions expect
data ExpectedVal
  = AnyArg
  | forall a. (Typeable a, Show a, Eq a) => ExpectedVal a
  | forall a. (Typeable a, Show a) => PredicateVal (a -> Bool)

instance Eq ExpectedVal where
  (==) = error "Eq ExpectedVal not implemented"

data ExpectedCallRecord = ExpectedCallRecord [ExpectedVal] ExpectedVal

instance Show ExpectedVal where
  show AnyArg = "AnyArg"
  show (ExpectedVal a) = show a
  show (PredicateVal p) = "PredicateVal p :: " <> show (typeOf p)

-- | Use this to create `ExpectedCallRecord`
--
-- Examples:
--
-- @
-- expectCall "email@example.com" True
-- @
expectCall ::
  (Function f args ExpectedCallRecord (Show & Eq & Typeable)) =>
  f
expectCall = createFunction (Proxy :: Proxy (Show & Eq & Typeable)) fa fr []
  where
    fa args arg = (: args) $ case cast arg of
      Just arg' -> arg'
      Nothing -> ExpectedVal arg
    fr args = ExpectedCallRecord (reverse args) AnyArg

-- | Assert that mock returned the given result.
-- Sometimes it is more convenient than checking arguments.
withResult :: (Show a, Eq a, Typeable a) => ExpectedCallRecord -> a -> ExpectedCallRecord
withResult (ExpectedCallRecord args _) arg = ExpectedCallRecord args r
  where
    r = case cast arg of
      Just arg' -> arg'
      Nothing -> ExpectedVal arg

checkCallRecord :: HasCallStack => ActualCallRecord -> ExpectedCallRecord -> Maybe MockFailureReason
checkCallRecord actCall@(ActualCallRecord actArgs actRes) expCall@(ExpectedCallRecord expArgs expRes) =
  argFailure <|> resFailure
  where
    argFailure =
      if length actArgs /= length expArgs
        then Just $ MockFailureArgumentCountMismatch actCall expCall
        else listToMaybe $ catMaybes $ zipWith matchArgs actArgs expArgs
    resFailure = matchArgs actRes expRes

matchArgs :: ActualVal -> ExpectedVal -> Maybe MockFailureReason
matchArgs _ AnyArg = Nothing
matchArgs (ActualVal actual) (ExpectedVal expected) =
  case cast actual of
    Just actual' | expected == actual' -> Nothing
    Just actual' ->
      Just $
        MockFailureArgumentValueMismatch actual' expected
    Nothing ->
      Just $
        MockFailureArgumentTypeMismatch (typeOf actual) (typeOf expected)
matchArgs (ActualVal (actual :: a)) (PredicateVal (p :: b -> Bool)) =
  case eqT :: Maybe (a :~: b) of
    Nothing -> Just $ MockFailureArgumentTypeMismatch (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))
    Just Refl ->
      if p actual
        then Nothing
        else Just $ MockFailureArgumentPredicateFailure actual

-- | Assert that all expected calls were made in the given order.
--
-- @
-- mock <- lookupMockInEnv "name"  -- user-defined helper function for your app env
-- liftIO $ assertHasCalls [expectCall "arg1" "arg2"] mock
-- @
assertHasCalls :: HasCallStack => [ExpectedCallRecord] -> Mock -> IO ()
assertHasCalls expectedCalls mock = do
  actualCalls <- getCalls mock
  zipEqualLength actualCalls expectedCalls
  where
    throw = throwIO . MockFailure mock location
    zipEqualLength [] [] = pure ()
    zipEqualLength (a : as) (e : es) = do
      maybe (pure ()) throw $ checkCallRecord a e
      zipEqualLength as es
    zipEqualLength (a : _) _ = throw $ MockFailureUnexpectedCall a
    zipEqualLength _ (e : _) = throw $ MockFailureNotCalled e

-- | Assert the mock was never called.
assertNotCalled :: HasCallStack => Mock -> IO ()
assertNotCalled = assertHasCalls []

-- | The expected call record matches the actual one. Note that it throws
-- error for the logic bugs when a mismatch is caused by wrong number
-- of arguments or wrong types.
callMatches :: ActualCallRecord -> ExpectedCallRecord -> Bool
callMatches actCall expCall = case checkCallRecord actCall expCall of
  Nothing -> True
  Just MockFailureArgumentValueMismatch {} -> False
  Just MockFailureArgumentPredicateFailure {} -> False
  Just reason -> error $ "callMatches: invalid arguments\n" <> show reason

-- | Assert that the expected call happened at least once.
assertAnyCall :: ExpectedCallRecord -> Mock -> IO ()
assertAnyCall expCall mock = do
  actualCalls <- getCalls mock
  unless (any (\actCall -> isNothing $ checkCallRecord actCall expCall) actualCalls) $ do
    throwIO $ MockFailure mock location $ MockFailureNotCalled expCall

-- | Get list of calls. Use together with 'callMatches'
-- when the existing assert* functions are not flexible enough.
getCalls :: Mock -> IO [ActualCallRecord]
getCalls Mock {..} = reverse <$> readIORef mockCallRecord

location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing

-- $polymorphic
-- It is common to have the application logic live in a polymorphic monad
-- that is constrained with several type classes.
-- However, only the monomorphic types have an instance of Typeable and can be mocked.
-- The solution is to wrap a polymorphic function with a concrete existential type.
--
-- The library provides MockMonadIO that lets you write a mock fitting in
-- any context that has @MonadIO@.
--
-- Define a mock and add it to the config:
--
-- > makeMock (const $ pure "getSomething" :: Int -> MockMonadIO String)
--
-- Then use it at the call site. Before calling the mock, you need to unwrap the existential type.
-- It is a good idea to define your own helpers for getting the mock config and mock lookup.
-- That would make calling mocks much more concise. See more at examples/App.hs.
--
-- Here is a verbose example that expands both retrieving and transforming the mock.
--
-- > insideAppPolymorphicMonad :: (HasEnv m, MonadIO m) => Int -> m ()
-- > insideAppPolymorphicMonad arg = do
-- >   mockConf <- getMockConfig <$> getEnv
-- >   let mock = lookupMockFunction mockConf "getSomething"
-- >   (maybe unMockMonadIO1 getSomething mock) arg
--
-- With your own helpers it can look like this:
--
-- > insideAppPolymorphicMonad :: (HasEnv m, MonadIO m) => Int -> m ()
-- > insideAppPolymorphicMonad arg = do
-- >   myMockHelper unMockMonadIO1 getSomething mock >>= \f -> f arg

-- $polymorphicAdvanced
-- === Advanced polymorphic mocks
-- If your mock needs to be aware of a custom class to return a result, and
-- be able to called from polymorphic code, make your own wrapper similar
-- to MockMonadIO. For example:
--
-- > newtype MockMonadHasMyEnv a = MockMonadHasMyEnv {
-- >   unMockMonadHasMyEnv :: forall m. (MonadIO m, HasMyEnv m) => m a
-- > }
--
-- > makeMock (\a -> getMyEnv >>= makeFakeResult ... :: Int -> MockMonadHasMyEnv String)

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

-- | The family of functions unMockMonadIO<N> is specialized with the number of arguments.
-- Unlike @fromMockMonadIO@, the monad @m@ can be polymorphic at the call site.
unMockMonadIO1 :: (a -> MockMonadIO x) -> (forall m. MonadIO m => a -> m x)
unMockMonadIO1 f = unMockMonadIO . f

unMockMonadIO2 :: (a -> b -> MockMonadIO x) -> (forall m. MonadIO m => a -> b -> m x)
unMockMonadIO2 f = unMockMonadIO1 . f

unMockMonadIO3 :: (a -> b -> c -> MockMonadIO x) -> (forall m. MonadIO m => a -> b -> c -> m x)
unMockMonadIO3 f = unMockMonadIO2 . f

unMockMonadIO4 :: (a -> b -> c -> d -> MockMonadIO x) -> (forall m. MonadIO m => a -> b -> c -> d -> m x)
unMockMonadIO4 f = unMockMonadIO3 . f

unMockMonadIO5 :: (a -> b -> c -> d -> e -> MockMonadIO x) -> (forall m. MonadIO m => a -> b -> c -> d -> e -> m x)
unMockMonadIO5 f = unMockMonadIO4 . f

-- | Changes the return type of a function from @MockMonadIO x@ to @m x@.
-- The @m@ must be a concrete type at the call site.
-- If the caller is in a polymorphic monad, use one of the @unMockMonadIO<N>@ instead.
fromMockMonadIO ::
  forall m x args f' f.
  (MonadIO m, Function f' args (MockMonadIO x) EmptyConstraint, Function f args (m x) EmptyConstraint) =>
  f' ->
  f
fromMockMonadIO = composeN unMockMonadIO
