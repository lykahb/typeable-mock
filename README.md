# typeable-mock

## Introduction

This library lets you add mocks for the function calls without updating your types. Whether the mocked function is called from a concrete monad, or a polymorphic one with class constraints - it will work there as long as it has the environment to store a `MockConfig`. Internally the library stores the mock functions in a Map with a mock name and type as a key.

### Usage

Here we have a snippet of code that calls a function too dangerous to run it live when testing.

```
launchMissiles :: Location -> IO ()
launchMissiles location = ...

runEverything :: ReaderT Env IO ()
runEverything = do
  ...
  launchMissiles target
```

When testing the `executeCommand`, we want to know that `launchMissiles` was run with a correct location. So we put a mock configuration into the environment Env and update the function to check for mock.

```
it "launches missile" $ do
  launchMissilesMock <- makeMock "launchMissiles" $ mockM1 (const $ pure () :: Location -> IO ())
  let mockConfig = mocksToConfig [printStringMock]
  runReaderT runEverything $ testEnv {envMockConfig = mockConfig}
  -- confirm that the function was called and had the expected
  assertHasCalls launchMissilesMock [call target]


executeCommand :: ReaderT Env IO ()
executeCommand (LaunchMissiles location) = do
  mockConfig <- envMockConfig <$> ask
  fromMaybe launchMissiles (useMockM1 mockConfig "launchMissiles") location
```

It is tedious to get mockConfig from the environment and pass it around. The recommended way is to write your own helpers, such as

```
useMockM1 :: String -> (a -> m x) -> a -> ReaderT Env IO x
useMockM1 mockName f a1 = do
  mockConfig <- envMockConfig <$> ask
  fromMaybe f (Mock.useMockM1 mockConfig mockName) a1

executeCommand :: ReaderT Env IO ()
executeCommand (LaunchMissiles location) = do
  useMockM1 "launchMissiles" launchMissiles location
```

### Polymorphic types
Using typeable is challenging when some types are polymorphic. It is a common pattern to have a polymorphic monad that sastisfies the type classes.
Naively calling typeOf on a polymorphic type would result in an error:
```
printType :: MonadIO m => m ()
printType = x where
  x = liftIO . print $ typeOf x

    • Could not deduce (Typeable m) arising from a use of ‘typeOf’
      from the context: MonadIO m
        bound by the type signature for:
                   printType :: forall (m :: * -> *). MonadIO m => m ()
```

typeable-mock has helpers for mocking functions that are polymorphic on their monad. When declaring mock in the tests, use `mockMX` where X is the number of arguments. Inside of the code that calls mock, use one of the corresponding functions `useMockMX`.