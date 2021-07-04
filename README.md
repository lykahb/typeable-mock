Mock any `Typeable` function or expression anywhere.

A common approach to mocking in Haskell is with type classes, with instances for the real and test logic. Typeable-mock fills a niche for the projects that do not use granular type classes so much. It lets you mock any `Typeable` expression in context of nearly any monad. It works in a monad of a concrete type or a polymorphic one with class constraints.

```haskell
-- Use mock in application. Here `useMock` is a user-written helper that
-- is aware of the application context and can look up mocks in there.
useMock "writeFile" writeFile >>= \f -> liftIO (f path contents)

-- Declare mock in test.
writeFileMock <- makeMock "writeFile"
  ((\_ _ -> pure ()) :: FilePath -> String -> IO ())

-- Check assertions
assertHasCalls
  [ expectCall "/tmp/1.txt" "Hello world",
    expectCall AnyVal (PredicateVal $ elem "Hello" . words)
  ]
  writeFileMock
```

See the package documentation and [examples/App.hs](https://github.com/lykahb/typeable-mock/blob/master/examples/App.hs) for more.
