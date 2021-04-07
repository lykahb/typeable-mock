# mock-typeable

## Usage



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

Mock-typeable 