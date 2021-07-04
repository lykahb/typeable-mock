{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module App where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader
import Data.Function.Variadic
import Data.Maybe
import Data.Typeable (Typeable)
import Test.Hspec
import Test.TypeableMock

-- Define a toy application.
newtype AppEnv = AppEnv
  { mockConfig :: MockConfig
  }

-- Define mock helpers for the application. Here there are helpers for both
-- concrete and polymorphic contexts. You may only need some of them.
getMockConfig :: MonadReader AppEnv m => m MockConfig
getMockConfig = asks mockConfig

-- | Find mock and return its function as-is.
-- This is enough if your app uses one particular monad.
useMock :: (MonadReader AppEnv m, Typeable f) => String -> f -> m f
useMock = useMockConvert getMockConfig id

-- | Find mock and unwrap MockMonadIO for use in any concrete MonadIO monad.
-- It lets you use mocks with multiple monads. If you mock function does side
-- effects, it can only use capabilities of @MonadIO@ instead of a richer
-- concrete monad.
useMockMonadIO ::
  ( MonadReader AppEnv m,
    MonadIO m,
    Typeable f',
    Function f' args (MockMonadIO x) EmptyConstraint,
    Function f args (m x) EmptyConstraint
  ) =>
  String ->
  f ->
  m f
useMockMonadIO = useMockConvert getMockConfig fromMockMonadIO

-- | Find mock and apply a converter for use in a polymorphic monad
useMockWithConv :: (MonadReader AppEnv m, Typeable mock) => (mock -> f) -> String -> f -> m f
useMockWithConv = useMockConvert getMockConfig

-- Application logic
greet :: (MonadIO m, MonadReader AppEnv m) => String -> m ()
greet name = liftIO $ print $ "Hello " <> name

readerTGreeter :: String -> ReaderT AppEnv IO ()
readerTGreeter name = do
  -- These calls use two different mocks.
  -- This mock function has type String -> MockMonadIO ()
  useMockMonadIO "greet" greet >>= \f -> f name
  -- This mock function has type String -> ReaderT AppEnv ()
  useMock "greet" greet >>= \f -> f name

polymorphicGreeter :: (MonadIO m, MonadReader AppEnv m) => String -> m ()
polymorphicGreeter name = do
  -- This mock function has type String -> MockMonadIO ().
  -- In the polymorphic context unwrapping of MockMonadIO must be explicit.
  useMockWithConv unMockMonadIO1 "greet" greet >>= \f -> f name

app :: ReaderT AppEnv IO ()
app = do
  readerTGreeter "monomorphic world"
  polymorphicGreeter "polymorphic world"

-- Test suite
main :: IO ()
main = hspec $ do
  -- This is a convenient form to initialize multiple mocks at once.
  -- To illustrate adding mocks later, we mock greet with only one type.
  mockConf <-
    runIO $
      addMocksToConfig defaultMockConfig
        <$> sequence
          [ makeMock "greet" (const $ pure () :: String -> ReaderT AppEnv IO ())
          ]

  before_ (resetAllCallRecords mockConf) $ do
    it "works in concrete monad" $ do
      runReaderT app $ AppEnv mockConf

      -- Look up a mock by type and name
      let Just mockReaderT = lookupMockTyped @(String -> ReaderT AppEnv IO ()) mockConf "greet"

      assertHasCalls [expectCall "monomorphic world"] mockReaderT

    it "works in polymorphic monad" $ do
      -- This mock also has the name "greet" but it has a different type.
      mockMockMonadIO <- makeMock "greet" (const $ pure () :: String -> MockMonadIO ())

      -- Now both mocks under the name "greet" are available.
      let mockConf' = addMocksToConfig mockConf [mockMockMonadIO]

      runReaderT app $ AppEnv mockConf'

      assertHasCalls
        [ expectCall "monomorphic world",
          expectCall "polymorphic world"
        ]
        mockMockMonadIO
