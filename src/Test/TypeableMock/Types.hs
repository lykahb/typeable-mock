{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- Required by & class
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- Required by & class
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.TypeableMock.Types
  ( Function (..),
    FunctionArgs,
    FunctionResult,
    ConstructFunction,
    EmptyConstraint,
    type (&),
    composeN,
  )
where

import Data.Kind (Type)

-- | Toolkit for creating and transforming functions with a variable number of arguments.
class
  (args ~ FunctionArgs f, r ~ FunctionResult f, ConstructFunction args r ~ f) =>
  Function argC f args r
    | args r -> f,
      f args -> r
  where
  -- | Make a new function out of an existing one. It keeps the same arguments but may have a different type of result.
  transformFunction ::
    -- | Required for unambiguous choice of Function instance
    proxy argC ->
    -- | Combine arguments with accumulator
    (forall a. argC a => acc -> a -> acc) ->
    -- | Make result of the target function
    (acc -> r0 -> r) ->
    -- | Accumulator
    acc ->
    -- | The original function
    ConstructFunction args r0 ->
    -- | The new function
    f

  -- | Create a new function
  --
  -- === __Example usage__
  --
  -- >>> printf :: Function Show func args String => func
  -- >>> printf = createFunction (Proxy :: Proxy Show) (\acc a -> acc <> show a) id ""
  -- >>> printf "hello" () :: String
  -- "hello()"
  createFunction ::
    -- | Required for unambiguous choice of Function instance
    proxy argC ->
    -- | Combine arguments with accumulator
    (forall a. argC a => acc -> a -> acc) ->
    -- | Make result of the function
    (acc -> r) ->
    -- | Accumulator
    acc ->
    f

-- | Extract list of arguments from a function.
type family FunctionArgs f :: [Type] where
  FunctionArgs (a -> f) = a : FunctionArgs f
  FunctionArgs x = '[]

-- | Extract the type of function result from a function.
type family FunctionResult f :: Type where
  FunctionResult (a -> f) = FunctionResult f
  FunctionResult x = x

-- | Given the types of functions arguments and result make a type of a function.
type family ConstructFunction (args :: [Type]) (r :: Type) where
  ConstructFunction '[] r = r
  ConstructFunction (a : as) r = a -> ConstructFunction as r

instance
  ('[] ~ FunctionArgs r, r ~ FunctionResult r) =>
  Function argC r '[] r
  where
  transformFunction _ _ fr acc r = fr acc r
  createFunction _ _ fr r = fr r

instance
  (Function argC f args r, argC a) =>
  Function argC (a -> f) (a : args) r
  where
  transformFunction pArgC fa fr acc f = \a -> transformFunction pArgC fa fr (fa acc a) (f a)
  createFunction pArgC fa fr acc = createFunction pArgC fa fr . fa acc

class EmptyConstraint a

instance EmptyConstraint a

-- | Function composition for any number of arguments
--
-- >>> (show `composeN` \a b c -> (a + b + c :: Int)) 1 2 3
-- "6"
composeN :: (Function EmptyConstraint f args a, Function EmptyConstraint g args b) => (a -> b) -> f -> g
composeN f = transformFunction (undefined :: p EmptyConstraint) const (\_ r -> f r) ()

-- | Combine constraints
class (f x, g x) => (&) f g (x :: k)

instance (f x, g x) => (&) f g x
