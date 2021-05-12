{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-} -- Required by & class
{-# LANGUAGE UndecidableSuperClasses #-} -- Required by & class
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeableMock.Types where

import Data.Kind (Type)

-- | Toolkit for creating and transforming functions with a variable number of arguments.

class (args ~ FunctionArgs f, r ~ FunctionResult f, ConstructFunction args r ~ f) =>
  Function argC f args r | args r -> f, f args -> r where
  -- | Make a new function out of an existing one. It keeps the same arguments but may have a different type of result.
  transformFunction :: (args ~ FunctionArgs f', r' ~ FunctionResult f', ConstructFunction args r' ~ f')
    => proxy argC  -- ^ Required for unambiguous choice of Function instance
    -> (forall a. argC a => acc -> a -> acc)   -- ^ Combine arguments with accumulator
    -> (acc -> r -> r')  -- ^ Make result of the target function
    -> acc  -- ^ Accumulator
    -> f  -- ^ The original function
    -> f' -- ^ The new function
  
  -- | Create a new function
  --
  -- === __Example usage__
  --
  -- >>> printf :: Function Show func args String => func
  -- >>> printf = createFunction (Proxy :: Proxy Show) (\acc a -> acc <> show a) id ""
  -- >>> printf "hello" () :: String
  -- "hello()"
  createFunction
    :: proxy argC  -- ^ Required for unambiguous choice of Function instance
    -> (forall a. argC a => acc -> a -> acc)  -- ^ Combine arguments with accumulator
    -> (acc -> r)   -- ^ Make result of the function
    -> acc  -- ^ Accumulator
    -> f

-- | Extract list of arguments from a function.
type family FunctionArgs f :: [Type] where
  FunctionArgs (a -> f) = a:FunctionArgs f
  FunctionArgs x = '[]

-- | Extract the type of function result.
type family FunctionResult f :: Type where
  FunctionResult (a -> f) = FunctionResult f
  FunctionResult x = x

-- | Given the types of functions arguments and result make a type of a function.
type family ConstructFunction (args :: [Type]) (r :: Type) where
  ConstructFunction '[] r = r
  ConstructFunction (a:as) r = a -> ConstructFunction as r

instance ('[] ~ FunctionArgs r, r ~ FunctionResult r)
  => Function argC r '[] r where
  transformFunction _ _ fr acc r = fr acc r
  createFunction _ _ fr r = fr r

instance (Function argC f args r, argC a)
  => Function argC (a -> f) (a:args) r where
  transformFunction pArgC fa fr acc f = \a -> transformFunction pArgC fa fr (fa acc a) (f a)
  createFunction pArgC fa fr acc = createFunction pArgC fa fr . fa acc

-- | Combine constraints
class (f x, g x) => (&) f g (x :: k)
instance (f x, g x) => (&) f g x
