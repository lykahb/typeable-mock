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
    constN,
    mappendN,
  )
where

import Data.Kind (Constraint, Type)

-- | Toolkit for creating and transforming functions with a variable number of arguments.
-- Its parameters are function, list of its arguments, its result, and `argC` that constraints all arguments of the function.
class
  (args ~ FunctionArgs func, result ~ FunctionResult func, ConstructFunction args result ~ func) =>
  Function func args result (argC :: Type -> Constraint)
    | args result -> func,
      func args -> result
  where
  -- | Create a function with the same arguments as given one but may have a different result.
  transformFunction ::
    -- | Required for unambiguous choice of Function instance
    proxy argC ->
    -- | Combine arguments with accumulator
    (forall a. argC a => acc -> a -> acc) ->
    -- | Create result of the `func` function using accumulator and the result of the function to transform
    (acc -> resultOrig -> result) ->
    -- | Accumulator
    acc ->
    -- | The function to transform
    ConstructFunction args resultOrig ->
    -- | The new function
    func

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
    (acc -> result) ->
    -- | Accumulator
    acc ->
    func

-- | Extract list of arguments from a function.
type family FunctionArgs func :: [Type] where
  FunctionArgs (a -> func) = a : FunctionArgs func
  FunctionArgs x = '[]

-- | Extract the type of function result from a function.
type family FunctionResult func :: Type where
  FunctionResult (a -> func) = FunctionResult func
  FunctionResult x = x

-- | Given the types of functions arguments and result make a type of a function.
type family ConstructFunction (args :: [Type]) (result :: Type) where
  ConstructFunction '[] result = result
  ConstructFunction (a : args) result = a -> ConstructFunction args result

instance
  ('[] ~ FunctionArgs result, result ~ FunctionResult result) =>
  Function result '[] result argC
  where
  transformFunction _ _ fr acc r = fr acc r
  createFunction _ _ fr r = fr r

instance
  (Function func args result argC, argC a) =>
  Function (a -> func) (a : args) result argC
  where
  transformFunction pArgC fa fr acc f = \a -> transformFunction pArgC fa fr (fa acc a) (f a)
  createFunction pArgC fa fr acc = createFunction pArgC fa fr . fa acc

class EmptyConstraint a

instance EmptyConstraint a

-- | Combine constraints
class (f x, g x) => (&) f g (x :: k)

instance (f x, g x) => (&) f g x

-- | Function composition for an arbitrary number of arguments.
--
-- >>> (show `composeN` \a b c -> (a + b + c :: Int)) 1 2 3
-- "6"
composeN :: (Function f args a EmptyConstraint, Function g args b EmptyConstraint) => (a -> b) -> f -> g
composeN f = transformFunction (undefined :: p EmptyConstraint) const (\_ r -> f r) ()

-- | Constant function for an arbitrary number of arguments.
--
-- @
-- let const2 = constN :: x -> a -> b -> x
-- @
--
-- >>> zipWith3 (constN 1) [1..10] [1..5] ["a", "b", "c"] :: [Int]
-- [1,1,1]
constN :: Function f args a EmptyConstraint => a -> f
constN a = createFunction (undefined :: p EmptyConstraint) const (const a) ()

-- | Append multiple monoid values. It is similar to `mconcat` but takes the values as arguments rather than list elements.
--
-- >>> mappendN [1, 2] [3] [4, 5] :: [Int]
-- [1,2,3,4,5]
mappendN :: forall r f args. (Function f args r ((~) r), Monoid r) => f
mappendN = createFunction (undefined :: proxy ((~) r)) mappend id mempty
