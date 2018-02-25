-- | This module defines a representation for undefined.
module Data.Undefinable
  ( Undefinable
  , undefined
  , defined
  , toMaybe
  , toUndefinable
  , fold
  ) where

import Prelude (class Eq, class Ord, class Show, (<<<), compare, eq, show)

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (class Apply)
import Control.Applicative (class Applicative)
import Control.Bind (class Bind)
import Control.Extend (class Extend)
import Control.Monad (class Monad)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Bounded (class Bounded, top)
import Data.Eq (class Eq1, (==))
import Data.Function (on, const)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Functor (class Functor, map)
import Data.Functor.Invariant (class Invariant)
import Data.Maybe (Maybe(Just,Nothing), maybe)
import Data.Ord (class Ord1)
import Data.Semigroup (class Semigroup, (<>))
import Unsafe.Coerce (unsafeCoerce)

-- | This type constructor defines a representation for a value that may
-- | or may not be undefined.
foreign import data Undefinable :: Type -> Type

-- | Takes `undefined` to `Nothing` and `defined x` to `Just x`.
toMaybe :: forall a. Undefinable a -> Maybe a
toMaybe = runFn3 fold_ Nothing Just

-- | Takes `Nothing` to `undefined` and `Just x` to `defiend x`.
toUndefinable :: forall a. Maybe a -> Undefinable a
toUndefinable = maybe undefined defined

-- | Construct an `Undefinable a` from some `x :: a`.
defined :: forall a. a -> Undefinable a
defined = unsafeCoerce

-- | Destruct an `Undefinable a`.
fold :: forall a r. r -> (a -> r) -> Undefinable a -> r
fold = runFn3 fold_

instance showUndefinable :: (Show a) => Show (Undefinable a) where
  show = runFn3 fold_ "undefined" (\x -> "(defined " <> show x <> ")")

instance eqUndefinable :: (Eq a) => Eq (Undefinable a) where
  eq = eq `on` toMaybe

instance ordUndefinable :: (Ord a) => Ord (Undefinable a) where
  compare = compare `on` toMaybe

instance functorUndefinable :: Functor Undefinable where
  map f = runFn3 fold_ undefined (defined <<< f)

instance altUndefinable :: Alt Undefinable where
  alt u v = runFn3 fold_ v (const u) u

instance alternativeUndefinable :: Alternative Undefinable

instance applyUndefinable :: Apply Undefinable where
  apply u v = runFn3 fold_ undefined (\f -> map f v) u
  
instance applicativeUndefinable :: Applicative Undefinable where
  pure = defined

instance bindUndefinable :: Bind Undefinable where
  bind u f = runFn3 fold_ undefined f u

instance monadUndefinable :: Monad Undefinable

instance monadZeroUndefinable :: MonadZero Undefinable

instance extendUndefinable :: Extend Undefinable where
  extend f u = defined (f u)

instance invariantUndefinable :: Invariant Undefinable where
  imap f _ = map f

instance
  semigroupUndefinable :: Semigroup a => Semigroup (Undefinable a) where
  append u v = runFn3 fold_ undefined (\x -> map (x <> _) v) u

instance eq1Undefinable :: Eq1 Undefinable where
  eq1 = eq

instance ord1Undefinable :: Ord1 Undefinable where
  compare1 = compare

instance plusUndefinable :: Plus Undefinable where
  empty = undefined

instance
  boundedUndefinable :: Bounded a => Bounded (Undefinable a) where
  top = defined top
  bottom = undefined

-- | Construct an `Undefinable a`.
foreign import undefined :: forall a. Undefinable a

foreign import fold_ :: forall a r. Fn3 r (a -> r) (Undefinable a) r
