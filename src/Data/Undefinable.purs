-- | This module defines a representation for undefined.
module Data.Undefinable
  ( Undefinable
  , toMaybe
  , toUndefinable
  ) where

import Prelude (class Eq, class Ord, class Show, (<<<), compare, eq, show)

import Data.Function (on)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(Just,Nothing), maybe)

-- | This type constructor defines a representation for a value that may
-- | or may not be undefined.
foreign import data Undefinable :: * -> *

-- | Takes `undefined` to `Nothing` and a `value` to `Just value`.
toMaybe :: forall value. Undefinable value -> Maybe value
toMaybe = runFn3 undefinable Nothing Just

-- | Takes `Nothing` to `undefined` and `Just value` to `value`.
toUndefinable :: forall value. Maybe value -> Undefinable value
toUndefinable = maybe undefined notUndefined

instance showUndefinable :: (Show a) => Show (Undefinable a) where
  show = maybe "undefined" show <<< toMaybe

instance eqUndefinable :: (Eq a) => Eq (Undefinable a) where
  eq = eq `on` toMaybe

instance ordUndefinable :: (Ord a) => Ord (Undefinable a) where
  compare = compare `on` toMaybe

foreign import undefined :: forall value. Undefinable value

foreign import undefinable :: forall value. Fn3 (Maybe value) (value -> Maybe value) (Undefinable value) (Maybe value)

foreign import notUndefined :: forall value. value -> Undefinable value
