module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Maybe (Maybe(..))
import Data.Undefinable (toUndefinable, toMaybe)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ toUndefinable (Nothing :: Maybe Number)
  logShow $ toUndefinable (Just 42)

  logShow $ toMaybe $ toUndefinable (Nothing :: Maybe Number)
  logShow $ toMaybe $ toUndefinable (Just 42)

  logShow $ toUndefinable Nothing == toUndefinable (Just 42)
  logShow $ toUndefinable Nothing `compare` toUndefinable (Just 42)

  logShow $ (identity <$> (toUndefinable Nothing)) == toUndefinable Nothing
  logShow $ (identity <$> (toUndefinable (Just 42))) == toUndefinable (Just 42)

  logShow $ (plus1 <$> toUndefinable Nothing) == toUndefinable Nothing
  logShow $ (plus1 <$> toUndefinable (Just 42)) == toUndefinable (Just 43)
  logShow $ ((times2 <<< plus1) <$> toUndefinable (Just 42)) == ((map times2) <<< (map plus1) $ toUndefinable (Just 42))

  where
    identity x = x
    plus1 x = x + 1
    times2 x = x * 2
