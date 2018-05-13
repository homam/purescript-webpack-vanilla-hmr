module App.Utils where

import Prelude
import Control.Monad.Eff (Eff)

foreign import consoleLog :: ∀ a e. a -> Eff e Unit

foreign import jsonStringify :: forall a. a -> String