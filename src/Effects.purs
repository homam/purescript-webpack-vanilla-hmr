module App.Effects where
  
import Control.Monad.Aff.Console (CONSOLE)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import Network.HTTP.Affjax (AJAX)

type AppEffects = (ajax :: AJAX, console :: CONSOLE, history :: HISTORY, dom :: DOM)