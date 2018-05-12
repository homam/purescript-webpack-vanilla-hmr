module App.Counter where

import Prelude
import Text.Smolder.Markup
import Pux.DOM.Events
import DOM.Event.Types
import Query.Parser.UrlQueryParser as P
import Query.Types as P
import Control.Monad.Aff (attempt, launchAff)
import Control.Monad.Aff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM.Event.KeyboardEvent as KeyboardEvent
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest, get)
import Pux (EffModel, noEffects)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (reactClass, reactClassWithProps)
import React (ReactClass)
import Text.Smolder.HTML (button, h1, h3, div, input) as S
import Text.Smolder.HTML.Attributes (name, type', value)
import Unsafe.Coerce (unsafeCoerce)

data Action = Increment | Decrement | Query | QueryTextChanged DOMEvent | QueryTextKeyUp KeyboardEvent

type State = { count :: Int, queryText :: String }

initialState :: State
initialState = {count: 0, queryText: "country_code:(AE,ZA,TH,MY,MX,OM,QA)"}

type MyEffects = (ajax :: AJAX, console :: CONSOLE)

update ::  Action -> State ->  EffModel State Action (ajax :: AJAX, console :: CONSOLE) -- forall e . Action -> State ->  EffModel State Action e
update Increment state = noEffects $ state { count = state.count + 1 }
update Decrement state = noEffects $ state { count= state.count - 3 }
update (QueryTextChanged ev) state = noEffects $ state { queryText = targetValue ev }
update (QueryTextKeyUp ev) state = {
  state,
  effects: [
    if "Enter" == KeyboardEvent.key ev
      then do
        case P.runFilterParser state.queryText of
          Left e -> liftEff $ consoleLog e
          Right r -> liftEff $ consoleLog r *> consoleLog (P.toQueryPathString r)
        pure Nothing
      else pure Nothing
  ]
}
update Query state = {
  state: state,
  effects: [
    do
      let url =  "http://localhost:8080/api/" <> state.queryText <> "?sync=true"
      res <- affjax $ defaultRequest { url = url, method = Left GET }
      liftEff $ consoleLog (res.response :: Foreign)
      pure Nothing
  ]
}

foreign import buttonClass :: ∀ props. ReactClass props
foreign import consoleLog :: ∀ a e. a -> Eff e Unit

-- myButton :: ∀ ev. HTML ev
myButton = reactClassWithProps buttonClass "button"

view :: State -> HTML Action
view state =
  S.div do
    S.h1 $ text "PureScript ++ Vanilla HMR"
    S.h3 $ text $ ("Count: " <> show state.count)
    S.input ! type' "text" ! value state.queryText #! onChange QueryTextChanged #! onKeyUp (QueryTextKeyUp <<< unsafeCoerce)
    S.div do
      myButton {} #! onClick (const Increment) $ text "Increment+"
      S.button #! onClick (const Decrement) $ text "Decrement"
      S.button #! onClick (const Query) $ text "Query"
