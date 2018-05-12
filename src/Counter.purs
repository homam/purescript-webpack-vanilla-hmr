module App.Counter where

import Prelude
import Text.Smolder.Markup
import Control.Monad.Aff (attempt, launchAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest, get)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (reactClass, reactClassWithProps)
import React (ReactClass)
import Text.Smolder.HTML (button, h1, h3, div) as S
import Text.Smolder.Markup (text) as S

data Action = Increment | Decrement | Query

type State = Int

initialState :: State
initialState = 0

type MyEffects = (ajax :: AJAX, console :: CONSOLE)

update ::  Action -> State ->  EffModel State Action (ajax :: AJAX, console :: CONSOLE) -- forall e . Action -> State ->  EffModel State Action e
update Increment state = noEffects $ state + 1
update Decrement state = noEffects $ state - 3
update Query state = {
  state: state,
  effects: [
    do
      let url =  "http://localhost:8080/api/Sessions/2/2018-05-01/2018-05-07/country_code:(AE,ZA,TH,MY,MX,OM,QA),affiliate_id:POM*/country_code:(sales:D),publisher_id:(sales:D),day:(views:D,(sales:10,views:100))?sync=true"
      res <- affjax $ defaultRequest { url = url, method = Left GET }
      log res.response
      pure Nothing
  ]
}

foreign import buttonClass :: ∀ props. ReactClass props

-- myButton :: ∀ ev. HTML ev
myButton = reactClassWithProps buttonClass "button"

view :: State -> HTML Action
view state =
  S.div do
    S.h1 $ S.text "PureScript ++ Vanilla HMR"
    S.h3 $ S.text $ ("Count: " <> show state)
    S.div do
      myButton {} #! onClick (const Increment) $ S.text "Increment+"
      S.button #! onClick (const Decrement) $ S.text "Decrement"
      S.button #! onClick (const Query) $ S.text "Query"
