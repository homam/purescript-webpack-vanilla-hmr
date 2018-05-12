module App where

import Prelude
import Pux.Renderer.React as R
import App.Counter (Action, State, MyEffects, update, view)
import Control.Monad.Eff (Eff)
import Pux (App, CoreEffects, start)
import Pux.DOM.Events (DOMEvent)
import Pux.Renderer.React (renderToDOM)
import React (ReactClass)

type WebApp = App (DOMEvent -> Action) Action State

main :: State -> Eff ( CoreEffects MyEffects) WebApp
main state = do
  app <- start
    { initialState: state
    , foldp: update
    , view
    , inputs: []
    }
  _ <- renderToDOM "#app" app.markup app.input
  pure app

toReact :: forall props . State -> Eff ( CoreEffects MyEffects) {component :: (ReactClass props), app:: WebApp}
toReact state = do
  app <- start
    { initialState: state
    , foldp: update
    , view
    , inputs: []
    }
  renderer <- R.renderToReact app.markup app.input -- renderToDOM
  -- pure $ Tuple renderer app
  pure $ { component: renderer, app: app }

