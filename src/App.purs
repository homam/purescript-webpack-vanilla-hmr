module App where

import Prelude

import App.Counter (Action, State, update, view)
import Control.Monad.Eff (Eff)
import Pux (App, CoreEffects, start)
import Pux.DOM.Events (DOMEvent)
import Pux.Renderer.React (renderToDOM)
import Pux.Renderer.React as R
import React (ReactClass)

type WebApp = App (DOMEvent -> Action) Action State

main :: forall e . State -> Eff (CoreEffects e) WebApp
main state = do
  app <- start
    { initialState: state
    , foldp: update
    , view
    , inputs: []
    }
  _ <- renderToDOM "#app" app.markup app.input
  pure app

toReact :: forall e props . State -> Eff (CoreEffects e) {component :: (ReactClass props), app:: WebApp}
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

