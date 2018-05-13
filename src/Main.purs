module App.Main where

import Prelude
import App.App as A
import App.Routes (match)
import Control.Monad.Eff (Eff)
import DOM.HTML (window)
import Pux (App, CoreEffects, start)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (renderToDOM, renderToReact)
import React (ReactClass)
import Signal ((~>))
import App.Effects (AppEffects)

type WebApp = App (DOMEvent -> A.Event) A.Event A.State

main :: A.State -> Eff ( CoreEffects AppEffects) WebApp
main state = do
  app <- start
    { initialState: state
    , foldp: A.foldp
    , view: A.view
    , inputs: []
    }
  _ <- renderToDOM "#app" app.markup app.input
  pure app


toReact :: forall props . A.State -> Eff ( CoreEffects AppEffects) {component :: (ReactClass props), app:: WebApp}
toReact state = do
  urlSignal <- sampleURL =<< window
  let routeSignal = urlSignal ~> (A.PageView <<< match)
  app <- start
    { initialState: state
    , foldp: A.foldp
    , view: A.view
    , inputs: [routeSignal] -- [routeSignal]
    }
  renderer <- renderToReact app.markup app.input 
  pure $ { component: renderer, app: app }


initalState :: A.State
initalState = A.initialState
