module App.Counter where

import Prelude

import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (reactClass, reactClassWithProps)
import React (ReactClass)
import Text.Smolder.HTML (button, h1, h3, div) as S
import Text.Smolder.Markup -- ((#!))
import Text.Smolder.Markup (text) as S

data Action = Increment | Decrement

type State = Int

initialState :: State
initialState = 0

update :: forall e . Action -> State ->  EffModel State Action e
update Increment state = noEffects $ state + 1
update Decrement state = noEffects $ state - 3

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
