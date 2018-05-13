module App.Layout where

import Prelude (bind, const, discard, pure, (#), ($), (<>))
import Text.Smolder.Markup (text, (!), (#!))
import Pux.DOM.Events (onClick)
import App.Utils (consoleLog, jsonStringify)
import App.Components.QueryInput as QueryInput
import Query.Parser.UrlQueryParser (runBreakdownParser, runFilterParser) as P
import Text.Smolder.HTML as S
import Control.Monad.Eff.Class (liftEff)
import Control.MonadZero (empty)
import Data.Either (Either(Left))
import Data.Foreign (Foreign)
import Data.HTTP.Method (Method(..))
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Traversable (intercalate)
import Network.HTTP.Affjax (affjax, defaultRequest)
import Pux (EffModel, mapEffects, mapState, noEffects)
import Pux.DOM.HTML (HTML, child)
import Query.Types (class ToQueryPathString)
import React (ReactClass)
import Text.Smolder.HTML.Attributes (className, type', value)
import App.Utils.RouteValue (RouteValue(..), fromRouteValue)
import App.LayoutTypes (Action(..), QueryInputType(..), QueryState(..), State)
import App.Effects (AppEffects)

initialState :: State
initialState = {
    filterQueryInput: QueryInput.initialState { parser: P.runFilterParser, value: "" }
  , breakdownQueryInput: QueryInput.initialState { parser: P.runBreakdownParser, value: "" }
  , filterStr: NotInitialized
  , breakdownStr: NotInitialized
  , dateFrom: "2018-05-09"
  , dateTo: "2018-05-15"
  , timezone: "2"
  , result: NothingYet
}

update :: Action -> State ->  EffModel State Action AppEffects
update (QueryInputAction ty ev) state = 
  let tup = 
          case ty of
            FilterQueryInputType ->  handle state.filterQueryInput (\s -> state { filterQueryInput = s, filterStr = FromComponent s.value })
            BreakdownQueryInputType -> handle state.breakdownQueryInput (\s -> state { breakdownQueryInput = s, breakdownStr = FromComponent s.value })
  in {state: tup.state, effects: tup.effects} -- A.cons (liftEff (consoleLog state *> consoleLog ev *> consoleLog tup.state) *> pure Nothing) tup.effects}

  where
  handle :: forall a. ToQueryPathString a => QueryInput.State a
  → (QueryInput.State a → State ) 
  → EffModel State Action AppEffects
  handle  get' set' = 
    QueryInput.update ev get'
      # mapEffects (QueryInputAction ty) # mapState set'

update (Result result) state = noEffects $ state { result = CompletedSuccessfully result }
update Query state = {
  state: state { result = Running },
  effects: [
    do
      let url = "http://localhost:8080/api/" <>  intercalate "/" ("Sessions" : state.timezone :  state.dateFrom : state.dateTo : state.filterQueryInput.value : state.breakdownQueryInput.value : empty) <> "?sync=true"
      res <- affjax $ defaultRequest { url = url, method = Left GET }
      liftEff $ consoleLog (res.response :: Foreign)
      pure $ Just (Result res.response)
  ]
}

view :: State -> HTML Action
view state =
  S.div do
    S.h1 $ text "PureScript + Vanilla HMR"
    S.div do
      S.div ! className "row" $ do
        S.input ! type' "date" ! value state.dateFrom
        S.input ! type' "date" ! value state.dateTo
      row "Filter" $ 
        child (QueryInputAction FilterQueryInputType) QueryInput.view $ 
          state.filterQueryInput { value = fromRouteValue "" state.filterStr }
      row "Breakdown" $ 
        child (QueryInputAction BreakdownQueryInputType) QueryInput.view $ 
          state.breakdownQueryInput { value = fromRouteValue "" state.breakdownStr }
    S.div do
      S.button #! onClick (const Query) $ text "Query"
    S.div $ viewQueryState state.result
  
  where
  row label comp = 
    S.div ! className "row" $ do
      S.div ! className "label" $ text label
      comp

viewQueryState :: QueryState -> HTML Action
viewQueryState NothingYet = S.pre $ text ""
viewQueryState (CompletedSuccessfully r) = S.pre $ text (jsonStringify r)
viewQueryState (CompletedWithError e) = S.pre $ text e
viewQueryState Running = S.pre $ text "Wait..."


foreign import buttonClass :: ∀ props. ReactClass props -- for importing CSS

{-
Using a foreign React component

-- foreign import buttonClass :: ∀ props. ReactClass props
-- myButton = reactClassWithProps buttonClass "button"
-- myButton {} #! onClick (const Increment) $ text "Increment+"
-}