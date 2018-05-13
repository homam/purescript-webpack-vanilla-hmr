module App.Layout where

import App.Components.QueryInput as QueryInput
import Text.Smolder.HTML as S
import App.Effects (AppEffects)
import App.LayoutTypes (Action(..), QueryInputType(..), QueryState(..), State)
import App.Utils (consoleLog, jsonStringify)
import App.Utils.RouteValue (RouteValue(..), fromRouteValue)
import Control.Monad.Eff.Class (liftEff)
import Control.MonadZero (empty)
import Data.Either (Either(Left))
import Data.Foreign (Foreign)
import Data.HTTP.Method (Method(..))
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Traversable (intercalate)
import Debug.Trace (traceAny)
import Network.HTTP.Affjax (affjax, defaultRequest)
import Prelude (bind, const, discard, pure, (#), ($), (*>), (<>))
import Pux (EffModel, mapEffects, mapState, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML, child)
import Query.Parser.UrlQueryParser (runBreakdownParser, runFilterParser) as P
import React (ReactClass)
import Text.Smolder.HTML.Attributes (className, type', value)
import Text.Smolder.Markup (text, (!), (#!))

initialState ::   State
initialState = {
    filterQueryInput: (QueryInput.initialState P.runFilterParser) -- { value = filterStr }
  , breakdownQueryInput: (QueryInput.initialState P.runBreakdownParser) -- { value = breakdownStr }
  , filterStr: NotInitialized
  , breakdownStr: NotInitialized
  , dateFrom: "2018-05-09"
  , dateTo: "2018-05-15"
  , timezone: "2"
  , result: NothingYet
}

update :: Action -> State ->  EffModel State Action AppEffects
update (QueryInputAction ty ev) state = 
        case ty of
          FilterQueryInputType -> 
            QueryInput.update ev state.filterQueryInput
              # mapEffects (QueryInputAction ty) # mapState (\s -> state { filterStr = FromComponent s.value, filterQueryInput = s })
          BreakdownQueryInputType -> 
            QueryInput.update ev state.breakdownQueryInput
              # mapEffects (QueryInputAction ty) # mapState (\s -> state { breakdownStr = FromComponent s.value, breakdownQueryInput = s })

update (Result result) state = noEffects $ state { result = CompletedSuccessfully result }
update Query state' = traceAny {component: "Layout/update", state'} $ const {
  state: state { result = Running },
  effects: [
    do liftEff (consoleLog "-----\nLayout" *> consoleLog state' *> consoleLog state) *> pure Nothing
    ,
    do
      let url = "http://localhost:8080/api/" 
              <>  intercalate "/" ("Sessions" : state.timezone :  state.dateFrom : state.dateTo : fromRouteValue "" state.filterStr : fromRouteValue "" state.breakdownStr : empty) 
              <> "?sync=true"
      res <- affjax $ defaultRequest { url = url, method = Left GET }
      liftEff $ consoleLog (res.response :: Foreign)
      pure $ Just (Result res.response)
  ]
  }
  where
    state = state' { result = Running }

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