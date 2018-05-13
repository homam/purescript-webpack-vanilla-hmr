module App.Counter where

import Prelude
import Text.Smolder.Markup
import Pux.DOM.Events
import DOM.Event.Types
import App.Utils
import App.Components.QueryInput as QueryInput
import DOM.Event.KeyboardEvent as KeyboardEvent
import Query.Parser.UrlQueryParser as P
import Query.Parser.UrlQueryParser as P
import Query.Types as P
import Text.Smolder.HTML as S
import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Aff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.MonadZero (empty)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HISTORY)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign)
import Data.Foreign.Generic (encodeJSON)
import Data.Function (const, ($), (#))
import Data.Generic (class Generic)
import Data.HTTP.Method (Method(..))
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Traversable (intercalate)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest, get)
import Pux (App, CoreEffects, EffModel, mapEffects, mapState, noEffects, start)
import Pux.DOM.HTML (HTML, child)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (reactClass, reactClassWithProps, renderToReact)
import Query.Types (class ToQueryPathString, Breakdown, Filters)
import React (ReactClass)
import Text.Smolder.HTML.Attributes (className, name, type', value)
import App.RouteValue
import Data.Array as A

data QueryInputType = FilterQueryInputType | BreakdownQueryInputType
derive instance genericQueryInputType :: Generic QueryInputType
derive instance eqQueryInputType :: Eq QueryInputType 

data QueryState = NothingYet | Running | CompletedSuccessfully Foreign | CompletedWithError String
data Action = QueryInputAction QueryInputType QueryInput.Action | Query | Result Foreign  

instance eqAction :: Eq Action where
  eq (QueryInputAction t1 a1) (QueryInputAction t2 a2) = t1 == t2 && a1 == a2
  eq Query Query = true
  eq (Result _) (Result _) = true
  eq _ _ = false

type State = { 
    filterQueryInput :: QueryInput.State Filters
  , breakdownQueryInput :: QueryInput.State Breakdown
  , filterStr :: RouteValue String
  , breakdownStr :: RouteValue String
  , dateFrom :: String
  , dateTo :: String
  , timezone :: String
  , result :: QueryState
}

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

type MyEffects = (ajax :: AJAX, console :: CONSOLE, history :: HISTORY, dom :: DOM)

update :: Action -> State ->  EffModel State Action MyEffects
update (QueryInputAction ty ev) state = 
  let tup = 
          case ty of
            FilterQueryInputType ->  handle state.filterQueryInput (\s -> state { filterQueryInput = s, filterStr = FromComponent s.value })
            BreakdownQueryInputType -> handle state.breakdownQueryInput (\s -> state { breakdownQueryInput = s, breakdownStr = FromComponent s.value })
  in {state: tup.state, effects: tup.effects} -- A.cons (liftEff (consoleLog state *> consoleLog ev *> consoleLog tup.state) *> pure Nothing) tup.effects}

  where
  handle :: forall a. ToQueryPathString a => QueryInput.State a
  → (QueryInput.State a → State ) 
  → EffModel State Action MyEffects
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
  where
    slash x = x <> "/"

foreign import buttonClass :: ∀ props. ReactClass props

-- myButton :: ∀ ev. HTML ev
myButton = reactClassWithProps buttonClass "button"

view :: State -> HTML Action
view state =
  S.div do
    S.h1 $ text "PureScript + Vanilla HMR"
      -- myButton {} #! onClick (const Increment) $ text "Increment+"
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

row label comp = 
  S.div ! className "row" $ do
    S.div ! className "label" $ text label
    comp

viewQueryState NothingYet = S.pre $ text ""
viewQueryState (CompletedSuccessfully r) = S.pre $ text (jsonStringify r)
viewQueryState (CompletedWithError e) = S.pre $ text e
viewQueryState Running = S.pre $ text "Wait..."


-- type WebApp = App (DOMEvent -> Action) Action State
-- toReact :: forall props . State -> Eff ( CoreEffects MyEffects) {component :: (ReactClass props), app:: WebApp}
-- toReact state = do
--   urlSignal <- sampleURL =<< window
--   let routeSignal = urlSignal ~> match
--   app <- start
--     { initialState: state
--     , foldp: update
--     , view
--     , inputs: [routeSignal]
--     }
--   renderer <- renderToReact app.markup app.input 
--   pure $ { component: renderer, app: app }
