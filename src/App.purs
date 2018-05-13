module App.App where
  
import Prelude
import App.Layout as Layout
import App.LayoutTypes as LayoutTypes
import Data.Array as A
import App.Effects (AppEffects)
import App.Routes (Route(..), match, toUrl)
import App.Utils.RouteValue (RouteValue(..), fromRouteValue)
import CSS.Elements (s)
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind ((=<<), bind)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Function (($))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Semigroup ((<>))
import Data.Show (show)
import Debug.Trace (traceAny)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, mapEffects, mapState, noEffects)
import Pux.DOM.Events (DOMEvent, onClick)
import Pux.DOM.HTML (HTML, child)
import Text.Smolder.HTML (a, div, h1, li, nav, ul)
import Text.Smolder.HTML.Attributes (href)
import Text.Smolder.Markup ((!), (#!), text)


data Event = PageView Route | Navigate String (Maybe DOMEvent) | LayoutAction LayoutTypes.Action | Noop

type State = { currentRoute :: Route, layoutState :: LayoutTypes.State }

initialState :: State
initialState = { currentRoute: Home, layoutState : Layout.initialState }

foldp :: Event -> State -> EffModel State Event AppEffects
foldp (Navigate url ev) st =
  { state: st
  , effects: [
      liftEff do
        maybe (pure unit) preventDefault ev
        h <- history =<< window
        pushState (toForeign {}) (DocumentTitle "") (URL url) h
        pure $ Just $ PageView (match url)
    ]
  }
foldp (PageView route) st = noEffects $ st { currentRoute = route, layoutState = toLayoutState route st.layoutState }
foldp (LayoutAction ev) st = 
  traceAny {component: "App/foldp (LayoutAction)", ev, st} $ const 
                let {state, effects} = 
                                        Layout.update ev st.layoutState 
                                          # mapEffects LayoutAction # mapState (\s ->  traceAny {component: "App/foldp (mapState)", s} $ const $ st { layoutState = toLayoutState st.currentRoute s }  )
                in if ev == LayoutTypes.Query
                  then {state, effects: A.cons (pure $ Just $ Navigate (toUrl toRoute) Nothing) effects } 
                  else {state, effects} 
  where
  s = st.layoutState
  toRoute = Reports "Sessions" s.timezone s.dateFrom s.dateTo (fromRouteValue "" s.filterStr) (fromRouteValue "" s.breakdownStr)

foldp Noop st = noEffects st

toLayoutState ::  Route -> LayoutTypes.State -> LayoutTypes.State
toLayoutState (Reports report timezone dateFrom dateTo filterStr breakdownStr) layoutState = 
  layoutState { 
      timezone = timezone 
    , dateFrom = dateFrom
    , dateTo = dateTo
    , filterQueryInput = layoutState.filterQueryInput { value = fromRouteValue "" $ FromRoute filterStr <|> layoutState.filterStr  }
    -- , breakdownQueryInput = layoutState.breakdownQueryInput { value = breakdownStr }
    , filterStr = FromRoute filterStr <|> layoutState.filterStr 
    , breakdownStr = FromRoute breakdownStr <|>  layoutState.breakdownStr 
  }
toLayoutState _ s = s

view :: State -> HTML Event
view state =
    page state state.currentRoute

page :: State -> Route -> HTML Event
page state Home = div $ do
    h1 $ text "Home" 
    child LayoutAction Layout.view $ state.layoutState
page _ (Test p) = h1 $ text "Test"
page state (Reports report timezone dateFrom dateTo filterStr breakdownStr) = do
  div $ do
    h1 $ text "Layout" 
    child LayoutAction Layout.view $ toLayoutState state.currentRoute state.layoutState
page _ NotFound = h1 $ text "Not Found!"