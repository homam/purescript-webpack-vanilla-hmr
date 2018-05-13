module App.App where
  
import Prelude
import App.Layout as Layout
import App.LayoutTypes as LayoutTypes
import Data.Array as A
import App.Utils.RouteValue (RouteValue(..))
import App.Routes (Route(..), match)
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
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, mapEffects, mapState, noEffects)
import Pux.DOM.Events (DOMEvent, onClick)
import Pux.DOM.HTML (HTML, child)
import Text.Smolder.HTML (a, div, h1, li, nav, ul)
import Text.Smolder.HTML.Attributes (href)
import Text.Smolder.Markup ((!), (#!), text)
import App.Effects (AppEffects)


data Event = PageView Route | Navigate String (Maybe DOMEvent) | LayoutAction LayoutTypes.Action

type State = { currentRoute :: Route, counterState :: LayoutTypes.State }

initialState :: State
initialState = { currentRoute: Home, counterState : Layout.initialState }

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
foldp (PageView route) st = noEffects $ st { currentRoute = route }
foldp (LayoutAction ev) st = 
  let {state, effects} = 
                          Layout.update ev st.counterState 
                            # mapEffects LayoutAction # mapState (\s -> st { counterState = s })
  in if ev == LayoutTypes.Query
    then {state, effects: A.cons (pure $ Just $ Navigate "/" Nothing) effects } 
    else {state, effects} 

view :: State -> HTML Event
view state =
    page state state.currentRoute

page :: State -> Route -> HTML Event
page state Home = div $ do
    h1 $ text "Home" 
    child LayoutAction Layout.view $ state.counterState
page _ (Test p) = h1 $ text "Test"
page state (Reports report timezone dateFrom dateTo filterStr breakdownStr) = do
  div $ do
    h1 $ text "Layout" 
    child LayoutAction Layout.view $ state.counterState { 
        timezone = timezone 
      , dateFrom = dateFrom
      , dateTo = dateTo
      , filterQueryInput = state.counterState.filterQueryInput { value = filterStr }
      , breakdownQueryInput = state.counterState.breakdownQueryInput { value = breakdownStr }
      , filterStr = FromRoute filterStr <|> state.counterState.filterStr 
      , breakdownStr = FromRoute breakdownStr <|>  state.counterState.breakdownStr 
      }
page _ NotFound = h1 $ text "Not Found!"