module App.Components.QueryInput where

import DOM.Event.KeyboardEvent as KeyboardEvent
import Text.Smolder.HTML as S
import App.Utils (consoleLog)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import DOM.Event.Types (KeyboardEvent, keyboardEventToEvent)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Prelude (class Eq, discard, pure, show, ($), (*>), (<<<), (<>), (==))
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent, onChange, onKeyUp, targetValue)
import Pux.DOM.HTML (HTML)
import Query.Types (class ToQueryPathString, toQueryPathString) as P
import Query.Types (class ToQueryPathString)
import Text.Parsing.Parser (ParseError)
import Text.Smolder.HTML.Attributes (className, title, type', value)
import Text.Smolder.Markup (text, (!), (#!))
import Unsafe.Coerce (unsafeCoerce)

data Action = QueryTextChanged DOMEvent | QueryTextKeyUp KeyboardEvent 

instance eqAction :: Eq Action where
  eq (QueryTextChanged _) (QueryTextChanged _ ) = true
  eq (QueryTextKeyUp _) (QueryTextKeyUp _ ) = true
  eq _ _ = false

type Parser a = String -> Either ParseError a

type State a = { value :: String, parser :: Parser a, result :: Maybe (Either ParseError a) }

initialState :: forall a. {value :: String, parser :: Parser a } -> State a
initialState {value, parser} = { value, parser, result: Nothing }

type MyEffects e = (console :: CONSOLE | e)

update :: forall e a. ToQueryPathString a => Action -> State a -> EffModel (State a) Action (MyEffects e)
update (QueryTextChanged ev) state = { state: state { value = targetValue ev }, effects: [pure Nothing] }
update (QueryTextKeyUp ev) state = 
  if "Enter" == KeyboardEvent.key ev
    then 
      case state.parser state.value of
        Left e -> { 
            state: state { value = value, result = Just (Left e) }
          , effects: [liftEff (consoleLog e) *> pure Nothing]
          }
        Right r -> {
            state: state { value = value, result = Just (Right r)}
          , effects: [liftEff $ consoleLog r *> consoleLog (P.toQueryPathString r) *> pure Nothing]
        }
    else  {state: state { value = value }, effects: [] }
  where
    value = targetValue $ keyboardEventToEvent ev


view :: forall a. P.ToQueryPathString a => State a -> HTML Action
view state =
  S.div do
    S.input ! type' "text" ! value state.value #! onChange QueryTextChanged #! onKeyUp (QueryTextKeyUp <<< unsafeCoerce)
    S.span ! className ("error-indicator " <> resultToClassName state.result) ! title (showResult state.result) $ text ""

  where
    resultToClassName Nothing = "hidden"
    resultToClassName (Just (Left e)) = "error"
    resultToClassName (Just (Right r)) = "ok"

    showResult :: Maybe (Either ParseError a) -> String
    showResult Nothing = ""
    showResult (Just (Left e)) = show e
    showResult (Just (Right r)) = P.toQueryPathString r
