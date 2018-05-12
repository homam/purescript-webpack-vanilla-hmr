module App.Components.QueryInput where

import Prelude (discard, pure, show, ($), (*>), (<<<), (<>), (==))
import Text.Smolder.Markup (text, (!), (#!))
import Pux.DOM.Events (DOMEvent, onChange, onKeyUp, targetValue)
import DOM.Event.Types (KeyboardEvent)
import App.Utils (consoleLog)
import DOM.Event.KeyboardEvent as KeyboardEvent
import Query.Types (class ToQueryPathString, toQueryPathString) as P
import Text.Smolder.HTML as S
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Pux (EffModel, noEffects)
import Pux.DOM.HTML (HTML)
import Query.Types (class ToQueryPathString)
import Text.Parsing.Parser (ParseError)
import Text.Smolder.HTML.Attributes (className, title, type', value)
import Unsafe.Coerce (unsafeCoerce)

data Action = QueryTextChanged DOMEvent | QueryTextKeyUp KeyboardEvent -- | QueryParserError ParseError

type Parser a = String -> Either ParseError a

type State a = { value :: String, parser :: Parser a, result :: Maybe (Either ParseError a) }

initialState :: forall a. {value :: String, parser :: Parser a } -> State a
initialState {value, parser} = { value, parser, result: Nothing }

type MyEffects e = (console :: CONSOLE | e)

update :: forall e a. ToQueryPathString a => Action -> State a -> EffModel (State a) Action (MyEffects e)
update (QueryTextChanged ev) state = noEffects $ state { value = targetValue ev }
update (QueryTextKeyUp ev) state = 
  if "Enter" == KeyboardEvent.key ev
    then 
      case state.parser state.value of
        Left e -> { 
            state: state { result = Just (Left e) }
          , effects: [liftEff (consoleLog e) *> pure Nothing]
          }
        Right r -> {
            state: state { result = Just (Right r)}
          , effects: [liftEff $ consoleLog r *> consoleLog (P.toQueryPathString r) *> pure Nothing]
        }
    else noEffects state


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
