module App.Routes where

import Prelude
import Pux.Router as R
import Control.Alt ((<|>))
import Control.MonadZero (empty)
import Data.Array (intercalate, (:))
import Data.Maybe (fromMaybe)
import Pux.Router (end, router)

data Route = Home | Test String | Reports String String String String String String | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
  Home <$ end
  <|>
  Reports <$ (R.lit "report") <*> R.str <*> R.str <*> R.str <*> R.str <*> R.str <*> R.str
  <|>
  Test <$ (R.lit "test") <*> R.str -- <* end


toUrl :: Route -> String
toUrl Home = "/"
toUrl (Test s) = intercalate "/" ("test" : s : empty)
toUrl (Reports name timezone dateFrom dateTo filterStr breakdownStr) =
  "/" <> intercalate "/" ("report" : name : timezone : dateFrom : dateTo : filterStr : breakdownStr : empty)
toUrl NotFound = "/NOT_FOUND"