module App.Routes where

import Prelude
import Pux.Router as R
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>), (<*>))
import Data.Function (($))
import Data.Functor ((<$), (<$>))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Pux.Router (end, int, lit, num, param, router, str)

data Route = Home | Test String | Reports String String String String String String | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
  Home <$ end
  <|>
  Reports <$ (R.lit "report") <*> R.str <*> R.str <*> R.str <*> R.str <*> R.str <*> R.str
  <|>
  Test <$ (R.lit "test") <*> R.str -- <* end
  -- <|>
  -- Reports "name" <$ (lit "report" ) <* end
  -- <|>
  -- User <$> (lit "report"  *> int) <* end

  --  *> lit "timezone" <*> lit "dateFrom" <*> lit "dateTo" <*> lit "filter" <*> lit "breakdown"