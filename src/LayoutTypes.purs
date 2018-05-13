module App.LayoutTypes where

import App.Components.QueryInput as QueryInput
import App.Utils.RouteValue (RouteValue)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.Generic (class Generic)
import Prelude (class Eq, (&&), (==))
import Query.Types (Breakdown, Filters)

data QueryInputType = FilterQueryInputType | BreakdownQueryInputType
derive instance genericQueryInputType :: Generic QueryInputType
derive instance eqQueryInputType :: Eq QueryInputType 

data QueryState = NothingYet | Running | CompletedSuccessfully Foreign | CompletedWithError String

data Action = QueryInputAction QueryInputType QueryInput.Action | Query | Result (Either Error Foreign)

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