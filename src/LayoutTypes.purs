module App.LayoutTypes where

import Prelude (class Eq, (&&), (==))
import Query.Types (Breakdown, Filters)
import App.Components.QueryInput as QueryInput
import App.Utils.RouteValue (RouteValue) 
import Data.Foreign (Foreign)
import Data.Generic (class Generic)

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