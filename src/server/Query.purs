module Server.Query where

import Data.JSDate as JSDate
import Database.Postgres as PG
import Server.Utils.TTLCache as C
import Control.Monad.Aff (Aff, Fiber, error, forkAff, killFiber, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF, Ref)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Prelude (class Show, bind, discard, not, pure, show, void, ($), (&&), (<>), (||))

type QueryCache a = C.Cache (QueryState a)

data QueryState e = Running ( Fiber (FiberEffect e) (Array Foreign)) JSDate.JSDate | Error String | Cancelled | Done (Array Foreign)

isDone :: ∀ a. QueryState a → Boolean
isDone (Done _) = true
isDone _ = false

isRunning :: ∀ a. QueryState a → Boolean
isRunning (Running _ _) = true
isRunning _ = false

instance showQueryState ::  Show (QueryState a) where
  show (Running a b) = "Running"
  show (Error e) = "Error " <> e
  show Cancelled = "Cancelled"
  show (Done as) = "Done"

type FiberEffect e = (db :: PG.DB, console :: CONSOLE, ref :: REF, now :: NOW | e)

type RunDbQuery b c =  
    String
  -> Aff ( db :: PG.DB | b ) c
    
type RunDbQueryAsync e = 
     Boolean
  -> String
  -> String 
  → Aff (FiberEffect e) (QueryState e) --(Fiber (FiberEffect e) (Array Foreign) )

type KillQuery e = 
    String 
  → Eff (FiberEffect e) (Either String String)

type GetQueryState e fb = 
    String 
  → Eff (ref :: REF | e) (Maybe (QueryState fb))

queryAsync :: ∀ e. 
    Ref (C.Cache (QueryState e))
  → PG.Pool 
  → RunDbQueryAsync e
queryAsync cache pool nocache qid q = do
  mcached <- C.getCache cache qid
  case mcached of 
    Just cached -> 
      if not nocache && (isDone cached || isRunning cached)
        then pure cached
        else go
    Nothing -> go

  where 
  go = do
    let myAff = PG.withClient pool (PG.query_ (PG.Query q :: PG.Query Foreign))

    fiber <- forkAff $ do
      results <- myAff
      liftEff $ log $ "Done " <> qid
      liftEff $  C.updateCache cache qid (Done results)
      pure results

    now <- liftEff JSDate.now
    let st = Running fiber now
    liftEff $ C.addCache cache 600000 qid st
    pure st


getQueryState :: ∀ a e.Ref (C.Cache (QueryState a)) → String → Eff ( ref ∷ REF | e ) (Maybe (QueryState a))
getQueryState cache qid = C.getCache cache qid 

killQuery :: ∀ a. Ref (C.Cache (QueryState a)) → KillQuery a
killQuery cache qid = do
  mqs <-  getQueryState cache qid
  case mqs of
    Nothing -> pure $ Left $ "No query was found for " <> qid
    Just qs -> case qs of
      Running fiber _ -> do
        void $ launchAff $ killFiber (error "User Cancelled") fiber
        C.updateCache cache qid Cancelled
        pure $ Right "Killed"
      x -> pure $ Left $ "Invalid Query State: " <> show x