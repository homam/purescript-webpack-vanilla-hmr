module Server.Main where

import Data.Array as A
import Data.JSDate as JSDate
import Data.StrMap as M
import Database.Postgres as PG
import Node.Express.Response as Res
import Node.FS.Sync as FS
import Server.Utils.TTLCache as Cache
import Control.Monad.Aff (forkAff, joinFiber, launchAff_, runAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error, throw)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (snd)
import Global (readFloat)
import Node.Encoding (Encoding(..))
import Node.Express.App (get, listenHttp, setProp, use)
import Node.Express.Handler (Handler, HandlerM, next, nextThrow)
import Node.Express.Request (getOriginalUrl, getQueryParam, getRouteParam, setUserData)
import Node.Express.Response (send)
import Node.Express.Types (EXPRESS)
import Node.Process (lookupEnv)
import Prelude (bind, discard, map, pure, show, unit, void, ($), (*>), (-), (<$>), (<<<), (<>), (=<<), (==), (>>=))
import Query.Parser.TemplateParser (doTemplate)
import Server.Query (GetQueryState, KillQuery, QueryCache, QueryState(..), RunDbQuery, RunDbQueryAsync, getQueryState, killQuery, queryAsync)
import Server.Utils.Hash (toHash)
import Unsafe.Coerce (unsafeCoerce)


logger :: forall e a b b1 fb kb. AppState a b b1 fb kb -> Handler (console :: CONSOLE, ref :: REF | e)
logger (AppState state) = do
  url   <- getOriginalUrl
  cache <- liftEff $ readRef state.queryCache
  liftEff $ log (">>> " <> url <> " state = " <> show (A.length $ M.keys cache))
  setUserData "logged" url
  next

commonHeaders _ = do
  Res.setResponseHeader "Access-Control-Allow-Origin" "*"
  next

newtype AppState a b b1 fb kb = AppState {
    queryCache :: Ref (QueryCache a)
  , query :: forall c. RunDbQuery b c
  , queryAsync :: RunDbQueryAsync b
  , getQueryState :: GetQueryState b1 fb
  , killQuery :: KillQuery kb
  -- , waitOnCache :: Cache.WaitOnCache b1 (QueryState fb)
}

-- indexHandler :: forall e. Ref AppState -> Handler e
indexHandler _ = send "home"

-- apiReportStatusHandler :: ∀ fb a e. 
--     AppState a ( exception ∷ EXCEPTION , express ∷ EXPRESS , fs ∷ FS | e ) fb 
--   → HandlerM ( express ∷ EXPRESS , exception ∷ EXCEPTION , fs ∷ FS , console ∷ CONSOLE , db ∷ DB , ref ∷ REF | e ) Unit

apiReportStatusHandler (AppState state) = do
  qid <- reqRouteParam "qid"
  mqs <- liftEff $ state.getQueryState qid
  case mqs of
    Nothing -> send $ "No query was found for " <> qid
    Just qs -> 
      case qs of
        Running _ t -> do 
          now <- liftEff JSDate.now
          let diff = JSDate.getTime now - JSDate.getTime t
          send { status: "Running", runtime: diff }
        Done result -> send { status: "Done", result }
        Error e -> send { status: "Error", errorText: show e }
        Cancelled -> send { status: "Cancelled" }

apiKillReportHandler (AppState state) = do
  qid <- reqRouteParam "qid"
  mqs <- liftEff $ state.killQuery qid
  case mqs of
    Left e -> send e
    Right msg -> send msg

-- apiReportHandler :: ∀ fb a e. 
--     AppState a ( exception ∷ EXCEPTION , express ∷ EXPRESS , fs ∷ FS | e ) fb 
--   → HandlerM ( express ∷ EXPRESS , exception ∷ EXCEPTION , fs ∷ FS , console ∷ CONSOLE , db ∷ DB , ref ∷ REF | e ) Unit
apiReportHandler (AppState state) = do
  report <- reqRouteParam "report"
  breakdownStr <- reqRouteParam "breakdownStr"
  filtersStr <- reqRouteParam "filtersStr"
  timezone <- readFloat <$> reqRouteParam "timezone"
  dateFrom <- reqRouteParam "dateFrom"
  dateTo <- reqRouteParam "dateTo"
  text <- liftEff $ FS.readTextFile UTF8 $ "./server/Reports/" <> report <> "/template.sql"

  -- additional options
  isSync <- isTrue <$> getQueryParam "sync"
  returnSql <- isTrue <$> getQueryParam "sql"
  nocache <- isTrue <$> getQueryParam "nocache"
  
  case (
    doTemplate 
      filtersStr
      breakdownStr
      timezone
      dateFrom
      dateTo
      text
  ) of 
    Left e -> do 
      liftEff $ log $ show e
      Res.setContentType "text/plain"
      send $ show e
    (Right sqlTemplate) -> do 
      if returnSql 
        then Res.setContentType "text/plain" *> send sqlTemplate
        else do
          let hash = toHash sqlTemplate
          Res.setContentType "text/json"
          Res.setResponseHeader "x-qid" hash
          qs <- liftAff $ state.queryAsync nocache hash sqlTemplate
          if isSync 
            then case qs of
              Running fiber _ -> liftAff (joinFiber fiber) >>= send
              Done result -> send { status: "Done", result }
              Error e -> send { status: "Error", errorText: show e }
              Cancelled -> send { status: "Cancelled" }
            
            else send { qid: hash, status: show qs }
  where  
  isTrue = (\r -> r == Just "true")


routeParam :: ∀ r. String → HandlerM ( express ∷ EXPRESS | r ) (Either String String)
routeParam p = maybe (Left $ "Route Param " <> p <> " is missing.") Right <$> getRouteParam p

reqRouteParam :: ∀ r. String → HandlerM ( express ∷ EXPRESS | r ) String
reqRouteParam p = either (nextThrow <<< error) pure =<< routeParam p


-- appSetup :: forall e. Ref AppState -> App (ref :: REF, console :: CONSOLE | e)
appSetup state = do
  liftEff $ log "Setting up"
  setProp "json spaces" 2.0
  use               (logger            state)
  use               (commonHeaders     state)
  get "/"           (indexHandler      state)
  get "/api/:report/:timezone/:dateFrom/:dateTo/:filtersStr/:breakdownStr" (apiReportHandler state)
  get "/api/result/:qid" (apiReportStatusHandler state)
  get "/api/kill/:qid" (apiKillReportHandler state)


main = do
  connectionInfo <- lookupEnv "jewel_connection_string" >>= \ m -> case m of
    Just a -> pure $ PG.connectionInfoFromString a
    Nothing -> throw "Expected jewel_connection_string ENV variable."
  port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  runServer port connectionInfo
    
  where
    parseInt :: String -> Int
    parseInt str = fromMaybe 0 $ fromString str


runServer port databaseConfig = do
  cache <- newRef M.empty
  pool <- PG.mkPool databaseConfig
  let query = \q -> PG.withClient pool (PG.query_ (PG.Query q :: PG.Query Foreign))
  let queryAsync' = queryAsync cache pool 
  let getQueryState' = getQueryState cache
  let killQuery' = killQuery cache
  let waitOnCache' = Cache.waitOnCache cache
  -- ?help killQuery
  void $ runAff (\err -> log $ "Error in running server: " <> show err) do
    let state = AppState { 
          queryCache: cache
        , query: map unsafeCoerce query
        , queryAsync: queryAsync'
        , getQueryState: getQueryState'  
        , killQuery: killQuery'
        -- , waitOnCache: waitOnCache'
        }
    let app = appSetup state
    void $ liftEff $ listenHttp app port \_ -> 
      log $ "Server listening on :" <> show port

