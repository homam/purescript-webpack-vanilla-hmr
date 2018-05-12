module Server.Utils.TTLCache where

import Prelude

import Control.Monad.Aff (Aff, Milliseconds(Milliseconds), delay, forkAff, launchAff, launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref as R
import Control.Monad.Eff.Timer as T
import Data.Int (toNumber)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Data.Tuple (Tuple(..), fst, snd)


type Cache a = M.StrMap (Tuple Number a)

mkEmptyCache :: forall a e. Eff (ref :: R.REF | e ) (R.Ref (Cache a))
mkEmptyCache = R.newRef M.empty


withCachedItem_ cache key f = do
  mItem <- liftEff ((M.lookup key) <$> R.readRef cache )
  case mItem of 
    Nothing -> do 
      pure unit
    Just item' -> f item'

withCachedItem cache key f = do
  mItem <- liftEff ((M.lookup key) <$> R.readRef cache )
  case mItem of 
    Nothing -> pure Nothing
    Just item' -> Just <$> f item'

time = JSDate.getTime <$> JSDate.now

wait :: ∀ u e. Number → Aff e u → Aff e u
wait n f = delay (Milliseconds n) *> f

wait' :: ∀ e. Int → Eff ( timer ∷ T.TIMER | e ) Unit → Eff ( timer ∷ T.TIMER | e ) T.TimeoutId
wait' n = T.setTimeout n

checkExpiry :: ∀ val e. R.Ref (M.StrMap (Tuple Number val)) → String → Eff ( now ∷ NOW , ref ∷ R.REF | e ) Unit
checkExpiry cache key = 
  withCachedItem_ cache key (\ (Tuple exp _) -> do
    now <- time
    let b = now - exp
    if b >= (toNumber 0)
      then void $ R.modifyRef cache (M.delete key) 
      else void $ launchAff $ forkAff $ wait b (liftEff $ checkExpiry cache key)
  )

extendCache :: ∀ e val. R.Ref (M.StrMap (Tuple Number val)) → String → Int → Eff ( ref ∷ R.REF , now ∷ NOW | e ) Unit
extendCache cache key ttl = do 
  withCachedItem_ cache key (\ (Tuple exp val)  -> do
    R.modifyRef cache (M.insert key (Tuple (exp + toNumber ttl) val))
    scheduleCleanup cache ttl key
  )

addCache :: ∀ e val. R.Ref (M.StrMap (Tuple Number val)) → Int → String → val → Eff ( now ∷ NOW , ref ∷ R.REF | e ) Unit
addCache cache ttl key val = do
  now <- time
  let item = Tuple (now + (toNumber ttl)) val
  R.modifyRef cache (M.insert key item)
  scheduleCleanup cache ttl key


updateCache :: ∀ e val. R.Ref (M.StrMap (Tuple Number val)) → String → val → Eff ( ref ∷ R.REF | e ) Unit
updateCache cache key val = withCachedItem_ cache key (\ (Tuple exp v) ->
  R.modifyRef cache (M.update (const $ Just $ Tuple exp val) key)
)

getCache :: ∀ val e m ttl. Bind m ⇒ MonadEff ( ref :: R.REF | e ) m ⇒ R.Ref (M.StrMap (Tuple ttl val)) → String → m (Maybe val)
getCache cache key = withCachedItem cache key (\ (Tuple exp val)  -> pure val)


scheduleCleanup :: ∀ val e. R.Ref (M.StrMap (Tuple Number val)) → Int → String → Eff ( ref ∷ R.REF , now ∷ NOW | e ) Unit
scheduleCleanup cache ttl key =
  launchAff_ $ forkAff $ 
    void $ wait (toNumber $ ttl + 1000) $ do
      mItem <- liftEff ((M.lookup key) <$> R.readRef cache )
      case mItem of 
        Nothing -> pure unit
        Just item' -> do
          now' <- liftEff time
          let b = now' - fst item'
          if b >= (toNumber 0)
            then liftEff $ void $ R.modifyRef cache (M.delete key) 
            else liftEff $ pure unit


type WaitOnCache m e val = String → Number → (Maybe val → m ( ref ∷ R.REF | e ) Boolean) → m ( ref ∷ R.REF | e ) Unit

-- waitOnCache :: ∀ val e m. Monad m => MonadEff e m => R.Ref (M.StrMap (Tuple Number val)) → WaitOnCache m e val
-- waitOnCache :: ∀ t83 t94 t97 m. MonadEff t83 m => R.Ref (M.StrMap (Tuple t97 t94)) → String → Number → (Maybe t94 → m ( ref ∷ R.REF | t83 ) Boolean ) → m ( ref ∷ R.REF | t83 ) Unit
waitOnCache cache key interval callback = 
  launchAff_ $ forkAff $ void $ wait interval $ do
    mItem <- liftEff ((M.lookup key) <$> R.readRef cache )
    b <- liftEff $ callback (snd <$> mItem) 
    if b
      then pure unit
      else liftEff $ waitOnCache cache key interval callback
