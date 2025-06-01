module SF where

--------------------------------------------------------------------------------
import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative
import Control.Arrow
import Debug.Trace
import Data.Bool
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Bifunctor as Bifunctor

import Util
--------------------------------------------------------------------------------

type Time = Double

newtype a ~> b = SF { unSF :: (Time, a) -> (b, a ~> b) }

instance Functor ((~>) a) where
  fmap f (SF sf) = SF $ \dta -> let (b, sf') = sf dta in (f b, fmap f sf')

instance Applicative ((~>) c) where
  pure x = SF (const (x, pure x))
  SF ff <*> SF fa = SF $ \dtc ->
    let (f, sff') = ff dtc
        (a, fa') = fa dtc
    in (f a, sff' <*> fa')

instance Category (~>) where
  id = SF $ \(_,a) -> (a, id)
  SF sg . SF sf = SF $ \(dt,a) ->
    let (b, sf') = sf (dt,a)
        (c, sg') = sg (dt,b)
    in (c, sg' . sf')

instance Arrow (~>) where
  arr f = SF $ \(_,b) -> (f b, arr f)
  first (SF sf) = SF $ \(dt,(b,d)) ->
    let (c,sf') = sf (dt,b)
    in ((c,d), first sf')
  -- TODO other implementations

instance ArrowChoice (~>) where
  -- left :: (b ~> c) -> Either b d ~> Either c d
  left (SF sf) = SF k
    where
      k (dt, Left  b) = let (c, sf') = sf (dt,b) in (Left c, left sf')
      k (_ , Right d) = (Right d, left (SF sf))

instance ArrowLoop (~>) where
  -- loop :: ((b,d) ~> (c,d)) -> (b~>c)
  loop (SF sf) = SF $ \(dt,b) ->
    let ((c,d), sf') = sf (dt,(b,d))
    in (c, loop sf')

updateSF :: Time -> i -> (o, i ~> o) -> (o, i ~> o)
updateSF dt input (_, SF sf) = (out, sf')
  where (out, sf') = sf (dt,input)

timeDelta :: () ~> Double
timeDelta = SF $ \(dt,_) -> (dt, timeDelta)

sfTrace :: Show a => a ~> a
sfTrace = arr (\a -> traceShow a a)

-- State
--------
delay :: a -> (a ~> a)
delay a0 = SF $ \(_,a) -> (a0, delay a)

-- | Delays the stream 0,1,2,3... to 0,0,1,2,3,...
delayedDelay :: a ~> a
delayedDelay = SF $ \(_,a) -> (a, delay a)

-- delayMany :: [b] -> (a ~> b) -> (a ~> b)
-- delayMany bs sf = foldr delay sf bs

stepper :: a -> (a -> a) -> (b ~> a)
stepper s0 update = SF $ const $ let s1 = update s0 in (s1, stepper s1 update)

setter :: a -> Maybe a ~> a
setter a0 = stateful' a0 update
  where update Nothing  a = a
        update (Just a) _ = a

potentiallyUninitializedSetter :: forall a. Maybe a -> (Maybe a ~> Maybe a)
potentiallyUninitializedSetter a0 = fmap Just ^>> setter a0


  -- proc a -> do
  -- setter a0 -< fmap Just a

  -- fmap Just >>> (setter a0 :: Maybe (Maybe a) ~> Maybe a)

stateful :: s -> (Time -> i -> s -> s) -> (i ~> s)
stateful a0 update = SF sf
  where sf (dt,b) = let a1 = update dt b a0 in (a1, stateful a1 update)

stateful' :: s -> (i -> s -> s) -> (i ~> s)
stateful' a0 update = stateful a0 (const update)

stateWithReset :: a -> (Time -> b -> a -> a) -> ((b, Maybe a) ~> a)
stateWithReset a0 update = SF sf
  where
    sf (dt, (b,Nothing)) = let a1 = update dt b a0 in (a1, stateWithReset a1 update)
    sf (_ , (_,Just a )) = (a, stateWithReset a update)

risingEdge :: Bool ~> Bool
risingEdge = (&&) <$> fmap not (delay False) <*> id

-- Streams
----------
fromInfiniteList :: [b] -> (a ~> b)
fromInfiniteList bs = head <$> stepper bs tail

sfCycle :: [b] -> (a ~> b)
sfCycle bs = head <$> stepper (cycle bs) tail

slideShow :: Time -> [b] -> (a ~> b)
slideShow timeDelay bs = head . fst <$> stateful (cycle bs, 0) step
  where step dt _ (b:bs, t) = let t' = t + dt
                              in if t' > timeDelay
                                 then (bs, t' - timeDelay)
                                 else (b:bs, t')

snack :: [b] -> (Bool ~> Maybe b)
snack init = proc next -> do
  rec
    dbs <- delay init -< bs
    let bs | next     = tail dbs
           | not next = dbs

  returnA -< if next then Just (head dbs) else Nothing

-- Switches
-----------
type Event = Maybe

switch :: (select -> (a ~> b)) -> (a ~> (Event select, b)) -> (a ~> b)
switch select (SF sf) = SF $ \dta -> case sf dta of
  ((Just  s, _), _  ) -> unSF (select s) dta
  ((Nothing, b), sf') -> (b, switch select sf')

switch' :: (a ~> (Event (a ~> b), b)) -> (a ~> b)
switch' (SF sf) = SF $ \dta -> case sf dta of
  ((Just sf', _), _  ) -> unSF sf' dta
  ((Nothing, b ), sf') -> (b, switch' sf')

untilNothing :: (a ~> Event b) -> (a ~> b) -> (a ~> b)
untilNothing (SF sf1) (SF sf2) = SF $ \dta -> case sf1 dta of
  (Just b , sf') -> (b, untilNothing sf' (SF sf2))
  (Nothing, _  ) -> sf2 dta

rSwitch :: (select -> (a ~> (Event select, b))) -> (a ~> (Event select, b)) -> (a ~> b)
rSwitch select (SF sf) = SF $ \dta -> case sf dta of
  ((Just s , _), _  ) -> unSF (rSwitch select (select s >>> first notYet)) dta
  ((Nothing, b), sf') -> (b, rSwitch select sf')

installEventSF :: (a ~> b) -> ((Event (a ~> b), a) ~> b)
installEventSF (SF sf) = SF $ \(dt, (event, a)) -> case event of
  Just (SF nextSF) -> let (b, nextSF') = nextSF (dt,a) in (b, installEventSF nextSF')
  Nothing          -> let (b, sf') = sf (dt,a) in (b, installEventSF sf')

-- erSwitch :: (select -> ((Event select, a) ~> b)) -> ((Event select, a) ~> b) -> ((Event select, a) ~> b)
-- erSwitch select (SF sf) = SF $ \(dt, (event, a)) -> case event of
--   Just event -> unSF (erSwitch select (select event)) (dt, (Nothing, a))
--   Nothing    -> let (b, sf') = sf (dt, a) in (b, erSwitch select sf')

newtype Mode i o = Mode { unMode :: i ~> (Event (Mode i o), o) }

runMode :: Mode i o -> (i ~> o)
runMode (Mode sf) = rSwitch unMode sf

updateOnJust :: s -> (s -> e -> s) -> (Event e ~> s)
updateOnJust b0 f = SF $ \case
  (_, Just a ) -> let b1 = f b0 a in (b1, updateOnJust b1 f)
  (_, Nothing) -> (b0, updateOnJust b0 f)

mkMode :: (i ~> o) -> (Mode (i,Bool) o -> Mode (i,Bool) o)
mkMode sf nextMode = Mode $ proc (i, next) -> do
  o <- sf -< i
  let event = sample next nextMode
  returnA -< (event, o)

chainedSwitch :: [i ~> o] -> ((i,Bool) ~> o)
chainedSwitch (sf:sfs) = switch (const (chainedSwitch sfs)) $ proc (i,next) -> do
  o <- sf -< i
  flipSwitch <- notYet -< if next then Just () else Nothing
  returnA -< (flipSwitch, o)

cycleSwitch :: (i ~> o) -> (i ~> o) -> ((i,Bool) ~> o)
cycleSwitch (SF sf1) (SF sf2) = SF $ \case
  (dt, (i, True))  -> let (o, sf2') = sf2 (dt,i) in (o, cycleSwitch sf2' (SF sf1))
  (dt, (i, False)) -> let (o, sf1') = sf1 (dt,i) in (o, cycleSwitch sf1' (SF sf2))

count :: (a -> Bool) -> (a ~> Int)
count pred = (bool 0 1 . pred) ^>> stateful' 0 (+)

countEvents :: Event a ~> Int
countEvents = length ^>> stateful' 0 (+)

-- Collections
--------------
col :: Functor f => f (a ~> b) -> (a ~> f b)
col sfs = SF $ \dta -> let fsf = fmap (\(SF sf) -> sf dta) sfs
                       in (fmap fst fsf, col (fmap snd fsf))

sfMap :: forall id i o. Ord id => ([id], [(id, i ~> o)], i) ~> Map id o
sfMap = fmap fst <$> stateful Map.empty step
  where
    advance :: Time -> i -> (i ~> o) -> (o, i ~> o)
    advance = curry (flip unSF)

    step :: Time -> ([id], [(id, i ~> o)], i) -> Map id (o, i ~> o) -> Map id (o, i ~> o)
    step dt (toDelete, newSFs, input) state = state
      |>  mapDeleteMany toDelete
      >>> fmap (advance dt input . snd)
      >>> mapInsertMany [(id, advance dt input sf) | (id, sf) <- newSFs]

-- | Like sfMap, but only updates those signal functions that have an input
--   directly addressed to them.
sparseUpdater :: forall id i o. Ord id
              => Map id (o,i~>o)
              -> ([id], [(id, (o,i~>o))], [(id,i)]) ~> Map id o
sparseUpdater map0 = fmap fst <$> stateful map0 step
  where
    applyInput :: Time -> i -> (o, i ~> o) -> (o, i ~> o)
    applyInput dt i (_, SF sf) = sf (dt,i)

    step :: Time -> ([id], [(id, (o,i~>o))], [(id,i)]) -> Map id (o, i ~> o) -> Map id (o, i ~> o)
    step dt (toDelete, newSFs, addressedInputs) state = state
      |>  mapDeleteMany toDelete
      >>> mapInsertMany newSFs
      >>> mapAdjustMany (map (Bifunctor.second (applyInput dt)) addressedInputs)

-- | Keeps track of an ID map, applying deletes and overwrite-updates on each tick.
applyMapUpdatesAndDeletesSF :: Ord id => ([id], [(id, a)]) ~> Map id a
applyMapUpdatesAndDeletesSF = stateful' Map.empty step
  where
    step (toDelete, updates) old =
      let intermediate = foldl' (flip Map.delete) old toDelete
      in foldl' (\m (id,a) -> Map.insert id a m) intermediate updates

-- | Keeps track of an ID map, applying overwrite-updates on each tick.
applyMapUpdatesSF :: Ord id => [(id, a)] ~> Map id a
applyMapUpdatesSF = applyMapUpdatesAndDeletesSF <<^ ([],)

-- Events
---------
sample :: Bool -> a -> Maybe a
sample True  a = Just a
sample False _ = Nothing

sampleOnRisingEdge :: (Bool,a) ~> Maybe a
sampleOnRisingEdge = first risingEdge >>> arr2 sample

sampleOnChange :: Eq a => a -> a ~> Maybe a
sampleOnChange a0 = proc a -> do
  da <- delay a0 -< a
  sampleOnRisingEdge -< (da /= a, a)

notYet :: Maybe a ~> Maybe a
notYet = SF $ const (Nothing, id)

-- Varia
--------
arr2 :: (a -> b -> c) -> ((a,b) ~> c)
arr2 = arr . uncurry

constant :: o -> (i ~> o)
constant = arr . const

inspect :: (a ~> b) -> (a ~> (b, a ~> b))
inspect (SF sf) = SF $ \dta -> let (b,sf') = sf dta in ((b,sf'), inspect sf')
