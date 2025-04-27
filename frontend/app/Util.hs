module Util where

--------------------------------------------------------------------------------
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
--------------------------------------------------------------------------------

mapDeleteMany :: Ord k => [k] -> Map k a -> Map k a
mapDeleteMany keys map = foldl' (flip Map.delete) map keys

mapInsertMany :: Ord k => [(k,a)] -> Map k a -> Map k a
mapInsertMany kvPairs map = Map.fromList kvPairs `Map.union` map

mapAdjustMany :: Ord k => [(k, a -> a)] -> Map k a -> Map k a
mapAdjustMany adjusters map0 = foldl' (\map (k,f) -> Map.adjust f k map) map0 adjusters 

editMap :: Ord k => [(k,a)] -> [k] -> Map k a -> Map k a
editMap inserts deletes = mapInsertMany inserts . mapDeleteMany deletes

-- | Construct a lookup table with possibly multiple results for each key.
--   The original order is not preserved.
multiMapFromList :: Ord k => [(k,a)] -> Map k [a]
multiMapFromList
  = foldl'
    (\m (k,a) -> Map.alter (cons a) k m)
    Map.empty
  where
    cons a Nothing   = Just [a]
    cons a (Just as) = Just (a:as)

multiMapLookup :: Ord k => k -> Map k [a] -> [a]
multiMapLookup k = join . maybeToList . Map.lookup k

infixr 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x
