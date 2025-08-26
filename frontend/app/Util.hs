module Util where

--------------------------------------------------------------------------------
import Control.Category ((>>>))
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Reflex
import Reflex.Dom (DomBuilder, text, blank)
import Optics
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

{-# INLINE (?) #-}
(?) :: a -> a -> Bool -> a
(?) x y b = if b then x else y

{-# INLINE (|->) #-}
(|->) :: Ord k => k -> v -> Map k v
(|->) = Map.singleton
infixr 2 |->

errorOnLeft :: Show a => Either a b -> b
errorOnLeft (Left x)  = error (show x)
errorOnLeft (Right y) = y

readText :: Read a => Text -> a
readText = read . unpack

showText :: Show a => a -> Text
showText = pack . show

showWidget :: (DomBuilder t m, Show a) => a -> m ()
showWidget = text . showText

formatModifier :: (Num a, Ord a, Show a) => a -> Text
formatModifier n | n > 0     = "+" <> showText n
                 | otherwise = showText n

modifierWidget :: (Num a, Ord a, Show a, DomBuilder t m) => a -> m ()
modifierWidget = text . formatModifier

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) k = k x
whenJust Nothing  _ = blank

dropPrefix :: String -> String -> String
dropPrefix prefix = drop (length prefix)
