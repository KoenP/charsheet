module Data.Zipper where

import Control.Comonad
import Data.List

data Zipper a = Zipper [a] a [a]

instance Show a => Show (Zipper a) where
  show (Zipper ls x rs) = "["
    <> intercalate ", " (map show (reverse ls))
    <> ", <" <> show x <> ">, "
    <> intercalate ", " (map show rs)
    <> "]"

zipLeft :: Zipper a -> Maybe (Zipper a)
zipLeft (Zipper (l:ls) x rs) = Just $ Zipper ls l (x:rs)
zipLeft _                    = Nothing

zipRight :: Zipper a -> Maybe (Zipper a)
zipRight (Zipper ls x (r:rs)) = Just $ Zipper (x:ls) r rs
zipRight _                    = Nothing

toList :: Zipper a -> [a]
toList (Zipper ls x rs) = reverse ls <> (x : rs)

instance Functor Zipper where
  fmap f (Zipper ls x rs) = Zipper (map f ls) (f x) (map f rs)

instance Comonad Zipper where
  extract (Zipper l x r) = x
  duplicate z = Zipper (unfoldr' zipLeft z) z (unfoldr' zipRight z)
    where
      unfoldr' :: (a -> Maybe a) -> a -> [a]
      unfoldr' f = unfoldr (\x -> (\y -> (y,y)) <$> f x)
