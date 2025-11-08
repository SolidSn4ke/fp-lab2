module Bag (
    Bag (..), newBag, insert, count, delete
) where

import Bucket 
import Data.Char (ord)
import Data.List (foldl')

newtype Bag k = Bag [Bucket k] deriving (Show, Eq)

instance Semigroup (Bag k) where
    (<>) (Bag b1) (Bag b2) = Bag (b1 ++ b2)

instance Monoid (Bag k) where
    mempty = newBag
    mappend = (<>)

newBag :: Bag k
newBag = Bag $ [EmptyBucket | _ <- [1..10]]

loadFactor :: Eq k => Bag k -> Double
loadFactor (Bag buckets) = filled / total
  where
    filled = fromIntegral . length $ filter (/= EmptyBucket) buckets
    total = fromIntegral $ length buckets

resize :: (Eq k, Show k) => Bag k -> Bag k
resize (Bag buckets) = reinsertAll keys (Bag [EmptyBucket | _ <- [1..newSize]])
  where
    newSize = length buckets * 2
    keys = [k | Bucket k _ <- buckets]
    reinsertAll [] bag = bag
    reinsertAll (k:ks) bag = reinsertAll ks (insert k bag)

insert :: Eq k => Show k => k -> Bag k -> Bag k
insert key (Bag buckets) = if loadFactor (Bag buckets) >= 0.7
    then insert key (resize $ Bag buckets)
    else updatedBag
  where
    helper i = case buckets !! i of
          EmptyBucket -> updateBucket buckets i key
          Bucket k v -> if k == key || v < 1
            then updateBucket buckets i key
            else helper ((i + 1) `mod` length buckets)
    h = hashcode key `mod` length buckets
    updatedBag = Bag $ helper h
    
count :: Eq k => Show k => k -> Bag k -> Int
count key (Bag buckets) = helper h
  where
    helper i = case buckets !! i of
          EmptyBucket -> 0
          Bucket k v -> if k == key
            then max 0 v
            else helper ((i + 1) `mod` length buckets)
    h = hashcode key `mod` length buckets

delete :: Eq k => Show k => k -> Bag k -> Bag k
delete key (Bag buckets) = Bag $ helper h
  where
    helper i = case buckets !! i of
          EmptyBucket -> decrementBucket buckets i key
          Bucket k v -> if k == key
            then decrementBucket buckets i key
            else helper ((i + 1) `mod` length buckets)
    h = hashcode key `mod` length buckets

hashcode :: Show s => s -> Int
hashcode s = case show s of
  str -> foldl' (\acc x -> acc * 31 + ord x) 0 str
