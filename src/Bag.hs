module Bag (
    Bag (..),
    newBag,
    insert,
    count,
    delete,
    filterBag,
    foldlBag,
    foldrBag,
    mapBag,
) where

import Bucket
import Data.Char (ord)

newtype Bag k = Bag [Bucket k]

instance (Eq k, Show k) => Semigroup (Bag k) where
    (<>) (Bag b1) (Bag b2) = foldlBag (\acc (Bucket k v) -> insertN k v acc) (Bag b1) $ filterBag (/= EmptyBucket) (Bag b2)

instance (Eq k, Show k) => Monoid (Bag k) where
    mempty = newBag
    mappend = (<>)

instance (Eq k, Show k) => Eq (Bag k) where
    (==) (Bag b1) (Bag b2) = cond1 && cond2
      where
        cond1 = length b1 == length b2
        cond2 = foldlBag (\acc (Bucket k v) -> acc && count k (Bag b2) == v) True $ filterBag (/= EmptyBucket) (Bag b1)

instance (Eq k, Show k) => Show (Bag k) where
    show (Bag bs) = "{" ++ inner ++ "}"
      where
        inner = foldlBag (\acc (Bucket k v) -> if v > 0 then acc ++ show k ++ ": " ++ show v ++ ", " else acc) "" $ filterBag (/= EmptyBucket) (Bag bs)

newBag :: Bag k
newBag = Bag $ [EmptyBucket | _ <- [1 .. 10]]

loadFactor :: (Eq k) => Bag k -> Double
loadFactor (Bag buckets) = filled / total
  where
    filled = fromIntegral . length $ filter (/= EmptyBucket) buckets
    total = fromIntegral $ length buckets

resize :: (Eq k, Show k) => Bag k -> Bag k
resize (Bag buckets) = reinsertAll keys (Bag [EmptyBucket | _ <- [1 .. newSize]])
  where
    newSize = length buckets * 2
    keys = [k | Bucket k _ <- buckets]
    reinsertAll [] bag = bag
    reinsertAll (k : ks) bag = reinsertAll ks (insert k bag)

insert :: (Eq k, Show k) => k -> Bag k -> Bag k
insert key (Bag buckets) =
    if loadFactor (Bag buckets) >= 0.7
        then insert key (resize $ Bag buckets)
        else updatedBag
  where
    helper i = case buckets !! i of
        EmptyBucket -> updateBucket buckets i key
        Bucket k v ->
            if k == key || v < 1
                then updateBucket buckets i key
                else helper ((i + 1) `mod` length buckets)
    h = hashcode key `mod` length buckets
    updatedBag = Bag $ helper h

insertN :: (Eq k, Show k) => k -> Int -> Bag k -> Bag k
insertN key n bag
    | n < 0 = bag
    | n == 0 = delete key . insert key $ bag
    | otherwise = insertN key (n - 1) (insert key bag)

count :: (Eq k) => (Show k) => k -> Bag k -> Int
count key (Bag buckets) = helper h
  where
    helper i = case buckets !! i of
        EmptyBucket -> 0
        Bucket k v ->
            if k == key
                then v
                else helper ((i + 1) `mod` length buckets)
    h = hashcode key `mod` length buckets

delete :: (Eq k) => (Show k) => k -> Bag k -> Bag k
delete key (Bag buckets) = Bag $ helper h
  where
    helper i = case buckets !! i of
        EmptyBucket -> decrementBucket buckets i key
        Bucket k _ ->
            if k == key
                then decrementBucket buckets i key
                else helper ((i + 1) `mod` length buckets)
    h = hashcode key `mod` length buckets

filterBag :: (Eq k) => (Bucket k -> Bool) -> Bag k -> Bag k
filterBag _ (Bag []) = Bag []
filterBag predicate (Bag (b : bs))
    | b == EmptyBucket || predicate b = Bag (b : filteredBs)
    | otherwise = case b of
        Bucket k _ -> Bag $ Bucket k 0 : filteredBs
  where
    Bag filteredBs = filterBag predicate (Bag bs)

foldlBag :: (a -> Bucket k -> a) -> a -> Bag k -> a
foldlBag _ acc (Bag []) = acc
foldlBag f acc (Bag (b : bs)) = foldlBag f (f acc b) (Bag bs)

foldrBag :: (Bucket k -> a -> a) -> a -> Bag k -> a
foldrBag _ acc (Bag []) = acc
foldrBag f acc (Bag (b : bs)) = f b (foldrBag f acc (Bag bs))

mapBag :: (Bucket k -> Bucket k) -> Bag k -> Bag k
mapBag _ (Bag []) = Bag []
mapBag f (Bag (b : bs)) = case mapBag f (Bag bs) of
    Bag mappedBs -> Bag (f b : mappedBs)

hashcode :: (Show s) => s -> Int
hashcode s = case show s of
    str -> foldl' (\acc x -> acc * 31 + ord x) 0 str
