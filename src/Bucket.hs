module Bucket (
    Bucket (..),
    updateBucket,
    decrementBucket,
) where

data Bucket k = Bucket k Int | EmptyBucket deriving (Eq)

instance (Show k) => Show (Bucket k) where
    show EmptyBucket = "EmptyBucket"
    show (Bucket k v) = unwords ["Bucket", show k, show v]

updateBucket :: [Bucket k] -> Int -> k -> [Bucket k]
updateBucket buckets index key = before ++ [newBucket] ++ after
  where
    newBucket = case buckets !! index of
        EmptyBucket -> Bucket key 1
        Bucket _ v -> Bucket key (v + 1)
    after = drop (index + 1) buckets
    before = take index buckets

decrementBucket :: [Bucket k] -> Int -> k -> [Bucket k]
decrementBucket buckets index key = before ++ [newBucket] ++ after
  where
    newBucket = case buckets !! index of
        EmptyBucket -> EmptyBucket
        Bucket _ v -> Bucket key $ max 0 (v - 1)
    after = drop (index + 1) buckets
    before = take index buckets
