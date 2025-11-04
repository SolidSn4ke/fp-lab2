module Bucket (
    Bucket (..), updateBuckets
) where

import Node

newtype Bucket a = Bucket (Node a) deriving (Show, Eq)

updateBuckets :: [Bucket a] -> Int -> Node a -> [Bucket a]
updateBuckets buckets index newNode = before ++ [Bucket $ insertNode oldNode newNode] ++ after 
    where
        Bucket oldNode = buckets !! index
        before = take index buckets
        after = drop (index + 1) buckets