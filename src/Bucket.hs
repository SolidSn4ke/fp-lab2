module Bucket (
    Bucket (..), updateBuckets
) where

data Status = Empty | Occupied | Deleted deriving (Show, Eq)

newtype Bucket k v = Bucket k v Status deriving (Show, Eq)