module Bag (
    Bag (..), 
) where

import Bucket

newtype Bag k v = Bag [Bucket k v] deriving (Show, Eq)
