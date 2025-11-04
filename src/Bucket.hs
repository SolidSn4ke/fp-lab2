module Bucket (
    Bucket (..),
) where

import Node

newtype Bucket a = Bucket (Node a) deriving (Show, Eq)
