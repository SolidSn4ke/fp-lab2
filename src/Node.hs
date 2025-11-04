module Node (
    Node (..),
) where

data Node a = Null | Node Int a Int (Node a) deriving (Show, Eq)
