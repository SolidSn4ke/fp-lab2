module Lib (
    someFunc,
) where

data Node a = Null | Node Int a Int (Node a) deriving (Show, Eq)

instance Functor Node where
    fmap _ Null = Null
    fmap f (Node h k v n) = Node h (f k) v (fmap f n)

newtype Bucket a = Bucket (Node a) deriving (Show, Eq)

newtype Bag a = Bag [Bucket a] deriving (Show, Eq)

hash :: Show a => a -> Int
hash x = case show x of
    "" -> -1
    (c:_)  -> fromEnum c

someFunc :: IO ()
someFunc = putStrLn "someFunc"
