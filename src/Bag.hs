module Bag (
    someFunc,
) where

import Bucket
import Data.Bits
import Node

newtype Bag a = Bag [Bucket a] deriving (Show, Eq)

instance Functor Bag where
    fmap f (Bag b) = Bag $ map (fmapBucket f) b
      where
        fmapBucket f (Bucket n) = Bucket $ fmapNode f n
        fmapNode f Null = Null
        fmapNode f (Node h k v n) = Node h (f k) v (fmapNode f n)

instance Applicative Bag where
    pure x = Bag [Bucket (Node 0 x 0 Null)]
    (Bag fs) <*> (Bag xs) = Bag $ concatMap (`applyBucket` xs) fs
      where
        applyBucket (Bucket f) xs = case f of
            Null -> []
            Node h func v next -> map (fmapBucket func) xs ++ applyBucket (Bucket next) xs
        fmapBucket f (Bucket n) = Bucket $ fmapNode f n
        fmapNode f Null = Null
        fmapNode f (Node h k v n) = Node h (f k) v (fmapNode f n)

instance Monad Bag where
    return = pure
    (Bag b) >>= f = Bag $ concatMap (bindBucket f) b
      where
        bindBucket f (Bucket n) = case n of
            Null -> []
            Node h k v n' -> let Bag newBuckets = f k in newBuckets

hash :: (Show a) => a -> Int
hash x = case show x of
    "" -> -1
    (c : _) -> fromEnum c

put :: (Show a) => Bag a -> a -> Bag a
put (Bag b) a = do
    let h = hash a
    let index = h .&. length b
    Bag b

someFunc :: IO ()
someFunc = putStrLn "someFunc"
