module Main (main) where

import Bag
import Bucket

main :: IO ()
main = do
    let b = insert 0 . insert 0 . insert 0 $ newBag
    let a = foldlBag (\acc b -> case b of
            EmptyBucket -> acc
            Bucket k _ -> acc ++ show k) "" b
    let c = foldrBag (\b acc -> case b of 
            EmptyBucket -> acc
            Bucket k _  ->  show k ++ acc) "" b
    print a
    print c

