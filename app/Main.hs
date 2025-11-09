module Main (main) where

import Bag
import Bucket

main :: IO ()
main = do
    let b1 = insert 0 . insert 0 . insert 0 $ newBag
    let b2 = delete 2 . delete 2 . insert 1 . insert 1 . insert 2 $ newBag
    print $ b1 <> b2

