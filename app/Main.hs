module Main (main) where

import Bag

main :: IO ()
main = do
    let b = insert "hello" . insert "hello" . insert "hello" $ newBag
    let b2 = delete "" b
    print b2

