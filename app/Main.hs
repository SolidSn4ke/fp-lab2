module Main (main) where

import Bag

main :: IO ()
main = do
    let bag = newBag
    let bag' = put "hello" bag
    let bag'' = put "hello" bag'
    print bag''
