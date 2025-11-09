import Test.Tasty
import Test.Tasty.HUnit

import Bag
import Bucket

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Bag Tests" [unitTests]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ testCase "Insert and Count" $
            let bag = insert "apple" . insert "apple" . insert "banana" $ newBag
             in count "apple" bag @?= 2,
          testCase "Delete" $
            let bag = delete "apple" . insert "apple" . insert "apple" $ newBag
             in count "apple" bag @?= 1,
          testCase "Delete non-existing key" $
            let bag = delete "orange" . insert "apple" $ newBag
             in count "apple" bag == 1 && count "orange" bag == 0 @?= True,
          testCase "FilterBag" $
            let bag = insert "apple" . insert "banana" . insert "apple" $ newBag
                filteredBag = filterBag (\(Bucket k _) -> k == "apple") bag
             in count "apple" filteredBag == 2 && count "banana" filteredBag == 0 @?= True,
          testCase "Automatic resize on insert" $
            let bag = foldl (flip insert) newBag [1 .. 10]
                Bag buckets = bag
             in length buckets @?= 20,
          testCase "FoldlBag" $
            let bag = foldl (flip insert) newBag [1 .. 10]
                f acc b = case b of
                    EmptyBucket -> acc
                    Bucket _ v -> acc + v
             in foldlBag f 0 bag @?= 10,
          testCase "FoldrBag" $
            let bag = foldl (flip insert) newBag [1 .. 10]
                f b acc = case b of
                    EmptyBucket -> acc
                    Bucket _ v -> acc + v
             in foldrBag f 0 bag @?= 10,
          testCase "MapBag" $
            let bag = foldl (flip insert) newBag [1 .. 10]
                mapFunc b = case b of
                    EmptyBucket -> EmptyBucket
                    Bucket k v -> Bucket k (v * 2)
                foldlFunc acc b = case b of
                    EmptyBucket -> acc
                    Bucket _ v -> acc + v
             in foldlBag foldlFunc 0 (mapBag mapFunc bag) @?= 20
        ]
