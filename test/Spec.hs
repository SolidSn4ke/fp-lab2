{- HLINT ignore "Monoid law, left identity", "Monoid law, right identity" -}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Bag
import Bucket

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Bag Tests" [unitTests, qcTests]

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
                f b = case b of
                    Bucket k _ -> k == "apple"
                    EmptyBucket -> True
                filteredBag = filterBag f bag
             in count "apple" filteredBag == 2 && count "banana" filteredBag == 0 @?= True,
          testCase "Automatic resize on insert" $
            let bag = foldl (flip insert) newBag [1 :: Int .. 10]
                Bag buckets = bag
             in length buckets @?= 20,
          testCase "FoldlBag" $
            let bag = foldl (flip insert) newBag [1 :: Int .. 10]
                f acc b = case b of
                    EmptyBucket -> acc
                    Bucket _ v -> acc + v
             in foldlBag f 0 bag @?= 10,
          testCase "FoldrBag" $
            let bag = foldl (flip insert) newBag [1 :: Int .. 10]
                f b acc = case b of
                    EmptyBucket -> acc
                    Bucket _ v -> acc + v
             in foldrBag f 0 bag @?= 10,
          testCase "MapBag" $
            let bag = foldl (flip insert) newBag [1 :: Int .. 10]
                mapFunc b = case b of
                    EmptyBucket -> EmptyBucket
                    Bucket k v -> Bucket k (v * 2)
                foldlFunc acc b = case b of
                    EmptyBucket -> acc
                    Bucket _ v -> acc + v
             in foldlBag foldlFunc 0 (mapBag mapFunc bag) @?= 20
        ]

prop_identityLaw :: Bag Int -> Bool
prop_identityLaw bag = (mempty <> bag) == (bag <> mempty)

prop_associativeLaw :: Bag Int -> Bag Int -> Bag Int -> Bool
prop_associativeLaw x y z =
    ((x <> y) <> z) == (x <> (y <> z))

prop_insertLaw :: Bag Int -> Int -> Bool
prop_insertLaw b e = numOfElems b + 1 == numOfElems (insert e b)
  where
    numOfElems = foldlBag f 0
    f acc bag = case bag of
        EmptyBucket -> acc
        Bucket _ v -> acc + v

qcTests :: TestTree
qcTests =
    testGroup
        "QuickCheck Tests"
        [ QC.testProperty "Neutral element law" prop_identityLaw,
          QC.testProperty "Associative law" prop_associativeLaw,
          QC.testProperty "Insert" prop_insertLaw
        ]
