module Data.ListTest (test) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.List as X

test :: TestTree
test =
  testGroup "Data.List" $
    [ testHead
    ]

testHead :: TestTree
testHead =
  testGroup "head" $
    [ testCase "returns Nothing on empty list" $
        X.head [] @?= (Nothing :: Maybe Integer)
    , testProperty "returns the first element of a non-empty list" $ \x xs ->
        X.head (x : xs) === (Just x :: Maybe Integer)
    ]
