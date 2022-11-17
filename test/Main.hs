import Test.Tasty

import qualified Data.ListTest

main :: Main
main = defaultMain test

test :: TestTree
test =
  testGroup "base-plus" $
    [ Data.ListTest.test
    ]
