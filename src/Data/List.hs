{-|
Changed behavior:

* Partial functions made safe (e.g. `head :: [a] -> Maybe a`)
* General functions monomorphized (e.g. `null :: [a] -> Bool`)
* More functions like `sortOn` (e.g. `nubOn`)
-}
module Data.List (
  -- * Basic functions
  (++),
  head,
  last,
  tail,
  init,
  uncons,
  singleton,
  null,
  length,

  -- * List transformations
  map,
  reverse,
  intersperse,
  intercalate,
  transpose,
  subsequences,
  permutations,

  -- * Reducing lists (folds)
  foldl,
  foldl',
  foldl1,
  foldl1',
  foldr,
  foldr1,

  -- ** Special folds
  concat,
  concatMap,
  and,
  or,
  any,
  all,
  sum,
  product,
  maximum,
  minimum,

  -- * Building lists

  -- ** Scans
  scanl,
  scanl',
  scanl1,
  scanr,
  scanr1,

  -- ** Accumulating maps
  mapAccumL,
  mapAccumR,

  -- ** Infinite lists
  iterate,
  iterate',
  repeat,
  replicate,
  cycle,

  -- ** Unfolding
  unfoldr,

  -- * Sublists

  -- ** Extracting sublists
  take,
  drop,
  splitAt,
  takeWhile,
  dropWhile,
  dropWhileEnd,
  span,
  break,
  stripPrefix,
  group,
  inits,
  tails,

  -- ** Predicates
  isPrefixOf,
  isSuffixOf,
  isInfixOf,
  isSubsequenceOf,

  -- * Searching lists

  -- ** Searching by equality
  elem,
  notElem,
  lookup,

  -- ** Searching with a predicate
  find,
  filter,
  partition,

  -- * Indexing lists

  -- | These functions treat a list @xs@ as a indexed collection,
  -- with indices ranging from 0 to @'length' xs - 1@.
  (!!),
  elemIndex,
  elemIndices,
  findIndex,
  findIndices,

  -- * Zipping and unzipping lists
  zip,
  zip3,
  zip4,
  zip5,
  zip6,
  zip7,
  zipWith,
  zipWith3,
  zipWith4,
  zipWith5,
  zipWith6,
  zipWith7,
  unzip,
  unzip3,
  unzip4,
  unzip5,
  unzip6,
  unzip7,

  -- * Special lists

  -- ** Functions on strings
  lines,
  words,
  unlines,
  unwords,

  -- ** \"Set\" operations
  nub,
  delete,
  deleteFirsts,
  (\\),
  union,
  intersect,

  -- ** Ordered lists
  sort,
  insert,

  -- * Generalized functions

  -- ** The \"@By@\" operations

  -- | By convention, overloaded functions have a non-overloaded
  -- counterpart whose name is suffixed with \`@By@\'.
  --
  -- It is often convenient to use these functions together with
  -- 'Data.Function.on', for instance @'sortBy' ('Prelude.compare'
  -- ``Data.Function.on`` 'Prelude.fst')@.

  -- *** User-supplied equality (replacing an @Eq@ context)

  -- | The predicate is assumed to define an equivalence.
  nubBy,
  deleteBy,
  deleteFirstsBy,
  unionBy,
  intersectBy,
  groupBy,

  -- *** User-supplied comparison (replacing an @Ord@ context)

  -- | The function is assumed to define a total ordering.
  sortBy,
  insertBy,
  maximumBy,
  minimumBy,

  -- ** The \"@On@\" operations

  -- | Convenience functions for the common case of using functions like
  -- `sortBy`:
  -- @
  -- sortOn f === sortBy (compare `on` f)
  -- @
  nubOn,
  deleteOn,
  deleteFirstsOn,
  unionOn,
  intersectOn,
  groupOn,
  sortOn,
  insertOn,
  maximumOn,
  minimumOn,

  -- ** The \"@generic@\" operations

  -- | The prefix \`@generic@\' indicates an overloaded function that
  -- is a generalized version of a "Prelude" function.
  genericLength,
  genericTake,
  genericDrop,
  genericSplitAt,
  genericIndex,
  genericReplicate,
) where

import "base" Data.List as X hiding (
  all,
  and,
  any,
  concat,
  concatMap,
  elem,
  find,
  foldl,
  foldl',
  foldl1,
  foldl1',
  foldr,
  foldr1,
  genericIndex,
  head,
  init,
  last,
  length,
  mapAccumL,
  mapAccumR,
  maximum,
  maximumBy,
  minimum,
  minimumBy,
  notElem,
  null,
  or,
  product,
  sortOn,
  sum,
  tail,
  (!!),
 )
import qualified "base" Data.List as X
import "base" Prelude (
  Maybe (..),
  undefined,
 )

head :: [a] -> Maybe a
head [] = Nothing
head (x : _) = Just x

last :: [a] -> Maybe a
last = undefined

tail = undefined
init = undefined
null = undefined
length = undefined

foldl = undefined
foldl' = undefined
foldl1 = undefined
foldl1' = undefined
foldr = undefined
foldr1 = undefined

concat = undefined
concatMap = undefined
and = undefined
or = undefined
any = undefined
all = undefined
sum = undefined
product = undefined
maximum = undefined
minimum = undefined

mapAccumL = undefined
mapAccumR = undefined

elem = undefined
notElem = undefined

find = undefined

(!!) = undefined

deleteFirsts = undefined

maximumBy = undefined
minimumBy = undefined

nubOn = undefined
deleteOn = undefined
deleteFirstsOn = undefined
unionOn = undefined
intersectOn = undefined
groupOn = undefined
sortOn = undefined
insertOn = undefined
maximumOn = undefined
minimumOn = undefined

genericIndex = undefined
