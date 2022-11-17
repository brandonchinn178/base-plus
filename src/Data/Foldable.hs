module Data.Foldable (
  Foldable (..),
  Foldable1 (..),
  -- -- * Special biased folds
  -- foldrM,
  -- foldlM,
  -- -- * Folding actions
  -- -- ** Applicative actions
  -- traverse_,
  -- for_,
  -- sequenceA_,
  -- asum,
  -- -- ** Monadic actions
  -- mapM_,
  -- forM_,
  -- sequence_,
  -- msum,
  -- -- * Specialized folds
  -- concat,
  -- concatMap,
  -- and,
  -- or,
  -- any,
  -- all,
  -- maximumBy,
  -- minimumBy,
  -- -- * Searches
  -- notElem,
  -- find,
) where

import "base" Data.Coerce (Coercible, coerce)
import "base" Data.List.NonEmpty (NonEmpty ((:|)))
import qualified "base" Data.List.NonEmpty as NonEmpty
import "base" Data.Monoid (Monoid, mappend, mconcat, mempty, (<>))
import "base" Data.Semigroup (Semigroup)
import qualified "base" Data.Semigroup as Semigroup
import "base" Prelude (
  Bool (..),
  Eq,
  Int,
  Num,
  Ord,
  const,
  flip,
  id,
  map,
  undefined,
  (+),
  (.),
 )

class Foldable t where
  {-# MINIMAL foldMap | foldr #-}

  fold :: Monoid m => t m -> m
  fold = foldMap id
  {-# INLINE fold #-}

  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f = foldr (mappend . f) mempty
  {-# INLINE foldMap #-}

  foldMap' :: Monoid m => (a -> m) -> t a -> m
  foldMap' f = foldl' (\acc a -> acc <> f a) mempty

  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f z t = Semigroup.appEndo (foldMap (Semigroup.Endo #. f) t) z

  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldr' = undefined

  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl = undefined

  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldl' = undefined

  toList :: t a -> [a]
  toList = undefined

  null :: t a -> Bool
  null = foldr (\_ _ -> False) True

  length :: t a -> Int
  length = foldl' (\c _ -> c + 1) 0

  elem :: Eq a => a -> t a -> Bool
  elem = undefined

  sum :: Num a => t a -> a
  sum = undefined

  product :: Num a => t a -> a
  product = undefined

instance Foldable [] where
  fold = mconcat
  foldMap = (mconcat .) . map
  foldr = undefined
  foldr' = undefined
  foldl = undefined
  foldl' = undefined
  toList = id
  null = undefined
  length = undefined
  elem = undefined
  sum = undefined
  product = undefined

instance Foldable NonEmpty where
  fold ~(m :| ms) = m `mappend` fold ms
  foldMap f ~(a :| as) = f a `mappend` foldMap f as
  foldr f z ~(a :| as) = f a (foldr f z as)
  foldl f z (a :| as) = foldl f (f z a) as
  toList = NonEmpty.toList
  null _ = False
  length = NonEmpty.length

class Foldable t => Foldable1 t where
  {-# MINIMAL foldMap1 #-}

  foldMap1 :: Semigroup m => (a -> m) -> t a -> m

  head1 :: t a -> a
  head1 = Semigroup.getFirst #. foldMap1 Semigroup.First

  last1 :: t a -> a
  last1 t = foldr (flip const) (head1 t) t

  toNonEmpty :: t a -> NonEmpty a
  toNonEmpty = foldMap1 NonEmpty.singleton

  foldr1 :: (a -> a -> a) -> t a -> a
  foldr1 f t = foldr f a as
    where
      a :| as = toNonEmpty t

  foldl1 :: (a -> a -> a) -> t a -> a
  foldl1 f t = foldl f a as
    where
      a :| as = toNonEmpty t

  maximum :: Ord a => t a -> a
  maximum = Semigroup.getMax #. foldMap1 Semigroup.Max

  minimum :: Ord a => t a -> a
  minimum = Semigroup.getMin #. foldMap1 Semigroup.Min

instance Foldable1 NonEmpty where
  foldMap1 = undefined
  head1 = NonEmpty.head
  last1 = NonEmpty.last
  toNonEmpty = id
  foldr1 = undefined
  foldl1 = undefined
  maximum = undefined
  minimum = undefined

-- https://hackage.haskell.org/package/base-4.17.0.0/docs/src/Data.Functor.Utils.html#%23.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce
