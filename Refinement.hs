module Refinement where

import InterComp
import Newton
import Data.Foldable (fold)

instance Ord a => Semigroup (Interval a) where
 (<>) = hull

instance Ord a => Monoid (Interval a) where
 mempty = empty


subdivide :: (Fractional a, Ord a) => Int -> Interval a -> [Interval a]
subdivide n x = let w = width x / fromIntegral n
                    (i,s) = (inf x, sup x)
                 in [(i + (fromIntegral m) * w)...(i + (fromIntegral $ m + 1) * w) | m <- [0..n - 1]]

refinement :: (Fractional a, Ord a) => Int -> (Interval a -> Interval a) -> Interval a -> Interval a
refinement 1 f = f
refinement n f = foldMap f . subdivide n

refinementM :: (Monad m, Fractional a, Ord a) => Int -> (Interval a -> m (Interval a)) -> Interval a -> m (Interval a)
refinementM 1 f = f
refinementM n f = fmap fold . mapM f . subdivide n

refinementM_ :: (Monad m, Fractional a, Ord a) => Int -> (Interval a -> m ()) -> Interval a -> m ()
refinementM_ 1 f = f
refinementM_ n f = mapM_ f . subdivide n
