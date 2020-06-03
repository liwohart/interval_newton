module Performs (performFs, performMultFs, performSafeFs) where

import Data.Monoid (Sum(..))
import Data.Maybe (fromMaybe)
import InterComp
import Newton
import Fs


fromTuple :: Ord a => (a,a) -> Interval a
fromTuple (i,s) = i ... s

-- perform functions

performNewtonFs :: Int -> Double -> Maybe (Interval Double) -> (Sum Int, Interval Double)
performNewtonFs idx err mx0 = newton err f df $ fromMaybe inter mx0
 where (inter, f, df, _, _) = fs!!idx
{-# INLINE performNewtonFs #-}

performNewtonMultFs :: Int -> Double -> Maybe (Interval Double) -> (Sum Int, [Interval Double])
performNewtonMultFs idx err mx0 = sequence $ newtonMult err f df $ fromMaybe inter mx0
 where (inter, f, df, _, _) = fs!!idx
{-# INLINE performNewtonMultFs #-}

performSafeNewtonFs :: Int -> Double -> Maybe (Interval Double) -> Maybe (Sum Int, Interval Double)
performSafeNewtonFs idx err mx0 = safeNewton err f df $ fromMaybe inter mx0
 where (inter, f, df, _, _) = fs!!idx
{-# INLINE performSafeNewtonFs #-}

-- show function versions

performNewtonShowFs :: Int -> Double -> Maybe (Interval Double) -> IO ()
performNewtonShowFs idx err mx0 = newtonShow err f df strF $ fromMaybe inter mx0
 where (inter, f, df, strF, _) = fs!!idx
{-# INLINE performNewtonShowFs #-}

performNewtonMultShowFs :: Int -> Double -> Maybe (Interval Double) -> IO ()
performNewtonMultShowFs idx err mx0 = newtonMultShow err f df strF $ fromMaybe inter mx0
 where (inter, f, df, strF, _) = fs!!idx
{-# INLINE performNewtonMultShowFs #-}

performSafeNewtonShowFs :: Int -> Double -> Maybe (Interval Double) -> IO ()
performSafeNewtonShowFs idx err mx0 = safeNewtonShow err f df strF $ fromMaybe inter mx0
 where (inter, f, df, strF, _) = fs!!idx
{-# INLINE performSafeNewtonShowFs #-}

-- interact function versions

performFs :: (Int, Double, Maybe (Double, Double), Bool) -> IO ()
performFs (idx, err, mx0, True) = performNewtonShowFs idx err (fmap fromTuple mx0)
performFs (idx, err, mx0, False) = print $ performNewtonFs idx err (fmap fromTuple mx0)
{-# INLINE performFs #-}

performMultFs :: (Int, Double, Maybe (Double, Double), Bool) -> IO ()
performMultFs (idx, err, mx0, True) = performNewtonMultShowFs idx err (fmap fromTuple mx0)
performMultFs (idx, err, mx0, False) = print $ performNewtonMultFs idx err (fmap fromTuple mx0)
{-# INLINE performMultFs #-}

performSafeFs :: (Int, Double, Maybe (Double, Double), Bool) -> IO ()
performSafeFs (idx, err, mx0, True) = performSafeNewtonShowFs idx err (fmap fromTuple mx0)
performSafeFs (idx, err, mx0, False) = print $ performSafeNewtonFs idx err (fmap fromTuple mx0)
{-# INLINE performSafeFs #-}

