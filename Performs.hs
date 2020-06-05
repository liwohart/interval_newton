module Performs (performFs, performMultFs, performSafeFs) where

import Data.Monoid (Sum(..))
import Data.Maybe (fromMaybe)
import InterComp (Interval(..), (...))
import Parsing (ShowOpt(..))
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

performNewtonShowFs :: Int -> Double -> Int -> Maybe (Interval Double) -> IO ()
performNewtonShowFs idx err maxI mx0 = newtonShow err f df strF maxI $ fromMaybe inter mx0
 where (inter, f, df, strF, _) = fs!!idx
{-# INLINE performNewtonShowFs #-}

performNewtonMultShowFs :: Int -> Double -> Int -> Maybe (Interval Double) -> IO ()
performNewtonMultShowFs idx err maxI mx0 = newtonMultShow err f df strF maxI $ fromMaybe inter mx0
 where (inter, f, df, strF, _) = fs!!idx
{-# INLINE performNewtonMultShowFs #-}

performSafeNewtonShowFs :: Int -> Double -> Int -> Maybe (Interval Double) -> IO ()
performSafeNewtonShowFs idx err maxI mx0 = safeNewtonShow err f df strF maxI $ fromMaybe inter mx0
 where (inter, f, df, strF, _) = fs!!idx
{-# INLINE performSafeNewtonShowFs #-}

-- interact function versions

performFs :: (Int, Double, Maybe (Double, Double), ShowOpt) -> IO ()
performFs (idx, err, mx0, P maxI) = performNewtonShowFs idx err maxI (fmap fromTuple mx0)
performFs (idx, err, mx0, S) = print $ performNewtonFs idx err (fmap fromTuple mx0)
performFs (idx, err, mx0, R) = print $ snd $ performNewtonFs idx err (fmap fromTuple mx0)
performFs (idx, err, mx0, I) = print $ fst $ performNewtonFs idx err (fmap fromTuple mx0)
{-# INLINE performFs #-}

performMultFs :: (Int, Double, Maybe (Double, Double), ShowOpt) -> IO ()
performMultFs (idx, err, mx0, P maxI) = performNewtonMultShowFs idx err maxI (fmap fromTuple mx0)
performMultFs (idx, err, mx0, S) = print $ performNewtonMultFs idx err (fmap fromTuple mx0)
performMultFs (idx, err, mx0, R) = print $ snd $ performNewtonMultFs idx err (fmap fromTuple mx0)
performMultFs (idx, err, mx0, I) = print $ fst $ performNewtonMultFs idx err (fmap fromTuple mx0)
{-# INLINE performMultFs #-}

performSafeFs :: (Int, Double, Maybe (Double, Double), ShowOpt) -> IO ()
performSafeFs (idx, err, mx0, P maxI) = performSafeNewtonShowFs idx err maxI (fmap fromTuple mx0)
performSafeFs (idx, err, mx0, S) = print $ performSafeNewtonFs idx err (fmap fromTuple mx0)
performSafeFs (idx, err, mx0, R) = print $ snd <$> performSafeNewtonFs idx err (fmap fromTuple mx0)
performSafeFs (idx, err, mx0, I) = print $ fst <$> performSafeNewtonFs idx err (fmap fromTuple mx0)
{-# INLINE performSafeFs #-}

