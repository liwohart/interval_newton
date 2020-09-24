module Testing.Performs (performFs, performMultFs, performSafeFs) where

import Data.Monoid (Sum(..))
import Data.Maybe (fromMaybe)
import Testing.Numeric.InterComp (Interval(..), (...))
import Testing.Parsing (ShowOpt(..))
import Testing.Numeric.Newton
import Testing.Fs


fromTuple :: Ord a => (a,a) -> Interval a
fromTuple (i,s) = i ... s

maxNum = length fs

-- perform functions

performNewtonFs :: Int -> Double -> Maybe (Interval Double) -> (Sum Int, Interval Double)
performNewtonFs idx err
 | idx < maxNum = let (inter, f, df, _, _) = fs!!idx 
                    in newton err f df . fromMaybe inter
 | otherwise = error $ "No such function, try these numbers [0.." ++ show (length fs - 1) ++ "]."
{-# INLINE performNewtonFs #-}

performNewtonMultFs :: Int -> Double -> Maybe (Interval Double) -> (Sum Int, [Interval Double])
performNewtonMultFs idx err
 | idx < maxNum = let (inter, f, df, _, _) = fs!!idx
                    in sequence . newtonMult err f df . fromMaybe inter
 | otherwise = error $ "No such function, try these numbers [0.." ++ show (length fs - 1) ++ "]."
{-# INLINE performNewtonMultFs #-}

performSafeNewtonFs :: Int -> Double -> Maybe (Interval Double) -> Maybe (Sum Int, Interval Double)
performSafeNewtonFs idx err
 | idx < maxNum = let (inter, f, df, _, _) = fs!!idx 
                    in safeNewton err f df . fromMaybe inter
 | otherwise = error $ "No such function, try these numbers [0.." ++ show (length fs - 1) ++ "]."
{-# INLINE performSafeNewtonFs #-}

-- show function versions

performNewtonShowFs :: Int -> Double -> Int -> Maybe (Interval Double) -> IO ()
performNewtonShowFs idx err maxI mx0
 | idx < maxNum = let (inter, f, df, strF, _) = fs!!idx 
                    in newtonShow err f df strF maxI $ fromMaybe inter mx0
 | otherwise = error $ " No such function.\n Try these numbers [0.." ++ show (length fs - 1) ++ "]."
{-# INLINE performNewtonShowFs #-}

performNewtonMultShowFs :: Int -> Double -> Int -> Maybe (Interval Double) -> IO ()
performNewtonMultShowFs idx err maxI mx0
 | idx < maxNum = let (inter, f, df, strF, _) = fs!!idx 
                    in newtonMultShow err f df strF maxI $ fromMaybe inter mx0
 | otherwise = error $ " No such function.\n Try these numbers [0.." ++ show (length fs - 1) ++ "]."
{-# INLINE performNewtonMultShowFs #-}

performSafeNewtonShowFs :: Int -> Double -> Int -> Maybe (Interval Double) -> IO ()
performSafeNewtonShowFs idx err maxI
 | idx < maxNum = let (inter, f, df, strF, _) = fs!!idx 
                    in safeNewtonShow err f df strF maxI . fromMaybe inter
 | otherwise = error $ " No such function.\n Try these numbers [0.." ++ show (length fs - 1) ++ "]."
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

