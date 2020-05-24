{-# LANGUAGE BangPatterns #-}

module Newton (newton,
               newtonShow,
               newtonN,
               newtonNShow,
               safeNewton,
               safeNewtonShow,
               newtonMult,
               newtonMultShow) where

import InterComp
import Text.Printf
import Data.Monoid
import Control.Monad
-- import ReturnAndPrint



newton :: (Fractional a, Ord a) =>
          a                             -- error threshold
          -> (a -> Interval a)          -- function of interest
          -> (Interval a -> Interval a) -- its derivitive
          -> Interval a                 -- initial interval
          -> (Sum Int, Interval a)
newton err f df !x0 = iter 1 x0 (next 1 x0)
 where
  recp inter = (recip s ... recip i)
   where (i,s) = (inf inter, sup inter)
  next idx x = x `intersection` (singleton mx - f mx * recp (df x))
   where mx = (if mod idx 2 == 0 then inf else sup) x
  iter idx ant curr
   | width curr <= err          ||
     distance ant curr <= err = (1,curr)
   | otherwise = join (1,iter (idx + 1) curr $ next (idx + 1) curr)


newtonN :: (Fractional a, Ord a) =>
           a
           -> (Vector a -> IVector a)
           -> (IVector a -> IMatrix a)
           -> IVector a
           -> IVector a
newtonN err f df !x0 = iter x0 (next x0)
 where
  next x = x `intersecV` ((singleV mx) !+! (iga (df x) (negV $ f mx))) where mx = midV x
  iter ant curr
   | any ((<=err) . width) curr ||
     widthV curr <= err         ||
     isEmptyV curr              || 
     distanceV ant curr <= err = curr
   | otherwise = iter curr (next curr)


safeNewton :: (Fractional a, Ord a) =>
              a                             -- error threshold
              -> (a -> Interval a)          -- function of interest
              -> (Interval a -> Interval a) -- its derivitive
              -> Interval a                 -- an initial interval
              -> Maybe (Sum Int, Interval a)
safeNewton err f df !x = if 0 `member` df x then Nothing else Just $ newton err f df x
{-# INLINE safeNewton #-}

newtonMult :: (Fractional a, Ord a, Show a) =>
              a                            -- error threshold
              -> (a -> Interval a)         -- function of interest
              -> (Interval a -> Interval a)-- its derivitive
              -> Interval a                -- initial interval
              -> [(Sum Int, Interval a)]
newtonMult err f df !x
 | 0 `notMember` df x = [newton err f df x]
 | otherwise = concatMap (newtonMult err f df) bisection
 where
  bisection = let mx = midpoint x
   in filter (not.isEmpty) $ map (\r -> x `intersection` (singleton mx - f mx * r)) $ reciprical $ df x

-- show function versions

newtonShow :: (Fractional a, Ord a, Show a) => --, PrintfArg a) =>
              a                            -- error threshold
              -> (a -> Interval a)         -- function of interest
              -> (Interval a -> Interval a)-- its derivitive
              -> String                    -- function formula
              -> Interval a                -- initial interval
              -> IO ()
newtonShow err f df strF !x0 = do
 printf "\n Function: %s\
      \\n\n Error threshold: %s\
      \\n\n  x0\t= %s\n" strF (show err) (show x0) :: IO ()
 iter 1 x0 (next 1 x0)
 where
  next idx x = x `intersection` (singleton mx - f mx / df x)
   where mx = (if mod idx 2 == 0 then inf else sup) x
  iter idx ant curr
   | width curr <= err          ||
     isEmpty curr               ||
     distance ant curr <= err =
    printf "\n  x%s\t= %s\t<- root\n\n" (show idx) (show curr) :: IO ()
   | otherwise = do
    printf "  x%s\t= %s\n" (show idx) (show curr) :: IO ()
    iter (idx + 1) curr (next (idx + 1) curr)


newtonNShow :: (Fractional a, Ord a, Show a) =>
           a
           -> (Vector a -> IVector a)
           -> (IVector a -> IMatrix a)
           -> IVector a
           -> IO ()
newtonNShow err f df !x0 = iter 1 x0 (next x0)
 where
  next x = x `intersecV` ((singleV mx) !+! (iga (df x) (negV $ f mx))) where mx = midV x
  iter idx ant curr
   | widthV curr <= err          ||
     isEmptyV curr               || 
     distanceV ant curr <= err =
    printf "\n  x%s\t=\n\n%s\t\t\t\t\t<- root\n\n" (show idx) (showVM curr) :: IO ()
   | otherwise = do
    printf "  x%s\t=\n\n%s\n\n" (show idx) (showVM curr) :: IO ()
    iter (idx + 1) curr (next curr)

safeNewtonShow :: (Fractional a, Ord a, Show a, PrintfArg a) =>
                  a                             -- error threshold
                  -> (a -> Interval a)          -- function of interest
                  -> (Interval a -> Interval a) -- its derivitive
                  -> String                     -- function formula
                  -> Interval a                 -- an initial interval
                  -> IO ()
safeNewtonShow err f df strF !x = if 0 `member` df x then putStrLn "\n 0 ´member` df x" else newtonShow err f df strF x
{-# INLINE safeNewtonShow #-}

newtonMultShow :: (Fractional a, Ord a, Show a, PrintfArg a) =>
              a                            -- error threshold
              -> (a -> Interval a)         -- function of interest
              -> (Interval a -> Interval a)-- its derivitive
              -> String                    -- function formula
              -> Interval a                -- initial threshold
              -> IO ()
newtonMultShow err f df strF !x 
 | 0 `notMember` df x = newtonShow err f df strF x
 | otherwise = do
  putStrLn "split"
  sequence_ $ map (newtonMultShow err f df strF) bisection
 where
  bisection = let mx = midpoint x
   in map (\r -> x `intersection` (singleton mx - f mx * r)) $ reciprical $ df x
