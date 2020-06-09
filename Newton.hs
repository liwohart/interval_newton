{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
#define MID 0

module Newton (newton,
               newtonShow,
               newtonN,
               newtonNShow,
               safeNewton,
               safeNewtonShow,
               newtonMult,
               newtonMultShow) where

import InterComp
import Text.Printf (printf, PrintfArg(..))
import Data.Monoid (Sum(..))
import Control.Monad (join)


newton :: (Fractional a, Ord a) =>
          a                             -- error threshold
          -> (a -> Interval a)          -- function of interest
          -> (Interval a -> Interval a) -- its derivitive
          -> Interval a                 -- initial interval
          -> (Sum Int, Interval a)
newton err f df !x0 = iter 1 x0 (next 1 x0)
 where
  recp inter
   | (i,s) <- (inf inter, sup inter)
   = (recip s ... recip i)
  next idx x
#if MID
   | mx <- midpoint x 
#else
   | mx <- (if mod idx 2 == 0 then inf else sup) x
#endif
   = x `intersection` (singleton mx - f mx * recp (df x))
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
  next x
   | mx <- midV x
   = x `intersecV` ((singleV mx) !+! (iga (df x) (negV $ f mx)))
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
safeNewton err f df !x
 | 0 `member` df x  = Nothing
 | otherwise        = Just $ newton err f df x
{-# INLINE safeNewton #-}

newtonMult :: (Fractional a, Ord a, Show a) =>
              a                            -- error threshold
              -> (a -> Interval a)         -- function of interest
              -> (Interval a -> Interval a)-- its derivitive
              -> Interval a                -- initial interval
              -> [(Sum Int, Interval a)]
newtonMult err f df !x
 | 0 `notMember` df x = [newton err f df x]
 | otherwise          = concatMap (newtonMult err f df) bisection
 where
  bisection
   | mx <- midpoint x
   = filter (not.isEmpty) $ map (\r -> x `intersection` (singleton mx - f mx * r)) $ reciprical $ df x

-- show function versions

newtonShow :: (Fractional a, Ord a, Show a) =>
              a                            -- error threshold
              -> (a -> Interval a)         -- function of interest
              -> (Interval a -> Interval a)-- its derivitive
              -> String                    -- function formula
              -> Int                       -- maximum number of iterations shown
              -> Interval a                -- initial interval
              -> IO ()
newtonShow err f df strF maxI !x0 = do
 printf "\n Function: %s \n\n Error threshold: %s \n\n  x0\t= %s\n" strF (show err) (show x0)
 iter 1 x0 (next 1 x0)
 where
  next idx x
#if MID
   | mx <- midpoint x
#else
   | mx <- (if mod idx 2 == 0 then inf else sup) x
#endif
   = x `intersection` (singleton mx - f mx / df x)

  iter idx ant curr
   | width curr <= err          ||
     isEmpty curr               ||
     distance ant curr <= err =
    printf "\n  x%s\t= %s\t<- root\n\n" (show idx) (show curr)
   | idx < maxI = do
    printf "  x%s\t= %s\n" (show idx) (show curr)
    iter (idx + 1) curr (next (idx + 1) curr)
   | idx == maxI = do
    putStrLn "  .\n  .\n  ." 
    iter (idx + 1) curr (next (idx + 1) curr)
   | otherwise = iter (idx + 1) curr (next (idx + 1) curr)


newtonNShow :: (Fractional a, Ord a, Show a) =>
           a
           -> (Vector a -> IVector a)
           -> (IVector a -> IMatrix a)
           -> Int
           -> IVector a
           -> IO ()
newtonNShow err f df maxI !x0 = iter 1 x0 (next x0)
 where
  next x
   | mx <- midV x
   = x `intersecV` ((singleV mx) !+! (iga (df x) (negV $ f mx)))

  iter idx ant curr
   | widthV curr <= err          ||
     isEmptyV curr               || 
     distanceV ant curr <= err =
    printf "\n  x%s\t=\n\n%s\t\t\t\t\t<- root\n\n" (show idx) (showVM curr) :: IO ()
   | idx <= maxI = do
    printf "  x%s\t=\n\n%s\n\n" (show idx) (showVM curr) :: IO ()
    iter (idx + 1) curr (next curr)
   | otherwise = iter (idx + 1) curr (next curr)

safeNewtonShow :: (Fractional a, Ord a, Show a, PrintfArg a) =>
                  a                             -- error threshold
                  -> (a -> Interval a)          -- function of interest
                  -> (Interval a -> Interval a) -- its derivitive
                  -> String                     -- function formula
                  -> Int                        -- maximum number of iteration shown
                  -> Interval a                 -- an initial interval
                  -> IO ()
safeNewtonShow err f df strF maxI !x
 | 0 `member` df x = putStrLn "\n 0 ´member` df x"
 | otherwise = newtonShow err f df strF maxI x
{-# INLINE safeNewtonShow #-}

newtonMultShow :: (Fractional a, Ord a, Show a, PrintfArg a) =>
              a                             -- error threshold
              -> (a -> Interval a)          -- function of interest
              -> (Interval a -> Interval a) -- its derivitive
              -> String                     -- function formula
              -> Int                        -- maximum number of iteration shown
              -> Interval a                 -- initial threshold
              -> IO ()
newtonMultShow err f df strF maxI !x 
 | 0 `notMember` df x = newtonShow err f df strF maxI x
 | otherwise = do
  putStrLn "split"
  sequence_ $ map (newtonMultShow err f df strF maxI) bisection
 where
  bisection
   | mx <- midpoint x
   = map (\r -> x `intersection` (singleton mx - f mx * r)) $ reciprical $ df x
