{- This module is a complement for the
 - Numeric.Interval created by Edward Kmett-}
module Testing.Numeric.InterComp
  ( module Numeric.Interval
  , module I
  , LinearSystem
  , (!+!), (*!)
  , negV
  , ga, iga
  , backSubs
  , isEmpty
  , distance
  , distanceV
  , intersecV
  , singleV
  , midV
  , widthV
  , isEmptyV
  , (\/), (!^)
  , gH
  , monotonic
  , reciprical
  , Vector
  , Matrix
  , IVector
  , IMatrix
  , showVM
  , isin
  , icos
  , upperHalf
  , lowerHalf) where



import Numeric.Interval hiding (distance, elem, notElem)
import qualified Numeric.Interval.Internal as I
import Data.List (intercalate, foldl', maximumBy)
import Data.Function (on)

type Vector a = [a]
type Matrix a = Vector (Vector a)
type IVector a = Vector (Interval a)
type IMatrix a = Matrix (Interval a)
type LinearSystem a = (Matrix a, Vector a)
type Joined a = [[a]]


instance Ord a => Semigroup (Interval a) where
 (<>) = hull


instance Ord a => Monoid (Interval a) where
 mempty = empty


instance Enum a => Enum (Interval a) where
  fromEnum = fromEnum . sup
  toEnum = singleton . toEnum


instance Integral a => Integral (Interval a) where
  quotRem a b = (iquot a b, imod a b)
  toInteger x = toInteger $ maximumBy (compare `on` abs) [inf x, sup x]


upperHalf x = midpoint x ... sup x
{-# INLINE upperHalf #-}


lowerHalf x = inf x ... midpoint x
{-# INLINE lowerHalf #-}


icos :: (Floating a, Ord a) => Interval a -> Interval a
icos = monotonic cos 
{-# INLINE icos #-}


isin x = case (flimod4, d) of
  (_,0) -> si \/ ss
  (n,1) -> case n of
    0 -> min si ss ... 1
    2 -> -1 ... max si ss
    _ -> si \/ ss
  (n,2) -> case n of
    0 -> ss ... 1
    1 -> -1 ... si
    2 -> -1 ... ss
    3 -> si ... 1
  (n,3) -> case n of
    1 -> -1 ... max si ss
    3 -> min si ss ... 1
    _ -> -1 ... 1
  _ -> -1 ... 1
  where i = inf x
        s = sup x
        si = sin i
        ss = sin s
        fli = floor $ i * 2 / pi
        fls = floor $ s * 2 / pi
        d = fls - fli
        flimod4 = fli `mod` 4
{-# INLINE isin #-}


showVM v = "  [ " ++ intercalate "\n  , " (map show v) ++ " ]"
{-# INLINE showVM #-}



(!+!) :: Num a => Vector a -> Vector a -> Vector a
(!+!) = zipWith (+)
{-# INLINE (!+!) #-}



(*!) :: Num a => a -> Vector a -> Vector a
(*!) alpha = map (*alpha)
{-# INLINE (*!) #-}



intersecV :: (Ord a) => IVector a -> IVector a -> IVector a
intersecV u v
 | any isEmpty inter = map (const empty) v
 | otherwise = inter
 where inter = zipWith intersection v u
{-# INLINE intersecV #-}



midV :: Fractional a => IVector a -> Vector a
midV = map midpoint
{-# INLINE midV #-}



singleV :: Vector a -> IVector a
singleV = map singleton
{-# INLINE singleV #-}



widthV :: (Num a, Ord a) => IVector a -> a
widthV = maximum . map width 
{-# INLINE widthV #-}



negV :: Num a => Vector a -> Vector a
negV = map negate
{-# INLINE negV #-}



distanceV :: (Fractional a, Ord a) => IVector a -> IVector a -> a
distanceV = (maximum .) . zipWith distance
{-# INLINE distanceV #-}



isEmptyV :: Ord a => IVector a -> Bool
isEmptyV = any isEmpty
{-# INLINE isEmptyV #-}



vecToMat :: Vector a -> Matrix a
vecToMat = map (:[])
{-# INLINE vecToMat #-}



linSys :: Fractional a => Matrix a -> Vector a -> LinearSystem a
linSys = (,)
{-# INLINE linSys #-}



mkJoined :: Matrix a -> Vector a -> Joined a
mkJoined mat b = [(mat !! i) ++ [b !! i] | i <- [0 .. length mat - 1]]
{-# INLINE mkJoined #-}



eliminateColumn :: Fractional a => Joined a -> Int -> Joined a
eliminateColumn mat pivot = [eliminateMat i | i <- [0..n]]
 where
 n = length mat - 1
 eliminateMat i
  | i <= pivot = mat!!i
  | otherwise = mat!!i !+! ((-mip) *! (mat!!pivot))
  where mip = ((mat!!i)!!pivot) / ((mat!!pivot)!!pivot)



ga' :: Fractional a => Joined a -> Joined a
ga' mat = fst $ iterate update (mat, 0)!!(length mat - 1)
 where update (matrix, idx) = (eliminateColumn matrix idx, idx + 1)



sum' :: Num a => [Vector a] -> Vector a
sum' = foldl' (!+!) $ repeat 0
{-# INLINE sum' #-}



--ga' :: Fractional a => LinearSystem a -> Joined a
--ga' ls@(m,b) = [bigL rows r | r <- [0..rows]]
-- where
--  mat = mkJoined ls
--  rows = length mat - 1
--  bigL 0 n = mat !! n
--  bigL m n = lMm1N !+! sum' [ negate (lMm1N !! i) *! (ani (m - 1) i) | i <- [0..n - 1]]
--   where
--    lMm1N = bigL (m - 1) n
--    ani m p = (1/(lmp!!p)) *! lmp
--     where lmp = bigL m p



solve :: Fractional a => Matrix a -> Vector a -> Vector a
solve = ((backSubs . ga') .) . mkJoined
{-# INLINE solve #-}



backSubs :: Fractional a => Joined a -> Vector a
backSubs mat = sol
 where
  cn = length (head mat) - 1
  rn = length mat - 1
  sol = [(((mat !! r) !! cn) - sum' r) / ((mat !! r) !! r) | r <- [0 .. rn]]
  sum' r = sum [((mat !! r) !! k) * (sol !! k) | k <- [r + 1 .. rn]]


ga :: (Fractional a, Ord a) => Matrix a -> Vector a -> Vector a
ga = solve



iga :: (Fractional a, Ord a) => IMatrix a -> IVector a -> IVector a
iga = solve
{-# INLINE iga #-}



--eliminateColumngH :: Fractional a => Joined a -> Int -> Joined a
--eliminateColumngH mat pivot = [eliminateMat i | i <- [0..n]]
-- where
-- n = length mat - 1
-- eliminateMat i
--  | i <= pivot = mat!!i
--  | otherwise = mat!!i !+! ((-mip) *! (mat!!pivot))
--  where mip = ((mat!!i)!!pivot) / ((mat!!pivot)!!pivot)
--
--
--ga :: Fractional a => Joined a -> Joined a
--ga mat = fst $ (iterate update (mat, 0))!!(length mat - 1)
-- where update (matrix, idx) = (eliminateColumn matrix idx, idx + 1)



isEmpty :: Ord a => Interval a -> Bool
isEmpty = Numeric.Interval.null
{-# INLINE isEmpty #-}



distance :: (Fractional a, Ord a) => Interval a -> Interval a -> a
distance I.Empty I.Empty = 0
distance I.Empty _ = 1/0
distance _ I.Empty = 1/0
distance i1 i2 = max distInf distSup
 where
  distInf = abs (inf i1 - inf i2)
  distSup = abs (sup i1 - sup i2)
{-# INLINE distance #-}




(\/) :: Ord a => a -> a -> Interval a
l \/ r 
 | l <= r = l ... r
 | otherwise = r ... l
{-# INLINE (\/) #-}




gH :: (Num a, Ord a) => Interval a -> Interval a -> Interval a
gH x y = (inf x - inf y) \/ (sup x - sup y)
{-# INLINE gH #-}



monotonic :: Ord b => (a -> b) -> Interval a -> Interval b
monotonic _ I.Empty = I.Empty
monotonic f i = f (inf i) \/ f (sup i)
{-# INLINE monotonic #-}



reciprical :: (Fractional a, Ord a) => Interval a -> [Interval a]
reciprical (I.I 0 0) = [0 ... 0]
reciprical (I.I i 0) = [-1/0 ... 1/i]
reciprical (I.I 0 s) = [1/s ... 1/0]
reciprical (I.I i s)
 | i > 0 || s < 0     = [1/s ... 1/i]
 | otherwise          = [-1/0 ... 1/i, 1/s ... 1/0]
{-# INLINE reciprical #-}



(!^) :: (Ord a, Num a, Integral b) => Interval a -> b -> Interval a
i !^ e
 | odd e || inf i >= 0 || sup i <= 0 = monotonic (^e) i
 | otherwise = 0 ... (magnitude i ^ e)
{-# INLINE (!^) #-}



testMat :: Matrix Double
testMat = [[3,-4,0],[1,2,3],[2,3,4]]
testB :: Vector Double
testB = [10,4,6]
