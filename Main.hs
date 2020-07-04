import System.Process (system)
import Control.Monad (join)
import Numeric.IEEE (epsilon)
import Data.Monoid (Sum(..),getSum)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldr')
import Data.List
import Refinement
import InterComp
import Newton
import Parsing
import Fs
import Performs
import Fuck

addVtoSubl :: (Fractional a, Ord a) => [IVector a] -> [Interval a] -> [IVector a]
addVtoSubl vs xs = [ v ++ [x] |  v <- vs, x <- xs]

subdivNl :: (Fractional a, Ord a) => Int -> IVector a -> [IVector a]
subdivNl n x = foldl' addVtoSubl [[]] $ map (subdivide n) x

addVtoSubr :: (Fractional a, Ord a) => [Interval a] -> [IVector a] -> [IVector a]
addVtoSubr xs vs = [ x:v |  x <- xs, v <- vs]

subdivNr :: (Fractional a, Ord a) => Int -> IVector a -> [IVector a]
subdivNr n x = foldr' addVtoSubr [[]] $ map (subdivide n) x

fe :: Ord a => [IVector a] -> [IVector a]
fe = filter (not . isEmptyV)

a f = midV . f . midV

pprint (Sum n, l) = do
  print n
  mapM_ print l

intervalDomToReal :: (Fractional a, Ord a) => (Vector a -> IVector a) -> Vector a -> Vector a
intervalDomToReal = (midV .)
intervalToReal :: (Fractional a, Ord a) => (IVector a -> IMatrix a) -> Vector a -> Matrix a
intervalToReal f = map midV . f . singleV

tempNewtonN :: (Fractional a, Ord a) => a -> (Vector a -> Vector a) -> (Vector a -> Matrix a) -> Vector a -> (Sum Int, Vector a)
tempNewtonN err f f' x0 = iter x0 (next x0)
  where next x = x !+! ga (f' x) (negV $ f x)
        iter ant curr
          | dist ant curr <= err = (1,curr)
          | otherwise = join (1,iter curr (next curr))
        dist vx vy = maximum $ zipWith (\x y -> abs (x - y)) vx vy 

tempNewton :: (Fractional a, Ord a) => a -> (a -> a) -> (a -> a) -> a -> (Sum Int, a)
tempNewton err f f' x0 = iter x0 (next x0)
  where next x = x - f' x / f x
        iter ant curr
          | dist ant curr <= err = (1,curr)
          | otherwise = join (1,iter curr (next curr))
        dist x y = abs (x - y)

tempNewtonNShow :: (Show a, Fractional a, Ord a) => a -> (Vector a -> Vector a) -> (Vector a -> Matrix a) -> Vector a -> IO ()
tempNewtonNShow err f f' x0 = iter x0 (next x0)
  where next x = x !+! ga (f' x) (negV $ f x)
        iter ant curr
          | dist ant curr <= err = print curr
          | otherwise = do
            print $ dist ant curr 
            print (curr, ant)
            iter curr (next curr)
        dist vx vy = maximum $ zipWith (\x y -> abs (x - y)) vx vy 

ref f f' = fmap (filter (not . isEmptyV)) . foldMap (fmap (:[]) . newtonN epsilon f f') . subdivNl 2

m f f' = fmap join . mapM (ref f f')

fil err (_,(_,r)) = widthV r <= err && not (isEmptyV r)

main :: IO ()
main = getOptions >>= performSafeFs
