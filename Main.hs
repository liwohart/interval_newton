import System.Process (system)
import qualified Graphics.Gloss as G
import Control.Monad (join, when)
import Numeric.IEEE (epsilon)
import Data.Monoid (Sum(..),getSum)
import Data.Maybe (fromMaybe)
import Data.Foldable (fold, foldr')
import Data.List
import Testing.Numeric.Refinement
import Testing.Numeric.InterComp
import Testing.Numeric.Newton
import Testing.Parsing
import Testing.Fs
import Testing.Performs
import Testing.Fuck

addVtoSub :: (Fractional a, Ord a) => [IVector a] -> [Interval a] -> [IVector a]
addVtoSub vs xs = [ v ++ [x] |  v <- vs, x <- xs]

subdivN :: (Fractional a, Ord a) => Int -> IVector a -> [IVector a]
subdivN n x = foldl' addVtoSub [[]] $ map (subdivide n) x

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

ref f f' = fmap (filter (not . isEmptyV)) . foldMap (fmap (:[]) . newtonN epsilon f f') . subdivN 2

m f f' = fmap join . mapM (ref f f')

fil err (_,(_,r)) = widthV r <= err && not (isEmptyV r)

mean l = sum l / fromIntegral (length l)

meanSd l = let n = fromIntegral (length l)
               m = sum l / n
               aux = map ((^2) . subtract m) l
            in (m, sum aux / (n - 1))

maxMeanSd l = let (me,sd) = meanSd l
               in (maximum l, me, sd)

listOfResults
    :: Double
    -> (Vector Double -> IVector Double)
    -> (IVector Double -> IMatrix Double)
    -> IVector Double
    -> Int
    -> [(Sum Int, IVector Double)]
listOfResults err f f' x0 n = map (newtonN err f f') $ subdivN n x0

listOfIterations :: [(Sum Int, IVector Double)] -> [Double]
listOfIterations = map
    ( fromIntegral
    . getSum
    . fst)

success :: Double -> [(Sum Int, IVector Double)] -> Bool
success err = any 
    ( (\v -> widthV v <= err && not (isEmptyV v))
    . snd)

garbage :: Double -> [(Sum Int, IVector Double)] -> Bool
garbage err = any
    ( (\v -> widthV v > err && not (isEmptyV v))
    . snd)

dataFn path err fn fn' x0 = do
 writeFile path "n,maximo,media,variancia,sucesso,lixo\n"
 putStrLn $ "\nPrinting Data to " ++ path ++ ".\n"
 putStr "0"
 mapM_ (\n -> do
       let rs = listOfResults err fn fn' x0 n
           suc = success err rs
           garb = garbage err rs
           is = listOfIterations rs
           (ma,me,sd) = maxMeanSd is
       appendFile path $ concat 
           [ show n, ",\t"
           , show ma, ",\t"
           , show me, ",\t"
           , show sd, ",\t"
           , show (fromEnum suc), ",\t"
           , show (fromEnum garb), "\n"]
       let m = n `mod` 10
       when (m == 0) $ putStr $ show n
       when (m `elem` [3,5,7]) $ putChar '.')
       [2..100]
 putStr "\n"

update = do
  let x0 = [-2...2,-2...2]
  dataFn "dataFn0.csv" 1e-10 fN0 fN0' x0
  dataFn "dataFn1.csv" 1e-10 fN1 fN1' x0
  dataFn "dataFn2.csv" 1e-10 fN2 fN2' x0

main :: IO ()
main = getOptions >>= performSafeFs
