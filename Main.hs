import System.Process (system)
import qualified Control.Monad.HT as M
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

addVtoSub :: (Fractional a, Ord a) => [Interval a] -> [IVector a] -> [IVector a]
addVtoSub xs vs = [ x:v |  x <- xs, v <- vs]

subdivN :: (Fractional a, Ord a) => Int -> IVector a -> [IVector a]
subdivN n x = foldr addVtoSub [[]] $ map (subdivide n) x

fe :: Ord a => [IVector a] -> [IVector a]
fe = filter (not . isEmptyV)

a f = midV . f . midV

main :: IO ()
main = getOptions >>= performSafeFs
