import System.Process (system)
import System.Console.GetOpt
import Data.Monoid (Sum(..),getSum)
import Data.Maybe (fromMaybe)
import Refinement
import InterComp
import Newton
import Parsing
import Fs
import Performs
import Fuck (discoPoint, domInterval, part, partPi2, g, r, v2)

main :: IO ()
main = getOptions >>= performSafeFs
