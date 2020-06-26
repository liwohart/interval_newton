import System.Process (system)
import System.Console.GetOpt
import Refinement
import InterComp
import Newton
import Parsing
import Fs
import Performs
import Fuck (part)

(x0, f, f', strF, _) = fs!!19

main :: IO ()
main = getOptions >>= performSafeFs
