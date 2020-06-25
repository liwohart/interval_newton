import System.Process (system)
import System.Console.GetOpt
import Refinement
import InterComp
import Newton
import Parsing
import Fs
import Performs
import Fuck

main :: IO ()
main = do
 opts <- getOptions
 system "cls"
 performSafeFs opts
