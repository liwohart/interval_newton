import System.Console.GetOpt
import Refinement
import InterComp
import Newton
import Parsing
import Fs
import Performs


pF :: (a -> b) -> (a -> c) -> a -> (b, c)
pF f g = (,) <$> f <*> g
{-# INLINE pF #-}

roundTo c = flip (/) (10^c) . fromIntegral . round . (*) (10^c)
{-# INLINE roundTo #-}

roundI c i = monotonic (roundTo c) i
{-# INLINE roundI #-}


main :: IO ()
main = getOptions >>= performSafeFs
