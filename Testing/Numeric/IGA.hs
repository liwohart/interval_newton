{- Future better implemantation
 - for the Interval Gaussian Algorithm-}
module Numeric.IGA where

import Data.Matrix
import Numeric.Interval

type Vector a = Matrix a
type Joined a = Matrix a

rVec :: Int -> [a] -> Vector a
rVec = fromList 1

makeJoined :: Matrix a -> Vector a -> Matrix a
makeJoined = (<|>)

cVec :: Int -> [a] -> Vector a
cVec = flip fromList 1

ga :: Fractional a => Joined a -> Joined a
ga = undefined

a :: Matrix Double
a = fromList 3 3 [8,1,2,3,4,8,4,7,2]

b :: Vector Double
b = cVec 3 [1,4,2]
