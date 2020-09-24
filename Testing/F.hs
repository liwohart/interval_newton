module Testing.F where

import Testing.Numeric.InterComp       hiding (scale)
import Testing.Numeric.Newton (newton)

g, r, v2, theta, disc :: Floating a => a
g      = 9.81
r      = 6371000.0
v2     = 8840.0^2
theta  = 80/180*pi
disc   = acos $ sqrt $ g*r/v2

x00 = [1.5...3.5,-6...0]
fN0 :: (RealFloat a, Ord a) => Vector a -> IVector a
fN0 [x1,x2] = map singleton [4*x1 - x1^3 + x2
                            ,-x1^2/9 + (4*x2 - x2^2)/4 + 1]

fN0' :: (RealFloat a, Ord a) => IVector a -> IMatrix a
fN0' [x1,x2] = [[4 - 3*x1!^2
                ,singleton 1]
               ,[-(2/9)*x1
                ,(2 - x2)/2]]

x01 = [1.0...1.5,1.0...1.5]
fN1 :: (RealFloat a, Ord a) => Vector a -> IVector a
fN1 [x1,x2] = map singleton [x1^2 + x2^2 - 2
                            ,exp (x1 - 1) + x2^3 - 2]

fN1' :: (RealFloat a, Ord a) => IVector a -> IMatrix a
fN1' [x1,x2] = [[2*x1
                ,2*x2]
               ,[exp (x1 - 1)
                ,3*x2!^2]]

x02 = [1.0...1.8,1.5...2.5]
fN2 :: (RealFloat a, Ord a) => Vector a -> IVector a
fN2 [x1,x2] = map singleton [3*x1^2*x2 - x2^3 - 4
                            ,x1^2 + x1*x2^3 - 9]

fN2' :: (RealFloat a, Ord a) => IVector a -> IMatrix a
fN2' [x1,x2] = [[6*x1*x2
                ,3*x1!^2 - 3*x2!^2]
               ,[2*x1 + x2!^3
                ,3*x1*x2!^2]]

--[...,...]
fN3 :: (RealFloat a, Ord a) => Vector a -> IVector a
fN3 l@[x1,x2] = fN0 l !+! [-1...1,-1...1]

fN3' :: (RealFloat a, Ord a) => IVector a -> IMatrix a
fN3' = fN0'

f :: (Floating a, Ord a) => a -> Interval a
f x = singleton $ first/second - tan(theta/2)
  where sx     = sin x
        cx     = cos x
        first  = sx * cx
        second = g*r/v2 - cx^2

f' :: (Floating a, Ord a) => Interval a -> Interval a
f' x | k     <- g * r / v2
     , sk    <- singleton k
     , s2km1 <- singleton $ 2*k - 1
     , ic2x  <- icos x!^2
     = (s2km1*ic2x - sk)/((sk - ic2x)!^2)

discoPoint :: Floating a => Int -> a
discoPoint n = (-1)^((n `mod` 2) + 1) * disc + fromIntegral (n `div` 2) * pi

domInterval' :: (Ord a, Floating a) => Int -> Interval a
domInterval' n = discoPoint n ... discoPoint (n + 1)

domInterval :: (Ord a, Floating a) => Int -> Interval a
domInterval n = let m = n `div` 2 in domInterval' (n - m) `intersection` partPi2 m

part n = fromIntegral n * pi ... fromIntegral (n + 1) * pi
partPi2 n | pi2 <- pi/2 = fromIntegral n * pi2 ... fromIntegral (n + 1) * pi2

root :: (Ord a, Floating a) => Int -> a
root = midpoint . snd . newton 0 f f' . deflate 1e-4 . domInterval . (+1) . (2*)

strF :: String
strF = "(sin(x) cos(x))/(gR/v2 - cos2(x)) - tan(2pi/9)"
