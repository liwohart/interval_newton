import System.Environment
import InterComp
import Newton
import Data.Function (on)
import Control.Monad
import Data.Monoid
import Data.List


fs :: [(Interval Double, Double -> Interval Double, Interval Double -> Interval Double, String, Maybe (Interval Double))]
fs =
 [
--
  (1 ... 2,
   \x -> singleton $ x^2 - 2,
   \x -> 2*x,
   "x^2 - 2",
   Just $ singleton $ sqrt 2),
--0 err = 1e-10, steps = 4, answer = 1.4142135623730951 ... 1.4142135623730951
--
  (1 ... 2,
   \x -> singleton $ x^2 - x - 1,
   \x -> 2*x - singleton 1,
   "x^2 - x - 1",
   Just $ singleton $ (1+sqrt 5)/2),
--1 err = 1e-10, steps = 4, answer = 1.6180339887493835 ... 1.6180339887504658
--
  (1 ... 3,
   \x -> singleton $ x^2 - 3,
   \x -> 2*x,
   "x^2 - 3",
   Just $ singleton $ sqrt 3),
--2 err = 1e-10, steps = 4, answer = 1.7320508075645042 ... 1.7320508075739158
--
  ((1 ... 2),
   \x -> singleton $ (x - 1) * x**2,
   \x -> (singleton 3 * x - singleton 2) * x,
   "x^3 - x^2",
   Just $ singleton 1),
--3 err = 1e-10, steps = 6, answer = 1.0 ... 1.000000000000001
--
  ((0.5 ... 2),
   \x -> singleton $ x^2 - 1,
   \x -> 2 * x,
   "x^2 - 1",
   Just $ singleton 1),
--4 err = 1e-10, steps = 5, answer = 1.0 ... 1.0
--
  (2.5 ... 3,
   \x -> singleton $ ((((x + 1) * x - 11) * x - 3) * x + 18) * x,
   \x -> (((5 * x + 4) * x - 33) * x - 6) * x + 18,
   "x^5 + x^4 - 11x^3 - 3x^2 + 18X",
   Nothing),
--5 err = 1e-10, steps = 5, answer = 2.6281878450866234 ... 2.6281878450866234
--
  ((0.75 ... 2),
   \x -> singleton $ (x^2 - 1) * x - 1,
   \x -> (singleton 3) * x!^2 - (singleton 1),
   "x^3 - x - 1",
   Nothing),
--6 err = 1e-10, steps = 5, answer = 1.324717957244746 ... 1.324717957244746
--
  ((2 ... 3),
   \x -> singleton $ (x - 1)^2 + 1,
   \x -> (singleton 2) * x - (singleton 2),
   "x^2 - 2x + 2",
   Just empty),
--7 err = 1e-10, steps = 1, answer = Empty
--
  ((-3 ... (-2)),
   \x -> singleton $ 2 * x^3 + 3 * x^2 - 2 * x + 1,
   \x -> ((singleton 6) * x + (singleton 6)) * x - (singleton 2),
   "2x^3 + 3x^2 - 2x + 8",
   Nothing),
--8 err = 1e-10, steps = 4, answer = -2.092193585704187 ... -2.0921935857034795
--
  ((-0.25 ... 0.5),
   \x -> singleton $ (1 - x^2)*x,
   \x -> (singleton 1) - (singleton 3) * x !^ 2,
   "x - x^3",
   Just $ singleton 0),
--9 err = 1e-10, steps = 3, answer = 0.0 ... 0.0
--
  ((0.75 ... 2),
   \x -> singleton $ x^3 - x,
   \x -> (singleton 3)*(x!^2) - (singleton 1),
   "x^3 - x",
   Just $ singleton 1),
--10 err = 1e-10, steps = 5, answer = 0.9999999999999999 ... 1.0000000000000002
--
  (7 ... 8,
   \x -> singleton $ 0.5*x^2 - 4*x + 1,
   \x -> x - (singleton 4),
   "0.5x^2 - 4x + 1",
   Just $ singleton $ 4 + sqrt 14),
--11 err = 1e-10, steps = 4, answer = 7.741657386773941 ... 7.741657386773941
--
  (1 ... 2,
   \x -> (singleton x) !^ 2 - (2 ... 3),
   \x -> 2*x,
   "x^2 - [2,3]",
   Just $ sqrt 2 ... sqrt 3),
--12 err = 1e-10, steps = 10, answer = 1.4031881762974567 ... 1.7556818181818181
--
  ((1 ... 5),
   \x -> (singleton x) !^ 2 - (4 ... 9),
   \x -> 2 * x,
   "x^2 - [ 4 , 9 ]",
   Just $ 2 ... 3),
--13 err = 1e-10, steps = 3, answer = 2.0 ... 3.0
--
  ((4.5 ... 20),
   \x -> (0.5 ... 3)*(singleton x)^2 + (-4 ... 1)*(singleton x) + (-100 ... (-98)),
   \x -> (1 ... 6)*x + (-4 ... 1),
   "[0.5,3]x^2 + [-4,1]x + [-100,-98]",
   Nothing),
--14 err = 1e-10, steps = 1, answer = 4.5 ... 20.0
--
  ((-2 ... 2),
   \x -> (1 ... 2)*(singleton x) !^ 3,
   \x -> (3 ... 6)*x !^ 2,
   "[1,2]x^3",
   Nothing),
--15 err = 1e-10, steps = 0
--
  (singleton 20 * (-10 ... 14),
   \x -> (1 ... 4)*(singleton x) + (-4 ... 1),
   const (1 ... 4),
   "[1,4]x + [-4,1]",
   Just $ (-1 ... 4)),
--16 err = 1e-10, steps = 2, answer = -2.0 ... 4.0
--
  ((5 ... 8),
   \x -> (0.5 ... 3)*(singleton x)^2 + (-4 ... 1)*(singleton x) + (1 ... 3),
   \x -> (1 ... 6)*x + (-4 ... 1),
   "[0.5,3]x^2 + [-4,1]x + [1,3]",
   Just $ 5 ... (4 + sqrt 14)),
--17 err = 1e-10, steps = 2, answer = 5.0 ... 8.375
--
  ((-3 ... 8),
   \x -> (singleton x) + (-2...2),
   \x -> singleton 1,
   "x + [-2,2]",
   Just $ -2...2)]
--18 err = 1e-10, steps = 2, answer = 5.0 ... 8.375

err :: Double
err = 1e-10

printList :: Show a => [a] -> IO ()
printList = endl <=< sequence_ . map print 
 where endl = const $ putStrLn ""

answers :: [Maybe (Int, Interval Double)]
answers = map ( fmap ((,) <$> getSum . fst <*> snd) . flip performSafeNewtonFs err) [0..length fs - 1]

roots :: [Maybe (Interval Double)]
roots = map (\(_,_,_,_,m) -> m) fs

funcs :: [String]
funcs = map (\(_,_,_,s,_) -> s) fs

filteredFuncs = filter (\i ->
   case dists!!i of
    Just _  -> True
    Nothing -> False) [0..length fs - 1]

inters :: [Interval Double]
inters = map (\(i,_,_,_,_) -> i) fs

dists :: [Maybe Double]
dists = zipWith (\x y -> distance <$> (snd <$> x) <*> y) answers roots



performNewtonFs :: Int -> Double -> (Sum Int, Interval Double)
performNewtonFs idx err = newton err f df inter
 where (inter, f, df, _, _) = fs!!idx

performNewtonMultFs :: Int -> Double -> (Sum Int, [Interval Double])
performNewtonMultFs idx err = sequence $ newtonMult err f df inter
 where (inter, f, df, _, _) = fs!!idx

performSafeNewtonFs :: Int -> Double -> Maybe (Sum Int, Interval Double)
performSafeNewtonFs idx err = safeNewton err f df inter
 where (inter, f, df, _, _) = fs!!idx

-- show function versions

performNewtonShowFs :: Int -> Double -> IO ()
performNewtonShowFs idx err = newtonShow err f df strF inter
 where (inter, f, df, strF, _) = fs!!idx

performNewtonMultShowFs :: Int -> Double -> IO ()
performNewtonMultShowFs idx err = newtonMultShow err f df strF inter
 where (inter, f, df, strF, _) = fs!!idx

performSafeNewtonShowFs :: Int -> Double -> IO ()
performSafeNewtonShowFs idx err = safeNewtonShow err f df strF inter
 where (inter, f, df, strF, _) = fs!!idx

-- interact function versions

performFs :: (Int, Double, Bool) -> IO ()
performFs (idx, err, True) = performNewtonShowFs idx err
performFs (idx, err, False) = print $ performNewtonFs idx err
{-# INLINE performFs #-}

performMultFs :: (Int, Double, Bool) -> IO ()
performMultFs (idx, err, True) = performNewtonMultShowFs idx err
performMultFs (idx, err, False) = print $ performNewtonMultFs idx err
{-# INLINE performMultFs #-}

performSafeFs :: (Int, Double, Bool) -> IO ()
performSafeFs (idx, err, True) = performSafeNewtonShowFs idx err
performSafeFs (idx, err, False) = print $ performSafeNewtonFs idx err
{-# INLINE performSafeFs #-}


parse :: [String] -> IO (Int, Double, Bool)
parse [strIdx, strErr, showBool] = return (idx, err, (showBool ==) `any` ["-s","--show"])
 where idx = read strIdx
       err = read strErr
parse [strIdx,strErr] = return (idx, err, False)
 where idx = read strIdx
       err = read strErr
parse [showBool] = return (0, 0, (showBool ==) `any` ["-s","--show"])
parse _ = return (0, 0, False)


pF :: (a -> b) -> (a -> c) -> a -> (b, c)
pF f g = (,) <$> f <*> g

-- roundTo :: Fractional a => Int -> a -> a
roundTo c = flip (/) (10^c) . fromIntegral . round . (*) (10^c)
{-# INLINE roundTo #-}


roundI c i = monotonic (roundTo c) i
{-# INLINE roundI #-}

showI c interval
 | isEmpty interval = show  interval
 | width interval == 0 = show $ roundTo 3 $ inf interval
 | otherwise = "["++i++","++s++"]"
 where i = show $ inf $ roundI 3 interval
       s = show $ sup $ roundI 3 interval
{-# INLINE showI #-}


main :: IO ()
main = getArgs >>= parse >>= performSafeFs
