module Fs (fs) where

import InterComp
import Fuck

fs :: (Floating a, Ord a) =>
      [(Interval a, a -> Interval a, Interval a -> Interval a, String, Maybe (Interval a))]
fs =
 [

--0
  (1 ... 2,
   \x -> singleton $ x^2 - 2,
   \x -> 2*x,
   "x^2 - 2",
   Just $ singleton $ sqrt 2),


--1
  (1 ... 2,
   \x -> singleton $ x^2 - x - 1,
   \x -> 2*x - singleton 1,
   "x^2 - x - 1",
   Just $ singleton $ (1+sqrt 5)/2),


--2
  (1 ... 3,
   \x -> singleton $ x^2 - 3,
   \x -> 2*x,
   "x^2 - 3",
   Just $ singleton $ sqrt 3),


--3
  ((1 ... 2),
   \x -> singleton $ (x - 1) * x**2,
   \x -> (singleton 3 * x - singleton 2) * x,
   "x^3 - x^2",
   Just $ singleton 1),


--4
  ((0.5 ... 2),
   \x -> singleton $ x^2 - 1,
   \x -> 2 * x,
   "x^2 - 1",
   Just $ singleton 1),


--5
  (2.5 ... 3,
   \x -> singleton $ ((((x + 1) * x - 11) * x - 3) * x + 18) * x,
   \x -> (((5 * x + 4) * x - 33) * x - 6) * x + 18,
   "x^5 + x^4 - 11x^3 - 3x^2 + 18X",
   Nothing),


--6
  ((0.75 ... 2),
   \x -> singleton $ (x^2 - 1) * x - 1,
   \x -> (singleton 3) * x!^2 - (singleton 1),
   "x^3 - x - 1",
   Nothing),


--7
  ((2 ... 3),
   \x -> singleton $ (x - 1)^2 + 1,
   \x -> (singleton 2) * x - (singleton 2),
   "x^2 - 2x + 2",
   Just empty),


--8
  ((-3 ... (-2)),
   \x -> singleton $ 2 * x^3 + 3 * x^2 - 2 * x + 1,
   \x -> ((singleton 6) * x + (singleton 6)) * x - (singleton 2),
   "2x^3 + 3x^2 - 2x + 8",
   Nothing),


--9
  ((-0.25 ... 0.5),
   \x -> singleton $ (1 - x^2)*x,
   \x -> (singleton 1) - (singleton 3) * x !^ 2,
   "x - x^3",
   Just $ singleton 0),


--10
  ((0.75 ... 2),
   \x -> singleton $ x^3 - x,
   \x -> (singleton 3)*(x!^2) - (singleton 1),
   "x^3 - x",
   Just $ singleton 1),


--11
  (7 ... 8,
   \x -> singleton $ 0.5*x^2 - 4*x + 1,
   \x -> x - (singleton 4),
   "0.5x^2 - 4x + 1",
   Just $ singleton $ 4 + sqrt 14),


--12
  (1 ... 2,
   \x -> (singleton x) !^ 2 - (2 ... 3),
   \x -> 2*x,
   "x^2 - [2,3]",
   Just $ sqrt 2 ... sqrt 3),


--13
  ((1 ... 5),
   \x -> (singleton x) !^ 2 - (4 ... 9),
   \x -> 2 * x,
   "x^2 - [ 4 , 9 ]",
   Just $ 2 ... 3),


--14
  ((4.5 ... 20),
   \x -> (0.5 ... 3)*(singleton x)^2 + (-4 ... 1)*(singleton x) + (-100 ... (-98)),
   \x -> (1 ... 6)*x + (-4 ... 1),
   "[0.5,3]x^2 + [-4,1]x + [-100,-98]",
   Nothing),


--15
  ((-2 ... 2),
   \x -> (1 ... 2)*(singleton x) !^ 3,
   \x -> (3 ... 6)*x !^ 2,
   "[1,2]x^3",
   Nothing),


--16
  (singleton 20 * (-10 ... 14),
   \x -> (1 ... 4)*(singleton x) + (-4 ... 1),
   const (1 ... 4),
   "[1,4]x + [-4,1]",
   Just $ (-1 ... 4)),


--17
  ((5 ... 8),
   \x -> (0.5 ... 3)*(singleton x)^2 + (-4 ... 1)*(singleton x) + (1 ... 3),
   \x -> (1 ... 6)*x + (-4 ... 1),
   "[0.5,3]x^2 + [-4,1]x + [1,3]",
   Just $ 5 ... (4 + sqrt 14)),


--18
  ((-3 ... 8),
   \x -> (singleton x) + (-2...2),
   \x -> singleton 1,
   "x + [-2,2]",
   Just $ -2...2),

--19
  (domInterval 1,
   f,
   f',
   strF,
   Nothing)


 ]
