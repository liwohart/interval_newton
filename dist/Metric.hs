{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


class Num a => Measure a where
 fromMeasure :: Num b => a -> b
 fromMeasure = id

class Metric a where
 distance :: Measure b => a -> a -> b

instance ( a) => Metric [a] where
 distance x y = sum $ zipWith (((^2) .) . distance) x y 
