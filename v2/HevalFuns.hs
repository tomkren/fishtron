module HevalFuns where

plus :: Num a => a -> a -> a
plus = (+)

krat :: Num a => a -> a -> a
krat = (*)

minus :: Num a => a -> a -> a
minus = (-)

rdiv :: Double -> Double -> Double
rdiv _ 0 = 1
rdiv x y = x / y

rlog :: Double -> Double
rlog 0 = 0
rlog x = log $ abs x

s :: (a->b->c) -> (a->b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x y = x

i :: a -> a
i x = x