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

