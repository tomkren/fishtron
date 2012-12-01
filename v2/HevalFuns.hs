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

if' :: Bool -> a -> a -> a
if' p x y = if p then x else y

head' :: a -> [a] -> a
head' defa [] = defa
head' _ (x:_) = x

tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs

xor :: Bool -> Bool -> Bool
xor x y = if x then not y else y

nand :: Bool -> Bool -> Bool
nand x y = not (x && y)

nor :: Bool -> Bool -> Bool
nor x y = not (x || y)


s :: (a->b->c) -> (a->b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x y = x

i :: a -> a
i x = x