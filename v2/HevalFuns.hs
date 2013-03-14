module HevalFuns where

import Ant

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

-- ant :


--l   :: AAnt   
--l = toAAnt doLeft
--
--r   :: AAnt
--r = toAAnt doRight
--
--m   :: AAnt                        
--m = toAAnt doMove
--
--ifa :: AAnt -> AAnt -> AAnt        
--ifa = toAAnt2 ifFoodAhead
--
--p2  :: AAnt -> AAnt -> AAnt 
--p2 = toAAnt2 progn2
--
--p3  :: AAnt -> AAnt -> AAnt -> AAnt
--p3 = toAAnt3 progn3

l   :: Ant   
l = doLeft

r   :: Ant
r = doRight

m   :: Ant                        
m = doMove

ifa :: Ant -> Ant -> Ant        
ifa = ifFoodAhead

p2  :: Ant -> Ant -> Ant 
p2 =  progn2

p3  :: Ant -> Ant -> Ant -> Ant
p3 =  progn3

ffAnt :: Ant -> (Double,Bool)
ffAnt ant =
 let eaten = fromIntegral $ evalAnt antWorld ant
  in ( eaten , eaten == 89 )

--- ... self problem(s)

listCase :: [a] -> b -> (a->[a]->b) -> b
listCase as b1 b2 = case as of
 []   -> b1
 x:xs -> b2 x xs 

ff_head :: ( [Int] -> Maybe Int ) -> (Double,Bool)
ff_head prog = 
  let ffval = ( if prog []       == Nothing then 1 else 0 ) +
              ( if prog [1]      == Just 1  then 1 else 0 ) +
              ( if prog [42,7,3] == Just 42 then 1 else 0 )
   in (ffval,ffval == 3)
