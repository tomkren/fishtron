module Problems.Ant.Funs where

import Problems.Ant.Ant

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

ff :: Ant -> (Double,Bool)
ff ant =
 let eaten = fromIntegral $ evalAnt antWorld ant
  in ( eaten , eaten == 89 )