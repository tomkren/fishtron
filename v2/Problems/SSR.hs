module Problems.SSR where

import TTerm (Typ(..),Context)

import Problems.Utils ( cttProblem3 )


problem = cttProblem3 "ssr3" "Problems.SSR" dou1 ctx


dou, dou1, dou2 :: Typ
dou  = Typ "Double"
dou1 = dou :-> dou
dou2 = dou :-> dou :-> dou

ctx :: Context
ctx = [("(+)",dou2),("(-)",dou2),("(*)",dou2),("rdiv",dou2),("sin",dou1),("cos",dou1),("exp",dou1),("rlog",dou1)]

ff :: (Double->Double) -> (Double,Bool)
ff prog = ( (1/) . (1+) . sum $ diffs , 20 == (length . filter (\dif-> (abs dif) < 0.01 ) $ diffs)  )  
 where 
  diffs = map (\x-> let dx = (prog x) - ( x*x*x*x+x*x*x+x*x+x ) in abs dx ) (linInterval (-1,1) 20)
  linInterval :: (Double,Double) -> Int -> [Double]
  linInterval (a,b) n = 
   let step = (b-a) / (fromIntegral $ n-1)
    in [ a + (fromIntegral i)*step | i <- [0..(n-1)] ]



rdiv :: Double -> Double -> Double
rdiv _ 0 = 1
rdiv x y = x / y

rlog :: Double -> Double
rlog 0 = 0
rlog x = log $ abs x
 