module Problems.SSR.Funs where




rdiv :: Double -> Double -> Double
rdiv _ 0 = 1
rdiv x y = x / y

rlog :: Double -> Double
rlog 0 = 0
rlog x = log $ abs x

ff :: (Double->Double) -> (Double,Bool)
ff prog = ( (1/) . (1+) . sum $ diffs , 20 == (length . filter (\dif-> (abs dif) < 0.01 ) $ diffs)  )  
 where 
  diffs = map (\x-> let dx = (prog x) - ( x*x*x*x+x*x*x+x*x+x ) in abs dx ) (linInterval (-1,1) 20)
  linInterval :: (Double,Double) -> Int -> [Double]
  linInterval (a,b) n = 
   let step = (b-a) / (fromIntegral $ n-1)
    in [ a + (fromIntegral i)*step | i <- [0..(n-1)] ]
 