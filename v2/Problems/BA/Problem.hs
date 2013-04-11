module Problems.BA.Problem where

import Problems.Utils ( boolListProblem2 )


mainProblem = boolListProblem2 "ba" (33,33,33) 100 ff


ff :: [Bool] -> Double
ff bits = 
 let tNum = dLen . filter (\(n,b)->if n `mod` 2 == 0 then b else not b) $ zip [0..] bits 
     len  = dLen $ bits
  in 100*(tNum * tNum)/(len*len)


dLen :: [a] -> Double
dLen = fromIntegral . length 
