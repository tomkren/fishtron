module Problems.BA.Problem where

import Problems.Utils ( boolListProblem2 )

import Problems.Utils 


mainProblem = boolListProblem2 "ba" (33,33,33) 100 ff





ff :: [Bool] -> Double
ff bits = 
 let dLen = fromIntegral . length
     tNum = dLen . filter (\(n,b)->if n `mod` 2 == 0 then b else not b) $ zip [0..] bits 
     len  = dLen $ bits
  in 100*(tNum * tNum)/(len*len)



-- mainProblem = mkProblem $ PO_BLP OBoolListProblem {
--   blp_numRuns     = OIntSlider 1 10   4   1   ,
--   blp_numGene     = OIntSlider 0 100  10  10  ,
--   blp_popSize     = OIntSlider 0 5000 500 10  ,
--   blp_length      = OIntSlider 0 1000 100 10  ,
--   blp_problemName = "ba"                      ,
--   blp_ff          = ff
-- }