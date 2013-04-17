module Problems.BA.Problem where

import Problems.Utils ( boolListProblem2 )
import Problems.Utils 
import Text.JSON


reg :: ProblemOpts a
reg = PO_BLP_ PO_BLP {
  blp_code        = "ba"                                         ,
  blp_info        = "Bool Alternate"                             ,
  blp_data        = JSNull                                       ,
  blp_numRuns     = IntSlider "Runs"            1 10    4    1   ,
  blp_numGene     = IntSlider "Generations"     0 100   10   10  ,
  blp_popSize     = IntSlider "Population size" 0 50000 5000 100 ,
  blp_length      = IntSlider "Genom length"    0 1000  200  10  ,
  blp_ff          = ff
}



mainProblem_ = boolListProblem2 "ba" (33,33,33) 100 ff





ff :: [Bool] -> Double
ff bits = 
 let dLen = fromIntegral . length
     tNum = dLen . filter (\(n,b)->if n `mod` 2 == 0 then b else not b) $ zip [0..] bits 
     len  = dLen $ bits
  in 100*(tNum * tNum)/(len*len)


