module Main () where

import Base
import Util
import Dist
import Evaluation
import Parser
import Prover
import GP
import Enviroments

import Ant

import Debug.Trace

import Data.Maybe
import Data.Dynamic
import System.Random


-- test cases ------------------------------------------------------------------

testCase5 = Ant.testCaseAnt
testCase4 = putList $ map distMax $ gp (mkStdGen 424248)   (dk' env3) (listInt:->int)   (ffSum [0..10] )                  20  10
testCase3 = putList $ map distMax $ gp (mkStdGen 42)       (dk' env2) (real:->real)     (ffInterp (\x->x*x+x+1) [0..10] ) 500 10
testCase2 = putList $ map distMax $ gp (mkStdGen 42424242) (dk' env1) (int:->int:->int) ff2                               200 10
testCase1 = putList $ map distMax $ gp (mkStdGen 42424242) (dk' env1) (int:->int)       ff1                               50  10

testCase1B= putList $ map distMax $ gp (mkStdGen 42) (dk2 en1 (1,1)) (int:->int) ff1 50  10


--testCase5 = Ant.testCaseAnt
--testCase4 = putList $ map distMax $ gp  (mkStdGen 424248) env3 (listInt:->int) (ffSum [0..10] ) 20 10
--testCase3 = putList $ map distMax $ gp  (mkStdGen 42) env2 (real:->real) (ffInterp (\x->x*x+x+1) [0..10] ) 500 10
--testCase2 = putList $ map distMax $ gp  (mkStdGen 42424242) env1 (int:->int:->int) ff2 200 10
--testCase1 = putList $ map distMax $ gp  (mkStdGen 42424242) env1 (int:->int) ff1 50 10

--------------------------------------------------------------------------------
ff1 :: TTerm -> FitVal
ff1 f =  case (compute 100 $ f @@ (42::Int) )::Either TTerm Int of
  Left  _ -> 0
  Right i -> fromIntegral $ abs i

ff2 :: TTerm -> FitVal
ff2 f =  case (compute 100 $ f @@ (42::Int) @@ ((84::Int)) )::Either TTerm Int of
  Left  _ -> 0
  Right i -> fromIntegral $ abs i

ffInterp :: (Double->Double) -> [Double] -> TTerm -> FitVal
ffInterp q xs f = 1 / ( 1 + err )
  where 
  err = sum $ map (interpErr f) $ map (\ x -> ( x , q x ) ) xs 
  interpErr :: TTerm -> (Double,Double) -> Double
  interpErr f (x,y) = case (compute 20 $ f @@ x )::Either TTerm Double of
    Left  _  -> 9999999
    Right y' -> let dy = y - y' in dy*dy

ffSum :: [Int] -> TTerm -> FitVal
ffSum xs f = 1 / ( 1 + fromIntegral ( ffSumErr xs f ) )

ffSumErr :: [Int] -> TTerm -> Int
ffSumErr xs f = case (compute 300 $ f @@ xs )::Either TTerm Int of
  Left  _ -> 9999999
  Right i -> abs $ (sum xs) - i

-- (\(Right (Val _ dyn _))->fromDynamic dyn :: Maybe Int)$ nf beta 4 $ (fromJust 
-- $ parseTTerm env3 $ "foldr + 0") @@ ([1,2,3]::[Int])










