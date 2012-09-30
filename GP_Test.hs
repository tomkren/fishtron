module GP_Test where

import GP_Core
import GP_Data

import Util
import KozaTree

-- Test Runs ------------------------------------------------------------

run_boolAlternate = run pro_boolAlternate
run_kozaSSR       = run pro_kozaSSR

bug1 = runWith "195915681 1"          pro_kozaSSR
bug2 = runWith "709804832 1549792035" pro_kozaSSR -- compilovaný dělá něco jinyho než interpretovaný ?!?

speedTest = runWith "1598308484 1" pro_kozaSSR

-- Problems & Fitnes Functions -------------------------------------------


pro_boolAlternate = boolListProblem 5 50 (33,33,33) 100 ff_boolAlternate

ff_boolAlternate :: [Bool] -> FitVal
ff_boolAlternate bits = 
 let tNum = dLen . filter (\(n,b)->if n `mod` 2 == 0 then b else not b) $ zip [0..] bits 
     len  = dLen $ bits
  in 100*(tNum * tNum)/(len*len)



pro_kozaSSR = kozaProblem ff_kozaSSR env_kozaSSR

env_kozaSSR :: KEnv
env_kozaSSR = (["x"],[("plus",2),("minus",2),("krat",2),("rdiv",2),("sin",1),("cos",1),("exp",1),("rlog",1)])
 
ff_lol :: FitFun KTree ()
ff_lol = mkFF1 $ return . fromIntegral . kSize 

ff_kozaSSR :: FitFun KTree (Double->Double)
ff_kozaSSR = FF2 toStr (asType::Double->Double) (return . ff)
 where
  toStr :: KTree -> String
  toStr tree = "(\\ x -> " ++ show tree ++ ")"
  ff :: (Double->Double) -> FitVal
  ff f = (1/) . (1+) . sum . map (\x-> let dx = (f x) - ( x*x*x*x+x*x*x+x*x+x ) in abs dx ) $ [-1,-0.9..1] 


-- utils ---------------------------------------------------------------

type KozaProblem a   = Problem KTree  a  KTreeGen          KTreeMut          KTreeCro
type BoolListProblem = Problem [Bool] () (ListGen BoolGen) (ListMut BoolMut) (ListCro () )

kozaProblem :: FitFun KTree a -> KEnv -> KozaProblem a
kozaProblem ff env = 
 let popSize    = 500
     numGene    = 51
     genOpProbs = (10,0,90)
     gOpt       = KG_Koza env
     mOpt       = KM_Koza env
     cOpt       = KC_Koza 
  in Problem popSize numGene (mkGenOps (mOpt,cOpt) genOpProbs) gOpt mOpt cOpt ff



boolListProblem :: PopSize -> NumGene -> GenOpProbs -> Len -> ([Bool]->FitVal) -> BoolListProblem
boolListProblem popSize numGene genOpProbs len ff = 
 let gOpt   = (LG_ BG_ len)
     mOpt   = (LM_OnePoint BM_Not len )
     cOpt   = (LC_OnePoint () len)
     genOps = mkGenOps (mOpt,cOpt) genOpProbs
     fitFun = mkFF1 $ return . ff
  in Problem popSize numGene genOps gOpt mOpt cOpt fitFun

dLen :: [a] -> Double
dLen = fromIntegral . length 
