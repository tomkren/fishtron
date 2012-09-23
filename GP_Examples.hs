
module GP_Examples where

import GP_Classes   ( Evolvable, Problem(Problem), FitFun2(FF2), PopSize, FitVal, Credit,
                      mkEOpt, mkFF1, putEvolveMaximas, evolveIt )
import GP_Instances 

import Util     ( Rand )
import Dist     ( Dist, distTake_new, distSize, distMax )
import KozaTree ( KTree )
import Heval    ( as )

import System.Random       (mkStdGen)
import Control.Monad.State (runState)

--------------------------------------

type BoolListProblem = Problem [Bool] () (ListGen BoolGen) (ListMut BoolMut) (ListCro () )

boolListProblem :: (Double,Double,Double) -> PopSize -> Len -> ([Bool]->FitVal) -> BoolListProblem
boolListProblem eParams popSize len ff = 
  Problem popSize (mkEOpt eParams) (LG_ BG_ len) (LM_OnePoint BM_Not len ) (LC_OnePoint () len) (mkFF1 $ return . ff)  ()
  


test       = putEvolveMaximas 1000 $ boolListProblem (   33,   33,   33) 20 100 ff2 -- 49    , 56 
testByMeta = putEvolveMaximas 1000 $ boolListProblem ( 1.93,94.85, 3.22) 2  100 ff2 --       , 98.01
testByMeta2= putEvolveMaximas 1000 $ boolListProblem (8.921,87.83,3.245) 2  100 ff2 -- 98.01 , 90
testByMeta3= putEvolveMaximas 1000 $ boolListProblem (5.040,94.56,0.396) 2  100 ff2 -- 
testByMeta4= putEvolveMaximas 1000 $ boolListProblem (20.39,74.26,5.341) 3  100 ff2
testByMeta5= putEvolveMaximas 1000 $ boolListProblem (6.962,88.36,4.680) 2  100 ff2 -- 96.04
testByMeta6= putEvolveMaximas 1000 $ boolListProblem (0.489,98.82,0.692) 2  100 ff2
testExtrapo= putEvolveMaximas 1000 $ boolListProblem (0    ,  100,    0) 2  100 ff2

normalize :: Credit -> [Double] -> ( (Double,Double,Double) , Int )
normalize credit [rep,mut,cro,gSize,gNum] =
 let q = 100.0 / (rep+mut+cro)
     popSize = round . sqrt $ credit * (gSize / gNum)
  in ( (rep*q,mut*q,cro*q) , popSize )


test2_1 = test2 ( (   33,   33,   33) , 20 ) -- 46, 47, 41, 49
test2_2 = test2 ( (5.819,85.73,8.442) ,  2 ) -- 77, 69, 64, 71
test2_3 = test2 ( (7.827,87.79,4.378) ,  3 ) -- 74, 72, 77, 69, 62, 69

test2 ( ePars , popSize ) = 
 let len     = 100
     eOpt    = mkEOpt ePars
     gOpt    = LG_         (PG_Both BG_    (DG_Normal (0,1)) ) len
     mOpt    = LM_OnePoint (PM_Both BM_Not (DM_Normal (0,1)) ) len
     cOpt    = LC_OnePoint ()                                  len 
  in putEvolveMaximas 500 $ Problem popSize eOpt gOpt mOpt cOpt (mkFF1 $ return . ff3) ()

metaTest2 = 
 let len      = 100
     gOpt     = LG_         (PG_Both BG_    (DG_Normal (0,1)) ) len
     mOpt     = LM_OnePoint (PM_Both BM_Not (DM_Normal (0,1)) ) len  
     cOpt     = LC_OnePoint ()                                  len
     ff       = mkFF1 $ return . ff3
     inCredit = 500
  in putEvolveMaximas 25000 $ metaProblem (gOpt,mOpt,cOpt,ff,(),inCredit)


testFF4 = 
 let popSize = 20
     len     = 2
     eOpt    = mkEOpt (33,33,33)
     gOpt    = DiG_Uniform (DG_Normal (0,1) ) len
     mOpt    = DiM_ ( DM_Normal (0,1) )
     cOpt    = DiC_OnePoint ()
  in putEvolveMaximas 20000 $ Problem popSize eOpt gOpt mOpt cOpt (mkFF1 ff4') () -- (return . ff4)

type MetaProblem = Problem [Double] () (ListGen DoubleGen) (ListMut DoubleMut) (ListCro ())
type MetaParams t a g m c = (g,m,c,FitFun2 t a,a,Credit) 


metaTest = 
 let len      = 100
     gOpt     = LG_ BG_ len  
     mOpt     = LM_OnePoint BM_Not len  
     cOpt     = LC_OnePoint () len
     ff       = mkFF1 $ return . ff2
     inCredit = 500
  in putEvolveMaximas 25000 $ metaProblem (gOpt,mOpt,cOpt,ff,(),inCredit)

metaProblem :: Evolvable t a g m c => MetaParams t a g m c -> MetaProblem
metaProblem params =
 let popSize = 20
     len     = 5 -- == length [rep,mut,cro,gSize,gNum] 
     eOpt    = mkEOpt (33,33,33)
     gOpt    = LG_ ( DG_Uniform (0,1) ) len
     mOpt    = LM_OnePoint ( DM_NormalAbs (0,1) ) len
     cOpt    = LC_OnePoint () len
  in Problem popSize eOpt gOpt mOpt cOpt (metaFF params) () 
 
metaFF :: Evolvable t a g m c => MetaParams t a g m c -> FitFun2 [Double] () -- [Double] -> Rand FitVal
metaFF (gOpt,mOpt,cOpt,ff,a,credit) = mkFF1 $ \ [rep,mut,cro,gSize,gNum] -> 
 let innerProblem = 
      let ratio   = gSize / gNum
          popSize = round . sqrt $ credit * ratio
          eOpt    = mkEOpt (rep,mut,cro)
       in Problem popSize eOpt gOpt mOpt cOpt ff a 
  in do 
   ds <- evolveIt innerProblem credit
   if null ds
    then return 0
    else let Just (_,ffVal) = distMax . last $ ds
          in return ffVal


testKoza ff env as = 
 let eOpt    = mkEOpt (10,0,90)
     popSize = 500
     gOpt    = KG_Koza env
     mOpt    = KM_Koza env
     cOpt    = KC_Koza 
  in putEvolveMaximas 25000 $ Problem popSize eOpt gOpt mOpt cOpt ff as


koza_ssr = testKoza ff_koza_ssr env_koza_ssr (as::Double->Double)

env_koza_ssr :: KEnv
env_koza_ssr = (["x"],[("plus",2),("minus",2),("krat",2),("rdiv",2),("sin",1),("cos",1),("exp",1),("rlog",1)])

ff_koza_ssr :: FitFun2 KTree (Double->Double)
ff_koza_ssr = FF2 toStr (as::Double->Double) (return . ff)
 where
  toStr :: KTree -> String
  toStr tree = "(\\ x -> " ++ show tree ++ ")"
  ff :: (Double->Double) -> FitVal
  ff f = (1/) . (1+) . sum . map (\x-> let dx = (f x) - ( x*x*x*x+x*x*x+x*x+x ) in dx*dx ) $ [-1,-0.9..1] 



ff_Koza0 :: FitFun2 KTree Double
ff_Koza0 = FF2 show (as::Double) (\i -> return $ 1/ (1+abs (123 - i)) )


ff1 :: [Bool]-> FitVal
ff1 bits = let tNum = dLen . filter id $ bits in tNum * tNum

ff2 :: [Bool] -> FitVal
ff2 bits = let tNum = dLen . filter (\(n,b)->if n `mod` 2 == 0 then b else not b) $ zip [0..] bits 
               len  = dLen $ bits
            in 100*(tNum * tNum)/(len*len)

ff3 :: [(Bool,Double)] -> FitVal
ff3 xs =
 let len     = dLen xs
     f (b,d) = (b && d > 0) || ( (not b) && d < 0 )
     numOk   = dLen . filter f $ xs
  in 100 * ( numOk * numOk ) / ( len * len )

ff4 :: Dist Double -> FitVal
ff4 d =  
 let xs = fst $ runState (distTake_new (100 * distSize d) d) (mkStdGen 42) 
  in 1 / ( 1 + abs (sum xs) ) 

ff4' :: Dist Double -> Rand FitVal
ff4' d = do 
 xs <- (distTake_new (5 * distSize d) d) 
 return $ 1 / ( 1 + abs (sum xs) ) 


dLen :: [a] -> Double
dLen = fromIntegral . length 

