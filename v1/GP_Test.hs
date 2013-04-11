module GP_Test where

import GP_Core
import GP_Data

import Util
import KozaTree
import TTerm

import InhabitationMachines (mkIM,testIM,proveN)

import TTree

import Data.Typeable
import Heval

-- Test Runs ------------------------------------------------------------

run_boolAlternate = run pro_boolAlternate
run_ttSSR         = run pro_ttSSR
run_ttSSR_mini    = run pro_ttSSR_mini
run_cttSSR        = run pro_cttSSR

run_kozaSSR       = run (pro_kozaSSR   50)
run_kozaSSR_2     = run (pro_kozaSSR_2 50)
runs_kozaSSR      = nRuns 3 (pro_kozaSSR 5)


-- Problems & Fitnes Functions -------------------------------------------


pro_boolAlternate = boolListProblem 50 50 (33,33,33) 100 ff_boolAlternate

ff_boolAlternate :: [Bool] -> FitVal
ff_boolAlternate bits = 
 let tNum = dLen . filter (\(n,b)->if n `mod` 2 == 0 then b else not b) $ zip [0..] bits 
     len  = dLen $ bits
  in 100*(tNum * tNum)/(len*len)

-- TODO
--ff_hromadky :: Int -> [Double] -> [Int] -> FitVal
--ff_hromadky numHromadek ws hs = 

pro_cttSSR     = cttProblem  ff_cttSSR  dou1 ctx_ttSSR
pro_kozaSSR    = kozaProblem ff_kozaSSR      env_kozaSSR
pro_kozaSSR_2  = kozaProblem ff_kozaSSR_2    env_kozaSSR
pro_ttSSR      = ttProblem   ff_ttSSR   dou1 ctx_ttSSR
pro_ttSSR_mini = ttProblem   ff_ttSSR   dou1 ctx_ttSSR_mini

env_kozaSSR :: KEnv
env_kozaSSR = (["x"],[("plus",2),("minus",2),("krat",2),("rdiv",2),("sin",1),("cos",1),("exp",1),("rlog",1)])

dou, dou1, dou2 :: Typ
dou  = Typ "Double"
dou1 = dou :-> dou
dou2 = dou :-> dou :-> dou

ctx_ttSSR :: Context
ctx_ttSSR = ([("plus",dou2),("minus",dou2),("krat",dou2),("rdiv",dou2),("sin",dou1),("cos",dou1),("exp",dou1),("rlog",dou1)])

ctx_ttSSR_mini :: Context
ctx_ttSSR_mini = ([("plus",dou2),("krat",dou2)])


ff_kozaSSR :: FitFun KTree (Double->Double)
ff_kozaSSR = FF2 (\ t -> "(\\ x -> " ++ show t ++ ")" ) (asType::Double->Double) (return . rawFF_SSR)

ff_kozaSSR_2 :: FitFun KTree (Double->Double)
ff_kozaSSR_2 = FF3 (\ t -> "(\\ x -> " ++ show t ++ ")" ) (asType::Double->Double) ff 
 where ff f = let fv       = rawFF_SSR f
                  isWinner = fv > 0.5
               in return ( fv , isWinner )
   
ff_ttSSR :: FitFun TTerm (Double->Double)
ff_ttSSR = FF2 show (asType::Double->Double) (return . rawFF_SSR)

ff_cttSSR :: FitFun CTT (Double->Double)
ff_cttSSR = FF2 show (asType::Double->Double) (return . rawFF_SSR)

rawFF_SSR :: (Double->Double) -> FitVal
rawFF_SSR f = (1/) . (1+) . sum . diffs_kozaSSR $ f  

diffs_kozaSSR :: (Double->Double) -> [Double]
diffs_kozaSSR f = map (\x-> let dx = (f x) - ( x*x*x*x+x*x*x+x*x+x ) in abs dx ) [-1,-0.9..1]


showRes_SSR :: (Double->Double) -> String
showRes_SSR f = "Sum of diffs : " ++ ( show . sum . diffs_kozaSSR $ f )


-- Testing phases ------------------------------------------------------

gene_boolAlternate n len = runTest $ testGene n (LG_ BG_ len) (mkFF1 $ return . ff_boolAlternate) undefined

gOpt_kozaSSR = KG_Koza env_kozaSSR
gOpt_ttSSR   = TTG_IM_rand dou1 ctx_ttSSR 100
gOpt_cttSSR  = CTTG_Koza   dou1 ctx_ttSSR 1000 

cOpt_kozaSSR = KC_Koza
cOpt_ttSSR   = TTC_my ctx_ttSSR

gene_kozaSSR n = runTest $ testGene n gOpt_kozaSSR ff_kozaSSR showRes_SSR 
gene_ttSSR   n = runTest $ testGene n gOpt_ttSSR   ff_ttSSR   showRes_SSR 
gene_cttSSR  n = runTest $ testGene n gOpt_cttSSR  ff_cttSSR  showRes_SSR -- bug na 1164190213 480375029

cros_kozaSSR i n = runTest $ testCros i n gOpt_kozaSSR cOpt_kozaSSR ff_kozaSSR showRes_SSR
cros_ttSSR   i n = runTest $ testCros i n gOpt_ttSSR   cOpt_ttSSR   ff_ttSSR   showRes_SSR

cros_ttSSRwith seedStr i n = runTestWith seedStr $ testCros i n gOpt_ttSSR   cOpt_ttSSR   ff_ttSSR   showRes_SSR

cros_fail1 = cros_ttSSRwith "2097148790 558345920" 2 10
cros_fail2 = cros_ttSSRwith "611099341 1516597540" 20 2


---------

testSKI = putList . map (\f-> g f == g (toSki' f) ) $ proveN 100 dou1 ctx_ttSSR_mini
 where g f = eval (show f) (asType::Double->Double) 42
testSKI2 n = skiComare dou1 ctx_ttSSR_mini n
testSKI3 n = skiComare ( (((dou:->dou):->dou):->dou) :-> dou :-> dou ) [] n

skiComare typ ctx n = putStrList . concatMap (\tt-> [ [], show tt , show (toSki' tt) , show (mkCTT tt) ] ) $ proveN n typ ctx

-----------



-- utils ---------------------------------------------------------------

type CTTProblem  a   = Problem CTT    a  CTTGen            ()                CTTCro
type TTProblem   a   = Problem TTerm  a  TTermGen          ()                TTermCro
type KozaProblem a   = Problem KTree  a  KTreeGen          KTreeMut          KTreeCro
type BoolListProblem = Problem [Bool] () (ListGen BoolGen) (ListMut BoolMut) (ListCro () )

cttProblem :: FitFun CTT a -> Typ -> Context -> CTTProblem a
cttProblem ff typ ctx =
 let popSize    = 500
     numGene    = 50   
     genOpProbs = (10,0,90)
     gOpt       = CTTG_Koza typ ctx 1000 
     mOpt       = ()
     cOpt       = CTTC_Koza
  in Problem popSize numGene (mkGenOps (mOpt,cOpt) genOpProbs) gOpt mOpt cOpt ff

ttProblem :: FitFun TTerm a -> Typ -> Context -> TTProblem a
ttProblem ff typ ctx =
 let popSize    = 500
     numGene    = 50   
     genOpProbs = (10,0,90)
     gOpt       = TTG_SKI typ ctx 100 -- TTG_IM_rand typ ctx 100
     mOpt       = ()
     cOpt       = TTC_SKI -- TTC_my ctx
  in Problem popSize numGene (mkGenOps (mOpt,cOpt) genOpProbs) gOpt mOpt cOpt ff

kozaProblem :: FitFun KTree a -> KEnv -> NumGene -> KozaProblem a
kozaProblem ff env numGene = 
 let popSize    = 500
     genOpProbs = (10,0,90)
     gOpt       = KG_Koza env
     mOpt       = KM_Koza env
     cOpt       = KC_Koza 
  in Problem popSize numGene (mkGenOps (mOpt,cOpt) genOpProbs) gOpt mOpt cOpt ff

boolListProblem :: PopSize -> NumGene -> GenOpProbs -> Len -> ([Bool]->FitVal) -> BoolListProblem
boolListProblem popSize numGene genOpProbs len ff = 
 let gOpt   = LG_ BG_ len
     mOpt   = LM_OnePoint BM_Not len 
     cOpt   = LC_OnePoint () len
     genOps = mkGenOps (mOpt,cOpt) genOpProbs
     fitFun = mkFF1 $ return . ff
  in Problem popSize numGene genOps gOpt mOpt cOpt fitFun



dLen :: [a] -> Double
dLen = fromIntegral . length 
