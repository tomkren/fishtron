module GP_Test where

import Data.List

import GP_Core
import GP_Data

import Utils ( asType,putList )
import KozaTree
import TTerm
-- 
-- import InhabitationMachines (mkIM,testIM,proveN)
import InhabitationMachines ( proveN , randProveN, randProveUnique, proveOneWithLimit, Limit, kozaProveN )
import Eva
-- 
import TTree

import Ant


-- for Server to call ... -----------------------------------------------



problem_ssr  = cttProblem  "ssr"  ff_cttSSR_2 dou1 ctx_ttSSR
problem_ssr2 = cttProblem2 "ssr2" ff_cttSSR_2 dou1 ctx_ttSSR
problem_ant  = cttProblem  "ant"  ff_ant2 ant ctx_ant
problem_ba   = boolListProblem2 "ba" (33,33,33) 100 ff_boolAlternate
problem_head = cttProblem2 "head" ff_head_ (l_int :-> m_int) ctx_head


-- Test Runs ------------------------------------------------------------

run_boolAlternate = run pro_boolAlternate
run_ttSSR         = run pro_ttSSR
run_ttSSR_mini    = run pro_ttSSR_mini

run_cttSSR        = run (pro_cttSSR 50 500)
run_cttSSR_2      = run (pro_cttSSR_2 50 500)

run_ant           = run (pro_ant 50 500)
 
run_kozaSSR       = run (pro_kozaSSR   50)
run_kozaSSR_2     = run (pro_kozaSSR_2 50)
runs_kozaSSR      = nRuns 20 (pro_kozaSSR 25)

runs_kozaSSR_2    = nRuns 113 (pro_kozaSSR_2 25)
runs_cttSSR_2     = nRuns 2  (pro_cttSSR_2 3 500)


run_M11_ctt = run (pro_M11_ctt 50 500)
run_M6_ctt = run (pro_M6_ctt 50 500)

runs_M6_ctt = nRuns 20 (pro_M6_ctt 100 500)

run_EP3 = run (pro_EP3 50 500)
run_EP = run (pro_EP 50 500)


-- Problems & Fitnes Functions -------------------------------------------



pro_boolAlternate = boolListProblem "boolAlt" 50 50 (33,33,33) 100 ff_boolAlternate

ff_boolAlternate :: [Bool] -> FitVal
ff_boolAlternate bits = 
 let tNum = dLen . filter (\(n,b)->if n `mod` 2 == 0 then b else not b) $ zip [0..] bits 
     len  = dLen $ bits
  in 100*(tNum * tNum)/(len*len)

pro_kozaSSR    = kozaProblem "kozaSSR"     ff_kozaSSR       env_kozaSSR
pro_kozaSSR_2  = kozaProblem "kozaSSRhits" ff_kozaSSR_2     env_kozaSSR

pro_cttSSR     = cttProblem  "cttSSR"      ff_cttSSR   dou1 ctx_ttSSR
pro_cttSSR_2   = cttProblem  "cttSSRhits"  ff_cttSSR_2 dou1 ctx_ttSSR

pro_ant        = cttProblem "ant"          ff_ant      ant  ctx_ant

pro_ttSSR      = ttProblem   "ttSSR"       ff_ttSSR    dou1 ctx_ttSSR
pro_ttSSR_mini = ttProblem   "ttSSRmini"   ff_ttSSR    dou1 ctx_ttSSR_mini
 
env_kozaSSR :: KEnv
env_kozaSSR = (["x"],[("plus",2),("minus",2),("krat",2),("rdiv",2),("sin",1),("cos",1),("exp",1),("rlog",1)])

dou, dou1, dou2 :: Typ
dou  = Typ "Double"
dou1 = dou :-> dou
dou2 = dou :-> dou :-> dou

ant, ant2, ant3 :: Typ
ant  = Typ "AAnt"
--ant  = Typ "Ant"
ant2 = ant :-> ant :-> ant
ant3 = ant :-> ant :-> ant :-> ant


int :: Typ
int   = Typ "Int"
m_int = Typ "Maybe Int"
l_int = Typ "[Int]"


ctx_head :: Context
ctx_head = [  ( "listCase" , l_int :-> m_int :-> (int:->l_int:->m_int) :-> m_int ),
              ( "Nothing"  , m_int ),
              ( "Just"     , int :-> m_int ) ]

ff_head_ :: FitFun CTT ()
ff_head_ = FF4 show "ff_head" () 

ctx_ttSSR :: Context
ctx_ttSSR = [("(+)",dou2),("(-)",dou2),("(*)",dou2),("rdiv",dou2),("sin",dou1),("cos",dou1),("exp",dou1),("rlog",dou1)]
 
ctx_ant :: Context
ctx_ant = [ ("l",ant), ("r",ant), ("m",ant), ("ifa",ant2), ("p2",ant2), ("p3",ant3) ]

ctx_ttSSR_mini :: Context
ctx_ttSSR_mini = ([("plus",dou2),("krat",dou2)])
 
 
ff_kozaSSR :: FitFun KTree (Double->Double)
ff_kozaSSR = FF2 (\ t -> "(\\ x -> " ++ show t ++ ")" ) (asType::Double->Double) (return . rawFF_SSR)

ff_kozaSSR_2 :: FitFun KTree (Double->Double)
ff_kozaSSR_2 = FF3 (\ t -> "(\\ x -> " ++ show t ++ ")" ) (asType::Double->Double) (return . rawFF_SSR_2)
  
ff_ttSSR :: FitFun TTerm (Double->Double)
ff_ttSSR = FF2 show (asType::Double->Double) (return . rawFF_SSR)

ff_cttSSR :: FitFun CTT (Double->Double)
ff_cttSSR = FF2 show (asType::Double->Double) (return . rawFF_SSR)

ff_cttSSR_2 :: FitFun CTT (Double->Double)
ff_cttSSR_2 = FF3 show (asType::Double->Double) (return . rawFF_SSR_2)

ff_ant2 :: FitFun CTT ()
ff_ant2 = FF4 show "ffAnt" ()

ff_ant :: FitFun CTT AAnt
ff_ant = FF3 show (asType::AAnt) (return . rawFF_ant)

rawFF_ant :: AAnt -> (FitVal,Bool)
rawFF_ant ant =
 let eaten = evalAnt antWorld (unAAnt ant) --ant
  in ( fromIntegral eaten , eaten == 89 )
  


rawFF_SSR :: (Double->Double) -> FitVal
rawFF_SSR f = (1/) . (1+) . sum . diffs_kozaSSR $ f  

rawFF_SSR_2 :: (Double->Double) -> (FitVal,Bool)
rawFF_SSR_2 f = 
 let diffs = diffs_kozaSSR f
  in( (1/) . (1+) . sum $ diffs , 20 == (length . filter (\dif-> (abs dif) < 0.01 ) $ diffs)  )  


-- 
diffs_kozaSSR :: (Double->Double) -> [Double]
diffs_kozaSSR f = map (\x-> let dx = (f x) - ( x*x*x*x+x*x*x+x*x+x ) in abs dx ) (linInterval (-1,1) 20)

linInterval :: (Double,Double) -> Int -> [Double]
linInterval (a,b) n = 
 let step = (b-a) / (fromIntegral $ n-1)
  in [ a + (fromIntegral i)*step | i <- [0..(n-1)] ]

 
showRes_SSR :: (Double->Double) -> String
showRes_SSR f = "Sum of diffs : " ++ ( show . sum . diffs_kozaSSR $ f )


-- even-parity ----

pro_EP3 = cttProblem "EP3" ff_EP3 b3 ctx_EPn

pro_EP = cttProblem "EP" ff_EP (bs:->b) ctx_EP 

ctx_EP :: Context
ctx_EP = [("foldr",(b:->b:->b):->b:->bs:->b),("not",b1),("xor",b2),("head'",b:->bs:->b),("True",b),("tail'",bs:->bs) ]

ctx_EPn :: Context
ctx_EPn = [("(&&)",bool2),("(||)",bool2),("not",bool1),("if'",bool3)]

evenParity :: [Bool] -> Bool
evenParity xs = even (length $ filter id xs)

ff_EP :: FitFun CTT ([Bool]->Bool)
ff_EP = FF3 show (asType::([Bool]->Bool)) ( return . rawFF_EP )

ff_EP3 :: FitFun CTT B3
ff_EP3 = FF3 show (asType::B3) (return . (rawFF_EPn 3 from3) )

rawFF_EP :: ([Bool]->Bool) -> (FitVal,Bool)
rawFF_EP f =
 let allBs  = (allBoolInputs 2) ++ (allBoolInputs 3) ++ (allBoolInputs 4) 
     okOuts = map evenParity allBs
     myOuts = map f allBs
     hits   = length . filter (\(ok,my)->ok == my) $ zip okOuts myOuts
  in ( fromIntegral hits , hits == (2^2 + 2^3 + 2^4) )

rawFF_EPn :: Int -> (a->[Bool]->Bool) -> a -> (FitVal,Bool)
rawFF_EPn n convertF f =
 let allBs  = allBoolInputs n 
     okOuts = map evenParity allBs
     myOuts = map (convertF f) allBs
     hits   = length . filter (\(ok,my)->ok == my) $ zip okOuts myOuts
  in ( fromIntegral hits , hits == 2^n )

from2 :: B2 -> [Bool] -> Bool
from2 f [x,y] = f x y

from3 :: B3 -> [Bool] -> Bool
from3 f [x,y,z] = f x y z


-- multiplexer ----

pro_M11_ctt = cttProblem "M11_ctt" ff_M11_ctt m11 ctx_M
pro_M6_ctt  = cttProblem "M6_ctt"  ff_M6_ctt  m6  ctx_M

type B = Bool
type B2 = B->B->B
type B3 = B->B->B->B
type M11 = B->B->B->B->B->B->B->B->B->B->B->B
type M6  = B->B->B->B->B->B->B

bs :: Typ 
bs = Typ "[Bool]"

b, bool1, bool2 , m11 , m6 :: Typ
b  = Typ "Bool"
bool1 = b :-> b
bool2 = b :-> b :-> b
bool3 = b :-> b :-> b :-> b
b3 = bool3
b2 = bool2
b1 = bool1
m11 = b:->b:->b:->b:->b:->b:->b:->b:->b:->b:->b:->b 
m6  = b:->b:->b:->b:->b:->b:->b 


ctx_M :: Context
ctx_M = ([("(&&)",bool2),("(||)",bool2),("not",bool1),("if'",bool3)])
 

ff_M11_ctt :: FitFun CTT M11
ff_M11_ctt = FF3 show (asType::M11) (return . (rawFF_M 11 3 from11 ) )

ff_M6_ctt :: FitFun CTT M6
ff_M6_ctt = FF3 show (asType::M6) (return . (rawFF_M 6 2 from6 ) )

rawFF_M :: Int -> Int -> (m->[Bool]->Bool) -> m -> (FitVal,Bool)
rawFF_M n k fromMu f = 
 let g = fromMu f
     allBs = allBoolInputs n
     okOuts = map (multiplexer k) allBs
     myOuts = map g allBs
     hits   = length . filter (\(ok,my)->ok == my) $ zip okOuts myOuts
  in ( fromIntegral hits , hits == 2^n )

from11 :: M11 -> [Bool] -> Bool
from11 f [a0,a1,a2,d0,d1,d2,d3,d4,d5,d6,d7] 
      = f a0 a1 a2 d0 d1 d2 d3 d4 d5 d6 d7

from6 :: M6 -> [Bool] -> Bool
from6 f [a0,a1,d0,d1,d2,d3] 
      = f a0 a1 d0 d1 d2 d3 

to11 :: ([Bool] -> Bool) -> M11 
to11 f a0 a1 a2 d0 d1 d2 d3 d4 d5 d6 d7 
  = f [a0,a1,a2,d0,d1,d2,d3,d4,d5,d6,d7]

multiplexer :: Int -> [Bool] -> Bool
multiplexer k xs =
 let ( pos , ys ) = splitAt k xs
  in ys !! ( bools2int pos )

bools2int :: [Bool] -> Int
bools2int [] = 0
bools2int (x:xs) = (if x then 1 else 0) + 2*(bools2int xs)
 
allBoolInputs :: Int -> [[Bool]]
allBoolInputs 0 = [ [] ] 
allBoolInputs n = (map (False:) rest) ++ (map (True:) rest)
 where rest = allBoolInputs (n-1) 

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

-- testSKI = putList . map (\f-> g f == g (toSki' f) ) $ proveN 100 dou1 ctx_ttSSR_mini
--  where g f = eval (show f) (asType::Double->Double) 42
-- testSKI2 n = skiComare dou1 ctx_ttSSR_mini n
-- testSKI3 n = skiComare ( (((dou:->dou):->dou):->dou) :-> dou :-> dou ) [] n
-- 
-- skiComare typ ctx n = putStrList . concatMap (\tt-> [ [], show tt , show (toSki' tt) , show (mkCTT tt) ] ) $ proveN n typ ctx

-----------



-- utils ---------------------------------------------------------------

type CTTProblem  a   = Problem CTT    a  CTTGen            ()                CTTCro
type TTProblem   a   = Problem TTerm  a  TTermGen          ()                TTermCro
type KozaProblem a   = Problem KTree  a  KTreeGen          KTreeMut          KTreeCro
type BoolListProblem = Problem [Bool] () (ListGen BoolGen) (ListMut BoolMut) (ListCro () )

boolListProblem2 :: String -> GenOpProbs -> Len -> ([Bool]->FitVal) -> NumGene -> PopSize -> BoolListProblem
boolListProblem2 problemName genOpProbs len ff numGene popSize = 
 let gOpt   = LG_ BG_ len
     mOpt   = LM_OnePoint BM_Not len 
     cOpt   = LC_OnePoint () len
     genOps = mkGenOps (mOpt,cOpt) genOpProbs
     fitFun = mkFF1 $ return . ff
  in Problem problemName popSize numGene genOps gOpt mOpt cOpt fitFun

cttProblem2 :: String -> FitFun CTT a -> Typ -> Context -> NumGene -> PopSize -> CTTProblem a
cttProblem2 problemName ff typ ctx numGene popSize =
 let genOpProbs = (10,0,90)
     gOpt       = CTTG_Koza2 typ ctx 
     mOpt       = ()
     cOpt       = CTTC_Koza
  in Problem problemName popSize numGene (mkGenOps (mOpt,cOpt) genOpProbs) gOpt mOpt cOpt ff


cttProblem :: String -> FitFun CTT a -> Typ -> Context -> NumGene -> PopSize -> CTTProblem a
cttProblem problemName ff typ ctx numGene popSize =
 let genOpProbs = (10,0,90)
     gOpt       = CTTG_Koza typ ctx 1000 
     mOpt       = ()
     cOpt       = CTTC_Koza
  in Problem problemName popSize numGene (mkGenOps (mOpt,cOpt) genOpProbs) gOpt mOpt cOpt ff

ttProblem :: String -> FitFun TTerm a -> Typ -> Context -> TTProblem a
ttProblem problemName ff typ ctx =
 let popSize    = 500
     numGene    = 50   
     genOpProbs = (10,0,90)
     gOpt       = TTG_SKI typ ctx 100 -- TTG_IM_rand typ ctx 100
     mOpt       = ()
     cOpt       = TTC_SKI -- TTC_my ctx
  in Problem problemName popSize numGene (mkGenOps (mOpt,cOpt) genOpProbs) gOpt mOpt cOpt ff

kozaProblem :: String -> FitFun KTree a -> KEnv -> NumGene -> KozaProblem a
kozaProblem problemName ff env numGene = 
 let popSize    = 500
     genOpProbs = (10,0,90)
     gOpt       = KG_Koza env
     mOpt       = KM_Koza env
     cOpt       = KC_Koza 
  in Problem problemName popSize numGene (mkGenOps (mOpt,cOpt) genOpProbs) gOpt mOpt cOpt ff

boolListProblem :: String -> PopSize -> NumGene -> GenOpProbs -> Len -> ([Bool]->FitVal) -> BoolListProblem
boolListProblem problemName popSize numGene genOpProbs len ff = 
 let gOpt   = LG_ BG_ len
     mOpt   = LM_OnePoint BM_Not len 
     cOpt   = LC_OnePoint () len
     genOps = mkGenOps (mOpt,cOpt) genOpProbs
     fitFun = mkFF1 $ return . ff
  in Problem problemName popSize numGene genOps gOpt mOpt cOpt fitFun



dLen :: [a] -> Double
dLen = fromIntegral . length 
