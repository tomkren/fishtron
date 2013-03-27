module Problems.Utils (
  cttProblem4,
  cttProblem3,
  cttProblem2,
  cttProblem2',
  cttProblem ,
  cttProblem',
  boolListProblem2,
  asType
) where

import GP_Core (FitFun(..),Problem(..),NumGene,PopSize,mkGenOps)
import GP_Core (GenOpProbs,FitVal,mkFF1)
import GP_Data (CTTGen(..),CTTCro(..))
import GP_Data (ListGen(..),ListCro(..),ListMut(..), BoolGen(..), BoolMut(..), Len)

import TTree (CTT)
import TTerm (Typ,Context)


asType :: a
asType = undefined

type CTTProblem a = Problem CTT a CTTGen () CTTCro
type BoolListProblem = Problem [Bool] () (ListGen BoolGen) (ListMut BoolMut) (ListCro () )

cttProblem4 :: String -> String -> (a -> (FitVal,Bool)) -> Typ -> Context -> a -> NumGene -> PopSize -> CTTProblem a
cttProblem4 name modul ff typ ctx as = cttProblem2 name (FF6 as ff modul) typ ctx

cttProblem3 :: String -> String -> Typ -> Context -> NumGene -> PopSize -> CTTProblem ()
cttProblem3 name modul typ ctx = cttProblem2 name (FF5 "ff" modul ()) typ ctx

cttProblem2' :: String -> (a -> (FitVal, Bool)) -> a -> Typ -> Context -> NumGene -> PopSize -> CTTProblem a
cttProblem2' name ff as = cttProblem2 name (FF3 show as (return . ff)) 

cttProblem2 :: String -> FitFun CTT a -> Typ -> Context -> NumGene -> PopSize -> CTTProblem a
cttProblem2 problemName ff typ ctx numGene popSize =
 let genOpProbs = (10,0,90)
     gOpt       = CTTG_Koza2 typ ctx 
     mOpt       = ()
     cOpt       = CTTC_Koza
  in Problem problemName popSize numGene (mkGenOps (mOpt,cOpt) genOpProbs) gOpt mOpt cOpt ff

cttProblem' :: String -> (a -> (FitVal, Bool)) -> a -> Typ -> Context -> NumGene -> PopSize -> CTTProblem a
cttProblem' name ff as = cttProblem name (FF3 show as (return . ff)) 

cttProblem :: String -> FitFun CTT a -> Typ -> Context -> NumGene -> PopSize -> CTTProblem a
cttProblem problemName ff typ ctx numGene popSize =
 let genOpProbs = (10,0,90)
     gOpt       = CTTG_Koza typ ctx 1000 
     mOpt       = ()
     cOpt       = CTTC_Koza
  in Problem problemName popSize numGene (mkGenOps (mOpt,cOpt) genOpProbs) gOpt mOpt cOpt ff

boolListProblem2 :: String -> GenOpProbs -> Len -> ([Bool]->FitVal) -> NumGene -> PopSize -> BoolListProblem
boolListProblem2 problemName genOpProbs len ff numGene popSize = 
 let gOpt   = LG_ BG_ len
     mOpt   = LM_OnePoint BM_Not len 
     cOpt   = LC_OnePoint () len
     genOps = mkGenOps (mOpt,cOpt) genOpProbs
     fitFun = mkFF1 $ return . ff
  in Problem problemName popSize numGene genOps gOpt mOpt cOpt fitFun