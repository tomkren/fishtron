module Problems.Utils (
  cttProblem5,
  cttProblem4,
  cttProblem3,
  cttProblem2,
  cttProblem2',
  cttProblem ,
  cttProblem',
  boolListProblem2,
  asType,
  casesFF,
  ProblemOpts(..),PO_BLP(..),IntSlider(..),JobID,runProblemOpts,po2json,poCode,json2po
) where

import GP_Core (FitFun(..),Problem(..),NumGene,PopSize,mkGenOps)
import GP_Core (GenOpProbs,FitVal,mkFF1)
import GP_Core ( nRunsByServer )
import GP_Data (CTTGen(..),CTTCro(..))
import GP_Data (ListGen(..),ListCro(..),ListMut(..), BoolGen(..), BoolMut(..), Len)


import TTree (CTT)
import TTerm (Typ,Context)

import Text.JSON
import JSONUtils

-- ---------------------------------------------------------------



data ProblemOpts = PO_BLP_ PO_BLP 

poCode :: ProblemOpts -> String
poCode po = case po of
  PO_BLP_ blp -> blp_code blp

po2json :: ProblemOpts -> JSValue
po2json po = case po of
  PO_BLP_ blp -> blp2json blp

json2po :: ProblemOpts -> JSValue -> ProblemOpts
json2po po json = case po of
  PO_BLP_ blp -> PO_BLP_ $ json2blp blp json


data PO_BLP = PO_BLP {
  
  blp_numRuns     :: IntSlider        , 
  blp_numGene     :: IntSlider        , 
  blp_popSize     :: IntSlider        , 
  blp_length      :: IntSlider        ,

  blp_code        :: String           ,
  blp_info        :: String           ,
  blp_data        :: JSValue          ,
  blp_ff          :: [Bool] -> Double }


blp2json :: PO_BLP -> JSValue
blp2json blp = jsObj [
  ( "code"    , jsStr          $ blp_code    blp                      ),
  ( "info"    , jsStr          $ blp_info    blp                      ),
  ( "data"    ,                  blp_data    blp                      ),
  ( "numRuns" , intSlider2json $ blp_numRuns blp                      ),
  ( "numGene" , intSlider2json $ blp_numGene blp                      ),
  ( "popSize" , intSlider2json $ blp_popSize blp                      ),
  ( "length"  , intSlider2json $ blp_length  blp                      ),
  ( "sliders" , jsArr $ map jsStr [ "numRuns", "numGene" , "popSize" , "length" ] )]

json2blp :: PO_BLP -> JSValue -> PO_BLP
json2blp blp json = blp {
  blp_numRuns = json2intSlider $ jsProp json "numRuns"  , 
  blp_numGene = json2intSlider $ jsProp json "numGene"  , 
  blp_popSize = json2intSlider $ jsProp json "popSize"  , 
  blp_length  = json2intSlider $ jsProp json "length"      
 }


data IntSlider = IntSlider { 
  slider_name    :: String ,
  slider_min     :: Int ,
  slider_max     :: Int ,
  slider_value   :: Int ,
  slider_step    :: Int } 
 deriving (Show)


intSlider2json :: IntSlider -> JSValue
intSlider2json is = jsObj [
  ( "name"  , jsStr $ slider_name  is ),
  ( "min"   , jsNum $ slider_min   is ),
  ( "max"   , jsNum $ slider_max   is ),
  ( "value" , jsNum $ slider_value is ),
  ( "step"  , jsNum $ slider_step  is )]

json2intSlider :: JSValue -> IntSlider
json2intSlider json = IntSlider {
  slider_name  = fromJsStr $ jsProp json "name"  ,
  slider_min   = fromJsInt $ jsProp json "min"   ,
  slider_max   = fromJsInt $ jsProp json "max"   ,
  slider_value = fromJsInt $ jsProp json "value" ,
  slider_step  = fromJsInt $ jsProp json "step" 
 }



type JobID = String

runProblemOpts :: ProblemOpts -> JobID -> IO ()
runProblemOpts pOpts jobID = case pOpts of
  PO_BLP_ blp ->
   let numRuns     = ( slider_value . blp_numRuns $ blp )
       numGene     = ( slider_value . blp_numGene $ blp )
       popSize     = ( slider_value . blp_popSize $ blp )
       len         = ( slider_value . blp_length  $ blp )
       gOpt        = LG_ BG_ len
       mOpt        = LM_OnePoint BM_Not len 
       cOpt        = LC_OnePoint () len
       genOps      = mkGenOps (mOpt,cOpt) (33,33,33)
       fitFun      = mkFF1 $ return . (blp_ff blp)
       problemName = blp_code blp
    in nRunsByServer jobID numRuns $ Problem problemName popSize numGene genOps gOpt mOpt cOpt fitFun



-- ---------------------------------------------------------------

casesFF :: [Bool] -> (Double,Bool)
casesFF conds = 
  let (d,b) = foldr (\cond (d,b)->if cond then (d+1,b) else (d,False) ) (0,True) conds 
   in ( d / (fromIntegral $ length conds) , b )

asType :: a
asType = undefined

type CTTProblem a = Problem CTT a CTTGen () CTTCro
type BoolListProblem = Problem [Bool] () (ListGen BoolGen) (ListMut BoolMut) (ListCro () )

cttProblem5 :: String -> (a -> (FitVal,Bool)) -> Typ -> Context -> a -> NumGene -> PopSize -> CTTProblem a
cttProblem5 name ff typ ctx as = cttProblem2 name (FF6 as ff modul) typ ctx
 where modul = "Problems." ++ name ++ ".Funs"

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