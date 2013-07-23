module ProblemUtils (
  cttProblem5,
  cttProblem4,
  cttProblem3,
  cttProblem2,
  cttProblem2',
  boolListProblem2,
  asType,
  casesFF,
  ProblemOpts(..),PO_BLP(..),PO_CTTP(..),IntSlider(..),JobID,runProblemOpts,po2json,poCode,json2po,
  runProblemOpts_new
) where

import GP_Core (FitFun(..),Problem(..),NumGene,PopSize,mkGenOps)
import GP_Core (GenOpProbs,FitVal,mkFF1)
import GP_Core ( nRunsByServer, nRunsByServer_new )
import GP_Data (CTTGen(..),CTTCro(..))
import GP_Data (ListGen(..),ListCro(..),ListMut(..), BoolGen(..), BoolMut(..), Len)


import TTree (CTT)
import TTerm (Typ,Context)

import Text.JSON
import JSONUtils

import Data.Typeable ( Typeable )

import ServerInterface ( OutputBuffer ) 

-- ---------------------------------------------------------------



data ProblemOpts a = PO_BLP_ PO_BLP | PO_CTTP_ (PO_CTTP a) 


poCode :: ProblemOpts a -> String
poCode po = case po of
  PO_BLP_  x ->  blp_code x
  PO_CTTP_ x -> cttp_code x

po2json :: ProblemOpts a -> JSValue
po2json po = case po of
  PO_BLP_  x ->  blp2json x
  PO_CTTP_ x -> cttp2json x

json2po :: ProblemOpts a -> JSValue -> ProblemOpts a
json2po po json = case po of
  PO_BLP_  x -> PO_BLP_  $ json2blp  x json
  PO_CTTP_ x -> PO_CTTP_ $ json2cttp x json



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
  ( "code"    , jsStr          $ blp_code    blp                                  ),
  ( "info"    , jsStr          $ blp_info    blp                                  ),
  ( "data"    ,                  blp_data    blp                                  ),
  ( "numRuns" , intSlider2json $ blp_numRuns blp                                  ),
  ( "numGene" , intSlider2json $ blp_numGene blp                                  ),
  ( "popSize" , intSlider2json $ blp_popSize blp                                  ),
  ( "length"  , intSlider2json $ blp_length  blp                                  ),
  ( "sliders" , jsArr $ map jsStr [ "numRuns", "numGene" , "popSize" , "length" ] )]

json2blp :: PO_BLP -> JSValue -> PO_BLP
json2blp blp json = blp {
  blp_numRuns = json2intSlider $ jsProp json "numRuns"  , 
  blp_numGene = json2intSlider $ jsProp json "numGene"  , 
  blp_popSize = json2intSlider $ jsProp json "popSize"  , 
  blp_length  = json2intSlider $ jsProp json "length"      
 }

data PO_CTTP a = PO_CTTP {
  
  cttp_numRuns     :: IntSlider          , 
  cttp_numGene     :: IntSlider          , 
  cttp_popSize     :: IntSlider          , 
                                                
  cttp_code        :: String             ,
  cttp_info        :: String             ,
  cttp_data        :: JSValue            ,
  
  cttp_ff          :: FitFun CTT a       ,
  cttp_typ         :: Typ                ,
  cttp_ctx         :: Context            ,
  cttp_gOpt        :: CTTGen         
 }

cttp2json :: PO_CTTP a -> JSValue
cttp2json cttp = jsObj [
  ( "code"    , jsStr          $ cttp_code    cttp                      ),
  ( "info"    , jsStr          $ cttp_info    cttp                      ),
  ( "data"    ,                  cttp_data    cttp                      ),
  ( "numRuns" , intSlider2json $ cttp_numRuns cttp                      ),
  ( "numGene" , intSlider2json $ cttp_numGene cttp                      ),
  ( "popSize" , intSlider2json $ cttp_popSize cttp                      ),
  ( "sliders" , jsArr $ map jsStr [ "numRuns", "numGene" , "popSize"  ] )]

json2cttp :: PO_CTTP a -> JSValue -> PO_CTTP a
json2cttp cttp json = cttp {
  cttp_numRuns = json2intSlider $ jsProp json "numRuns"  , 
  cttp_numGene = json2intSlider $ jsProp json "numGene"  , 
  cttp_popSize = json2intSlider $ jsProp json "popSize"        
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

runProblemOpts :: (Typeable a) => ProblemOpts a -> JobID -> IO ()
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
  PO_CTTP_ cttp ->
   let numRuns     = ( slider_value . cttp_numRuns $ cttp )
       numGene     = ( slider_value . cttp_numGene $ cttp )
       popSize     = ( slider_value . cttp_popSize $ cttp )

       typ         = cttp_typ  cttp
       ctx         = cttp_ctx  cttp
       fitFun      = cttp_ff   cttp
       gOpt        = cttp_gOpt cttp -- CTTG_Koza2 typ ctx 
       mOpt        = ()
       cOpt        = CTTC_Koza

       genOpProbs  = (10,0,90)
       genOps      = mkGenOps (mOpt,cOpt) genOpProbs
       problemName = cttp_code cttp
    in nRunsByServer jobID numRuns $ Problem problemName popSize numGene genOps gOpt mOpt cOpt fitFun


runProblemOpts_new :: (Typeable a) => ProblemOpts a -> OutputBuffer -> IO ()
runProblemOpts_new pOpts buff = case pOpts of
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
    in nRunsByServer_new buff numRuns $ Problem problemName popSize numGene genOps gOpt mOpt cOpt fitFun
  PO_CTTP_ cttp ->
   let numRuns     = ( slider_value . cttp_numRuns $ cttp )
       numGene     = ( slider_value . cttp_numGene $ cttp )
       popSize     = ( slider_value . cttp_popSize $ cttp )

       typ         = cttp_typ  cttp
       ctx         = cttp_ctx  cttp
       fitFun      = cttp_ff   cttp
       gOpt        = cttp_gOpt cttp -- CTTG_Koza2 typ ctx 
       mOpt        = ()
       cOpt        = CTTC_Koza

       genOpProbs  = (10,0,90)
       genOps      = mkGenOps (mOpt,cOpt) genOpProbs
       problemName = cttp_code cttp
    in nRunsByServer_new buff numRuns $ Problem problemName popSize numGene genOps gOpt mOpt cOpt fitFun


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

-- cttProblem' :: String -> (a -> (FitVal, Bool)) -> a -> Typ -> Context -> NumGene -> PopSize -> CTTProblem a
-- cttProblem' name ff as = cttProblem name (FF3 show as (return . ff)) 
-- 
-- cttProblem :: String -> FitFun CTT a -> Typ -> Context -> NumGene -> PopSize -> CTTProblem a
-- cttProblem problemName ff typ ctx numGene popSize =
--  let genOpProbs = (10,0,90)
--      gOpt       = CTTG_Koza typ ctx 1000 
--      mOpt       = ()
--      cOpt       = CTTC_Koza
--   in Problem problemName popSize numGene (mkGenOps (mOpt,cOpt) genOpProbs) gOpt mOpt cOpt ff

boolListProblem2 :: String -> GenOpProbs -> Len -> ([Bool]->FitVal) -> NumGene -> PopSize -> BoolListProblem
boolListProblem2 problemName genOpProbs len ff numGene popSize = 
 let gOpt   = LG_ BG_ len
     mOpt   = LM_OnePoint BM_Not len 
     cOpt   = LC_OnePoint () len
     genOps = mkGenOps (mOpt,cOpt) genOpProbs
     fitFun = mkFF1 $ return . ff
  in Problem problemName popSize numGene genOps gOpt mOpt cOpt fitFun