module Problems.SSR.Problem where

import Text.JSON
import JSONUtils
import DrawIM( imGraphInJSON )

import TTerm (Typ(..),Context)
--import Problems.Utils ( cttProblem5 , cttProblem3 , cttProblem' , cttProblem2' , cttProblem4 , asType )
import Problems.SSR.Funs (ff)

import ProblemUtils 
import GP_Core (FitFun(FF6))
import GP_Data (CTTGen(..))


reg = PO_CTTP_ PO_CTTP {
  cttp_code        = "ssr"                                        ,
  cttp_info        = "Simple Symbolic Regression."                ,
  cttp_data        = jsData                                       ,
  cttp_numRuns     = IntSlider "Runs"            1 100   1    1   ,
  cttp_numGene     = IntSlider "Generations"     0 200   50   10  ,
  cttp_popSize     = IntSlider "Population size" 0 5000  500  100 ,
  
  cttp_typ         = dou1                                         ,
  cttp_ctx         = ctx                                          ,

  cttp_gOpt        = CTTG_Geom  dou1 ctx 0.75 , --CTTG_Geom  dou1 ctx 0.75  , --CTTG_Koza2 dou1 ctx , 
  
  cttp_ff          = FF6 (asType::Double->Double) ff "Problems.SSR.Funs", 
  
  cttp_saveBest    = False -- True 
  
}


mainProblem = cttProblem5 "SSR" ff dou1 ctx (asType::Double->Double)

jsData = jsObj [ ( "im" , imGraphInJSON dou1 ctx ) ]


dou, dou1, dou2 :: Typ
dou  = Typ "Double"
dou1 = dou :-> dou
dou2 = dou :-> dou :-> dou

ctx :: Context
ctx = [( "(+)"  , dou2 ),
       ( "(-)"  , dou2 ),
       ( "(*)"  , dou2 ),
       ( "rdiv" , dou2 ),
       ( "sin"  , dou1 ),
       ( "cos"  , dou1 ),
       ( "exp"  , dou1 ),
       ( "rlog" , dou1 )]




-- problem1 = cttProblem'  "ssr"  ff (asType::Double->Double) dou1 ctx
-- problem2 = cttProblem2' "ssr2" ff (asType::Double->Double) dou1 ctx
-- problem3 = cttProblem3  "ssr3" "Problems.SSR.Funs" dou1 ctx
-- problem4 = cttProblem4  "ssr4" "Problems.SSR.Funs" ff dou1 ctx (asType::Double->Double)
