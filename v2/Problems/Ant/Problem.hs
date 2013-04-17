module Problems.Ant.Problem where

import Text.JSON
import JSONUtils
import DrawIM( imGraphInJSON )

import TTerm (Typ(..),Context)
import Problems.Utils 
import GP_Core (FitFun(FF5))

reg = PO_CTTP_ PO_CTTP {
  cttp_code        = "ant"                                        ,
  cttp_info        = "Artifical Ant"                              ,
  cttp_data        = jsData                                       ,
  cttp_numRuns     = IntSlider "Runs"            1 10    1    1   ,
  cttp_numGene     = IntSlider "Generations"     0 100   10   10  ,
  cttp_popSize     = IntSlider "Population size" 0 5000  500  100 ,
  
  cttp_typ         = ant                                         ,
  cttp_ctx         = ctx                                          ,
  
  cttp_ff          = FF5 "ff" "Problems.Ant.Funs" ()
  
}

mainProblem = cttProblem3 "ant" "Problems.Ant.Funs" ant ctx

jsData = jsObj [ ( "im" , imGraphInJSON ant ctx ) ]

ctx :: Context
ctx = [ ( "l"   , ant  ) , 
        ( "r"   , ant  ) , 
        ( "m"   , ant  ) , 
        ( "ifa" , ant2 ) , 
        ( "p2"  , ant2 ) , 
        ( "p3"  , ant3 ) ]


ant, ant2, ant3 :: Typ
ant  = Typ "Ant"
ant2 = ant :-> ant :-> ant
ant3 = ant :-> ant :-> ant :-> ant



