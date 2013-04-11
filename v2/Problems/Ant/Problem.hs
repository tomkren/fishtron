module Problems.Ant.Problem where

import Text.JSON
import JSONUtils
import DrawIM( imGraphInJSON )

import TTerm (Typ(..),Context)
import Problems.Utils (  cttProblem3 )

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



