module Problems.Ant.Problem where

import Text.JSON
import JSONUtils
import DrawIM( imGraphInJSON )

import TTerm (Typ(..),Context)
import ProblemUtils 
import GP_Core (FitFun(FF5))
import GP_Data (CTTGen(..))


reg = PO_CTTP_ PO_CTTP {
  cttp_code        = "ant"                                        ,
  cttp_info        = "Artifical Ant"                              ,
  cttp_data        = jsData                                       ,
  cttp_numRuns     = IntSlider "Runs"            1 100   50   1   ,
  cttp_numGene     = IntSlider "Generations"     0 100   50   10  ,
  cttp_popSize     = IntSlider "Population size" 0 5000  500  100 ,
  
  cttp_typ         = ant                                          ,
  cttp_ctx         = ctx                                          ,
  
  cttp_gOpt        = CTTG_Geom     ant ctx 0.75 ,  
--  cttp_gOpt        = CTTG_Koza2 ant ctx ,    

  cttp_ff          = FF5 "ff" "Problems.Ant.Funs" (), 
  
  cttp_saveBest    = True --False
  
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


-- function (){return ifa(p3(m,m,l),p2(ifa(p3(m,r,l),p2(r,m)),ifa(ifa(l,ifa(l,l)),p2(p2(r,ifa(ifa(l,r),ifa(r,r))),ifa(p3(m,l,l),ifa(l,r))))));}


-- function (){return ifa(p2(ifa(l,r),ifa(ifa(l,m),ifa(r,l))),p2(ifa(p3(m,l,l),p2(r,r)),ifa(p3(p3(m,m,m),p2(l,l),r),p3(l,m,l))));}
-- =
-- ┌────────────────────────┐
-- │ Run              26/50 │
-- │ Genration           23 │
-- ├────────────────────────┤
-- │ Best          89.00000 │
-- │ Average       60.91600 │
-- │ Worst          0.00000 │
-- └────────────────────────┘
--WINNER!!!!!!!
-- ┌────────────────────────────────────────────────────────────────────────────┐
-- │ ifa (p2 (ifa l r) (ifa (ifa l m) (ifa r l))) (p2 (ifa (p3 m l l) (p2 r r)) │
-- │  (ifa (p3 (p3 m m m) (p2 l l) r) (p3 l m l)))                              │
-- └────────────────────────────────────────────────────────────────────────────┘







