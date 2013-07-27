module Problems.Ant2.Problem_ where


import JSONUtils (jsObj)
import DrawIM( imGraphInJSON )

import TTerm (Typ(..),Context)
import ProblemUtils 
import GP_Core (FitFun(FF5))
import GP_Data (CTTGen(..))


import Problems.Ant2.Funs


reg = PO_CTTP_ PO_CTTP {
  cttp_code        = "ant2"                                        ,
  cttp_info        = "Artifical Ant"                              ,
  cttp_data        = jsObj [ ( "im" , imGraphInJSON ant ctx ) ]   ,
  cttp_numRuns     = IntSlider "Runs"            1 10    50   1   ,
  cttp_numGene     = IntSlider "Generations"     0 100   50   10  ,
  cttp_popSize     = IntSlider "Population size" 0 5000  500  100 ,
  
  cttp_typ         = ant                                          ,
  cttp_ctx         = ctx                                          ,
  
  cttp_gOpt        = CTTG_Koza2 ant ctx                           , 

  cttp_ff          = FF5 "ff" "Problems.Ant2.Funs" (), 
  
  cttp_saveBest    = False
  
}


ctx :: Context
ctx = [ ( "L"   , ant  ) , 
        ( "R"   , ant  ) , 
        ( "M"   , ant  ) , 
        ( "IFA" , ant2 ) , 
        ( "P2"  , ant2 ) , 
        ( "P3"  , ant3 ) ]

ant, ant2, ant3 :: Typ
ant  = Typ "Ant"
ant2 = ant :-> ant :-> ant
ant3 = ant :-> ant :-> ant :-> ant



winner = IFA M (P3 L (P2 (IFA M R) (P2 R (P2 L R) ) ) (P2 (IFA M L) M) )

winner2 = IFA M (P3 L (P2 (IFA M R ) R ) (P2 (IFA M L) M) )






instance Show World where show = showWorld

showWorld :: World -> String
showWorld (World (iMax,jMax) limit (antPos,dir) (eaten,steps) path set) 
  = (concatMap (\pos@(_,j) -> (if j==1 then "\n" else [] )++
                             (if pos == antPos then (showDir dir)++" " 
                              else if elem pos set  then "F " 
                              else if elem pos path then "@ " else ". ")  ) 
               [(i,j)|i<-[1..iMax],j<-[1..jMax]])++"\n"++
    "\nfood eaten        : " ++ show eaten ++ 
    "\nactions performed : " ++ show steps ++ 
    "\nlimit             : " ++ show limit ++ "\n"
  where showDir d = case d of DUp -> "A" ; DDown -> "V" ; DLeft -> "<" ; DRight -> ">"
