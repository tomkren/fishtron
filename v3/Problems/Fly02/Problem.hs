module Problems.Fly02.Problem where

import Data.List

import Problems.Fly02.Funs  --(  )
import Problems.Fly02.Fly2  --(  )

import TTerm  (Typ(..),Context)
import GP_Core (FitFun(FF6))
import GP_Data (CTTGen(..))

import DrawIM( imGraphInJSON )

import ProblemUtils 

import JSONUtils
import Text.JSON


--import Heval

reg = PO_CTTP_ PO_CTTP {
  cttp_code        = "fly02"                                      ,
  cttp_info        = "Fly 0.2 eating apples and stuff... "        ,
  cttp_data        = jsData                                       ,
  cttp_numRuns     = IntSlider "Runs"            1 10    1    1   ,
  cttp_numGene     = IntSlider "Generations"     0 100   10   10  ,
  cttp_popSize     = IntSlider "Population size" 0 5000  500  100 ,
  
  cttp_typ         = prog_typ                                     ,
  cttp_ctx         = ctx                                          ,

  cttp_gOpt        = CTTG_Geom     prog_typ ctx 0.5 ,  --CTTG_Koza2 prog_typ ctx    , 
  
  cttp_ff          = FF6 prog_type ff "Problems.Fly02.Funs" 
  
}

prog_type  = asType :: Prog_

prog_typ   = input_typ :-> output_typ
input_typ  = Typ "Input_"
output_typ = Typ "Output_"
move_typ   = Typ "Move_"
regs_typ   = Typ "Registers_"
dir_typ    = Typ "Dir_"
bool_typ   = Typ "Bool" 
int_typ   = Typ "Int" 




ctx :: Context
ctx = [
  ( "output_"          , move_typ :-> regs_typ :-> output_typ         ) ,

  ( "if'"              , bool_typ :-> move_typ :-> move_typ :-> move_typ ),
  ( "if'"              , bool_typ :-> output_typ :-> output_typ :-> output_typ ),


  ( "travel_"          , dir_typ :-> move_typ                         ) ,

  ( "split_"           , dir_typ :-> int_typ :-> regs_typ :-> move_typ ),

  ( "myLastTravel_"    , input_typ :-> dir_typ                        ) ,      
  ( "myRegs_"          , input_typ :-> regs_typ                       ) ,

  ( "myWasSuccess_"    , input_typ :-> bool_typ                       ),  

  ( "nAppleDir_"       , input_typ :-> dir_typ                        ),

  ( "rotCW_"           , dir_typ :-> dir_typ                          ) ,

  ( "dUp"              , dir_typ                                      ) ,
  ( "dDown"            , dir_typ                                      ) ,
  ( "dLeft"            , dir_typ                                      ) ,
  ( "dRight"           , dir_typ                                      ) ,
 
  ( "0"                , int_typ ),
  ( "1"                , int_typ ),
  ( "2"                , int_typ )

 ]





ff :: Prog_ -> (Double,Bool)
ff prog_ = 
  let w  = ff_world_complet_ prog_
      w' = steps w numSteps
      finalEnergy = fromIntegral $ solutionFliesSumEnergy w'
      --finalEnergy = fromIntegral . length . solutionFlies $ w' 
   in ( finalEnergy , False )


numSteps :: Int
numSteps = 100


jsData = jsObj [ 
 ( "world" , worldToJSON ff_solutionFlyPos ff_world_withEnvirFlies ),
 ( "im"    , imGraphInJSON prog_typ ctx )]








---------------------------------
-- worlds stuff -----------------
---------------------------------

wo1_ = ff_world_complet_      prog1_
wo1  = ff_world_complet  (snd prog1)

wo2_ = ff_world_complet_      prog2_
wo2  = ff_world_complet  (snd prog2)

wo4_ = ff_world_complet_      prog4_
wo4  = ff_world_complet  (snd prog4)


prog1_,prog2_,prog4_ :: Prog_

prog1_ = \ x -> output_ (travel_ dRight) (myRegs_ x)

prog2_ = \ x -> output_ (travel_ (nAppleDir_ x) ) (myRegs_ x) 

prog4_ i = if' ((xGet_ i) > 5) 
               (output_ ( split_ dDown (myEnergy_ i `div` 2) defaultRegs_ ) ( xSet_ 0 $ myRegs_ i) )
               ( if' ((yGet_ i) > 5)
                     (output_ ( travel_ (nAppleDir_ i) ) ( xInc_         $ myRegs_ i) )
                     (output_ ( travel_ dRight         ) ( xInc_ . yInc_ $ myRegs_ i) ) )


{- add: 
 >
 div
 2

 
-}


ff_world_complet_ :: Prog_ -> World
ff_world_complet_ = ff_world_complet . prog_2prog

ff_world_complet :: Prog -> World
ff_world_complet prog = 
 foldr (uncurry putFly) ff_world_withEnvirFlies
  [ ( ff_solutionFlyPos , ( "_" , prog ) ) ]

ff_solutionFlyPos :: Pos
ff_solutionFlyPos = (10,10)

ff_world_withEnvirFlies :: World
ff_world_withEnvirFlies  = 
 foldr (uncurry putFly) ff_world_noFlies
  [ ]--( (30,30) , prog1 ) ,
    --( (35, 2) , prog3 ) ]

ff_world_noFlies :: World
ff_world_noFlies = w1_orig






-------------------------------
-- adapter stuff --------------
-------------------------------

prog_2prog :: Prog_ -> Prog
prog_2prog prog_ = output_2output . prog_ . input2input_




input2input_ :: Input -> Input_
input2input_ input =
 ( (myEnergy     input 
 , dir2dir_ $ myLastTravel input
 , myWasSuccess input)
 
 , (dir2dir_ $ nAppleDir    input
 , nAppleDist   input
 , nAppleEnergy input 
 , dir2dir_ $ nFlyDir      input
 , nFlyDist     input
 , nFlyEnergy   input)
 
 , (dir2dir_ $ cAppleDir    input
 , cAppleDist   input)

 , regs2regs_ $ myRegs       input ) 


output_2output :: Output_ -> Output
output_2output ( move_, regs_ ) = Output{ 
 myMove     = case move_ of
               Left dir_             -> Travel $ dir_2dir dir_ 
               Right (dir_ ,en, rs_) -> Split  (dir_2dir dir_) en (regs_2regs rs_) ,
 myNextRegs = regs_2regs regs_ } 



dir2dir_ :: Dir -> Dir_
dir2dir_ dir = case dir of
  DUp    -> dUp   
  DDown  -> dDown  
  DLeft  -> dLeft   
  DRight -> dRight  


dir_2dir :: Dir_ -> Dir
dir_2dir dir_ 
  | dir_ == dUp    = DUp     
  | dir_ == dDown  = DDown    
  | dir_ == dLeft  = DLeft     
  | dir_ == dRight = DRight    

regs2regs_ :: Registers -> Registers_
regs2regs_ rs = ( xReg rs , yReg rs , zReg rs, dir2dir_ $ dReg rs )

regs_2regs :: Registers_ -> Registers
regs_2regs (x,y,z,d) = Registers x y z (dir_2dir d)




-------------------------------
-- world to json---------------
---------------------------------

worldToJSON :: Pos -> World -> JSValue
worldToJSON solutionFlyPos@(x,y) w =
  let mapaJSON = jsArr $ map (\row -> jsArr $ map f row ) $ transpose $ worldToLists defaultView w
   in jsObj [ ("mapa"            , mapaJSON) , 
              ("solutionFlyPos"  , jsArr [ jsNum x , jsNum y ] ) ,
              ("fliesToDo"       , posesToJSON (fliesToDo  w)   ) ,
              ("doneFlies"       , posesToJSON (doneFlies  w)   ) ,
              ("applePoses"      , posesToJSON (applePoses w)   ) ,
              ("numSteps"        , jsNum numSteps               ) ]
 where
  f :: Object -> JSValue
  f o = case o of
   Fly flyData -> jsObj [("type"     , jsStr "fly"  ),
                         ("progName" , jsStr (flyProgName flyData) ),
                         ("energy"   , jsNum (flyEnergy   flyData) )]
   Apple energy-> jsObj [("type", jsStr "apple"),("energy" , jsNum energy )]
   Wall _      -> jsObj [("type", jsStr "wall" )]
   Free        -> jsObj [("type", jsStr "free" )]

  posesToJSON :: [Pos] -> JSValue
  posesToJSON poses = jsArr $ map (\(x,y)->jsArr [ jsNum x , jsNum y ]) poses
  

