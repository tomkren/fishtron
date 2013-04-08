module Problems.Fly.Problem where


import Data.List

import TTerm          (Typ(..),Context)
import Problems.Utils ( cttProblem5 , asType )

import Text.JSON
import JSONUtils

import Problems.Fly.Funs ( Input_ , Output_ , Dir_  , Energy , Pos ,  
                           output_ , nearestApplePos_ ,nearestFlyPos_, myPos_ , posToDir_  ,
                           dStay , dUp , dDown , dLeft , dRight )
import Problems.Fly.Fly

problem1 = cttProblem5 "Fly" ff prog_typ ctx prog_type


jsData = worldToJSON ff_solutionFlyPos ff_world_withEnvirFlies 


prog_type = asType :: Input_ -> Output_
prog_typ  = input_typ :-> output_typ

input_typ  = Typ "Input_"
output_typ = Typ "Output_"
dir_typ    = Typ "Dir_"

m_pos = Typ "Maybe Pos"
pos_typ = Typ "Pos"

ctx :: Context
ctx = [
  ( "output_"          , dir_typ   :-> output_typ         ) ,
  ( "nearestApplePos_" , input_typ :-> m_pos              ) ,
  ( "nearestFlyPos_"   , input_typ :-> m_pos              ) ,
  ( "myPos_"           , input_typ :-> pos_typ            ) ,
  ( "posToDir_"        , pos_typ   :-> m_pos :-> dir_typ  )
 ]

--  nearestApplePos :: Maybe Pos ,
--  nearestFlyPos   :: Maybe Pos ,
--  myPos           :: Pos ,
--  inputEnergy     :: Energy 

-- nearestAppleDir :: Input -> Dir
-- Output :: Dir -> Output

prog_1 input = output_ $ posToDir_ (myPos_ input) (nearestApplePos_ input)
prog_2 _     = output_ $ dRight 
prog_3 input = output_ $ posToDir_ (myPos_ input) (nearestFlyPos_ input)



numSteps :: Int
numSteps = 100

ff :: (Input_->Output_) -> (Double,Bool)
ff prog_ = 
  let w  = ff_world_complet prog_
      w' = steps numSteps w
      finalEnergy = fromIntegral $ head $ energies w'
   in ( finalEnergy , False )

ff_world_complet :: Prog_ -> World
ff_world_complet prog_ = 
 foldr (uncurry putFly) ff_world_withEnvirFlies
  [ ( ff_solutionFlyPos , (prog_2prog prog_ , "_" ) ) ]

w0 = ff_world_complet prog_1

ff_solutionFlyPos :: Pos
ff_solutionFlyPos = (10,10)

ff_world_withEnvirFlies :: World
ff_world_withEnvirFlies  = 
 foldr (uncurry putFly) ff_world_noFlies
  [ ( (30,30) , prog1' ) ,
    ( (35, 2) , prog3' ) ]

ff_world_noFlies :: World
ff_world_noFlies = w1 

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
   Apple       -> jsObj [("type", jsStr "apple")]
   Wall        -> jsObj [("type", jsStr "wall" )]
   Free        -> jsObj [("type", jsStr "free" )]

  posesToJSON :: [Pos] -> JSValue
  posesToJSON poses = jsArr $ map (\(x,y)->jsArr [ jsNum x , jsNum y ]) poses
  



--mapka = intercalate "\n" $ map (concatMap (\o-> objChar o : " ")) $ worldToLists view w 

--worldToLists :: (Pos,Pos) -> World -> [[Object]]






type Prog_ = Input_ -> Output_



dir2dir_ :: Dir -> Dir_
dir2dir_ dir = case dir of
  DUp    -> dUp   
  DDown  -> dDown  
  DLeft  -> dLeft   
  DRight -> dRight  
  DStay  -> dStay   

dir_2dir :: Dir_ -> Dir
dir_2dir dir_ 
  | dir_ == dUp    = DUp     
  | dir_ == dDown  = DDown    
  | dir_ == dLeft  = DLeft     
  | dir_ == dRight = DRight    
  | dir_ == dStay  = DStay     

input2input_ :: Input -> Input_
input2input_ input = ( nearestApplePos input ,
                       nearestFlyPos   input ,
                       myPos           input , 
                       inputEnergy     input )

output_2output :: Output_ -> Output
output_2output dir_ = Output{ moveDir = dir_2dir dir_ } 

prog_2prog :: Prog_ -> Prog
prog_2prog prog_ = output_2output . prog_ . input2input_













