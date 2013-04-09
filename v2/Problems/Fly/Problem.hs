module Problems.Fly.Problem where


import Data.List

import TTerm          (Typ(..),Context)
import Problems.Utils ( cttProblem5 , asType )

import Text.JSON
import JSONUtils

import Problems.Fly.Funs ( Input_ , Output_ , Dir_  , Energy , Pos , avg , 
                           output_ , myApplePoses_ ,nearestFlyPos_, myPos_ , posToDir_  ,
                           dStay , dUp , dDown , dLeft , dRight )
import Problems.Fly.Fly

problem1 = cttProblem5 "Fly" ff prog_typ ctx prog_type


jsData = worldToJSON ff_solutionFlyPos ff_world_withEnvirFlies 


prog_type = asType :: Input_ -> Output_
prog_typ  = input_typ :-> output_typ

input_typ  = Typ "Input_"
output_typ = Typ "Output_"
dir_typ    = Typ "Dir_"

m_pos   = Typ "Maybe Pos"
l_pos   = Typ "List Pos"
pos_typ = Typ "Pos"

dou     = Typ "Double"
boolean = Typ "Bool"

ctx :: Context
ctx = [
  ( "output_"          , dir_typ   :-> output_typ                     ) ,
  ( "myApplePoses_"    , input_typ :-> l_pos                          ) ,
  ( "nearestFlyPos_"   , input_typ :-> m_pos                          ) ,
  ( "myPos_"           , input_typ :-> pos_typ                        ) ,
  ( "posToDir_"        , pos_typ   :-> m_pos   :-> dir_typ            ) ,
  ( "dist"             , pos_typ   :-> pos_typ :-> dou                ) ,
  ( "dStay"            , dir_typ                                      ) ,
  ( "dUp"              , dir_typ                                      ) ,
  ( "dDown"            , dir_typ                                      ) ,
  ( "dLeft"            , dir_typ                                      ) ,
  ( "dRight"           , dir_typ                                      ) ,
  ( "(<=)"             , dou :-> dou :-> boolean                      ) ,
  ( "if'"              , boolean :-> dir_typ :-> dir_typ :-> dir_typ  ) ,
  ( "head_"            , l_pos :-> m_pos                              ) ,
  ( "avg"              , l_pos :-> m_pos                              ) 

 ]





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


ff_world_complet2 :: Prog_ -> World
ff_world_complet2 prog_ = 
 foldr (uncurry putFly) ff_world_withEnvirFlies2
  [ ( ff_solutionFlyPos2 , (prog_2prog prog_ , "_" ) ) ]

ff_solutionFlyPos2 :: Pos
ff_solutionFlyPos2 = (2,2)

ff_world_withEnvirFlies2 :: World
ff_world_withEnvirFlies2  = 
 foldr (uncurry putFly) ff_world_noFlies2
  [  ]


ff_world_noFlies2 :: World
ff_world_noFlies2 = worldFromStrs [
--1234567890123456789012345678901234567890
 "........................................",
 ".WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW.",
 ".W....A..A..A...A...A..A.....A...A...AW.",
 ".W....WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW.W.",
 ".W..................................W.W.",
 ".W..................................W.W.",
 ".WWW.AAAA...........................W.W.",
 ".W.W..AAAA..........................W.W.",
 ".W.W...AAAA.........................W.W.",
 ".W.W....AAAAA.......................WAW.",
 ".W.W.....AAAAAA.....................W.W.",
 ".W.W......AAAAAAAA..................W.W.",
 ".W.W......AAAAAAAA..................W.W.",
 ".WAW.......AAAAAAAAA................WAW.",
 ".W.W.......AAAAAAAAAAAA.............W.W.",
 ".W.W......AAAAAAAAAAAAAA............W.W.",
 ".W.W.....AAAAAAAAAAAAAAAAA..........W.W.",
 ".W.W.....AAAAAAAAAAAAAAAAAAA........W.W.",
 ".W.W....AAAAAAAAAAAAAAAAAAAAAA......WAW.",
 ".W.W....AAAAAAAAAAAAAAAAAAAAAA......W.W.",
 ".WAW....AAAAAAAAAAAAAAAAAAAAAA......W.W.",
 ".W.W....AAAAAAAAAAAAAAAAAAAAAA......W.W.",
 ".W.W....AAAAAAAAAAAAAAAAAAAAAA......WAW.",
 ".W.W....AAAAAAAAAAAAAAAAAAAAAA......W.W.",
 ".W.W.....AAAAAAAAAAAAAAAAAAAAA......W.W.",
 ".WAW.....AAAAAAAAAAAAAAAAAAAA.......WAW.",
 ".W.W......AAAAAAAAAAAAAAAAA.........W.W.",
 ".W.W.........AAAAAAAAAAAAA..........W.W.",
 ".W.W.............AAAAAAAAA..........WAW.",
 ".W.W................................W.W.",
 ".W.W................................W.W.",
 ".WAW................................W.W.",
 ".WAW................................W.W.",
 ".W.W................................WAW.",
 ".W.W................................W.W.",
 ".W.W................................W.W.",
 ".W.WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW.W.",
 ".WA....A....A.....A.....A.....A.......W.",
 ".WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW.",
 "........................................"]

-- ff_world_noFlies2 :: World
-- ff_world_noFlies2 = worldFromStrs [
-- --1234567890123456789012345678901234567890
--  "........................................",
--  ".WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW.",
--  ".W....A..A..A.A...A.A..A.A...A...A..AAW.",
--  ".W....WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW.W.",
--  ".W..................................W.W.",
--  ".W..................................W.W.",
--  ".WWW................................W.W.",
--  ".W.W................................W.W.",
--  ".W.W................................W.W.",
--  ".W.W................................WAW.",
--  ".W.W................................W.W.",
--  ".W.W.....AAAAAAAAAAAAAAAAAAAAA......W.W.",
--  ".W.W.....AAAAAAAAAAAAAAAAAAAAA......W.W.",
--  ".WAW.....AAAAAAAAAAAAAAAAAAAAA......WAW.",
--  ".W.W.....AAAAAAAAAAAAAAAAAAAAA......W.W.",
--  ".W.W.....AAAAAAAAAAAAAAAAAAAAA......W.W.",
--  ".W.W.....AAAAAAAAAAAAAAAAAAAAA......W.W.",
--  ".W.W.....AAAAAAAAAAAAAAAAAAAAA......W.W.",
--  ".W.W.....AAAAAAAAAAAAAAAAAAAAA......WAW.",
--  ".W.W.....AAAAAAAAAAAAAAAAAAAAA......W.W.",
--  ".WAW.....AAAAAAAAAAAAAAAAAAAAA......W.W.",
--  ".W.W.....AAAAAAAAAAAAAAAAAAAAA......W.W.",
--  ".W.W.....AAAAAAAAAAAAAAAAAAAAA......WAW.",
--  ".W.W.....AAAAAAAAAAAAAAAAAAAAA......W.W.",
--  ".W.W.....AAAAAAAAAAAAAAAAAAAAA......W.W.",
--  ".WAW.....AAAAAAAAAAAAAAAAAAAAA......WAW.",
--  ".W.W.....AAAAAAAAAAAAAAAAAAAAA......W.W.",
--  ".W.W.....AAAAAAAAAAAAAAAAAAAAA......W.W.",
--  ".W.W.....AAAAAAAAAAAAAAAAAAAAA......WAW.",
--  ".W.W................................W.W.",
--  ".W.W................................W.W.",
--  ".WAW................................W.W.",
--  ".WAW................................W.W.",
--  ".W.W................................WAW.",
--  ".W.W................................W.W.",
--  ".W.W................................W.W.",
--  ".W.WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW.W.",
--  ".WA....A....A.....A.....A.....A......AW.",
--  ".WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW.",
--  "........................................"]


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
input2input_ input = ( myApplePoses    input ,
                       nearestFlyPos   input ,
                       myPos           input , 
                       inputEnergy     input )

output_2output :: Output_ -> Output
output_2output dir_ = Output{ moveDir = dir_2dir dir_ } 

prog_2prog :: Prog_ -> Prog
prog_2prog prog_ = output_2output . prog_ . input2input_













