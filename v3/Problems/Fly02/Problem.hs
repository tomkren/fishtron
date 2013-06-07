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

  cttp_gOpt        = CTTG_Geom     prog_typ ctx 0.75 ,  --CTTG_Koza2 prog_typ ctx    , 
  
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
int_typ    = Typ "Int" 
dist_typ   = Typ "Double"


--function (x0){return output_(if_(lte(xGet_(x0),zGet_(x0)),easySplit(x0),travel_(dLeft)),xInc_(myRegs_(x0)));} 

ctx :: Context
ctx = [


  ( "output_"          , move_typ :-> regs_typ :-> output_typ         ) ,

  ( "if'"              , bool_typ :-> output_typ :-> output_typ :-> output_typ ),
  ( "if'"              , bool_typ :-> move_typ :-> move_typ :-> move_typ ),
  ( "if'"              , bool_typ :-> dir_typ :-> dir_typ :-> dir_typ ),
  ( "if'"              , bool_typ :-> int_typ :-> int_typ :-> int_typ ),
  ( "if'"              , bool_typ :-> dist_typ :-> dist_typ :-> dist_typ ),
  ( "if'"              , bool_typ :-> regs_typ :-> regs_typ :-> regs_typ ),
  


  ( "easySplit"        , input_typ :-> move_typ ),
  ( "travel_"          , dir_typ :-> move_typ                         ) ,
  ( "split_"           , dir_typ :-> int_typ :-> regs_typ :-> move_typ ),


  ( "myEnergy_"        , input_typ :-> int_typ                         ),
  ( "myLastTravel_"    , input_typ :-> dir_typ                        ) ,      
  ( "myWasSuccess_"    , input_typ :-> bool_typ                       ),  


  ( "nAppleDir_"       , input_typ :-> dir_typ                        ),

  ( "nAppleDir_"       ,  input_typ :-> dir_typ                       ),           
  ( "nAppleDist_"      ,  input_typ :-> dist_typ                      ),            
  ( "nAppleEnergy_"    ,  input_typ :-> int_typ                       ),           
  ( "nFlyDir_"         ,  input_typ :-> dir_typ                       ),             
  ( "nFlyDist_"        ,  input_typ :-> dist_typ                      ),             
  ( "nFlyEnergy_"      ,  input_typ :-> int_typ                       ),           
  
  ( "cAppleDir_"       , input_typ :-> dir_typ                         ),
  ( "cAppleDist_"      , input_typ :-> dist_typ                        ),
  
  ( "myRegs_"          , input_typ :-> regs_typ                       ) ,

  ( "xGet_"            , input_typ :-> int_typ                       ),
  ( "yGet_"            , input_typ :-> int_typ                       ),
  ( "zGet_"            , input_typ :-> int_typ                       ),
  ( "dGet_"            , input_typ :-> dir_typ                       ),

  ( "xSet_"            , int_typ :-> regs_typ :-> regs_typ           ),
  ( "ySet_"            , int_typ :-> regs_typ :-> regs_typ           ),
  ( "zSet_"            , int_typ :-> regs_typ :-> regs_typ           ),
  ( "dSet_"            , dir_typ :-> regs_typ :-> regs_typ           ),

  ( "xInc_"            , regs_typ :-> regs_typ                       ),
  ( "yInc_"            , regs_typ :-> regs_typ                       ),
  ( "zInc_"            , regs_typ :-> regs_typ                       ),


  ( "rotCW_"           , dir_typ :-> dir_typ                          ) ,

  ( "dUp"              , dir_typ                                      ) ,
  ( "dDown"            , dir_typ                                      ) ,
  ( "dLeft"            , dir_typ                                      ) ,
  ( "dRight"           , dir_typ                                      ) ,

  ( "(==)" , int_typ :-> int_typ :-> bool_typ ),
  ( "(<=)" , int_typ :-> int_typ :-> bool_typ ),
 
  ( "0"                , int_typ ),
  ( "1"                , int_typ ),
  ( "2"                , int_typ )


--  ( "(+)"  , int_typ :-> int_typ :-> int_typ  ),
--  ( "(-)"  , int_typ :-> int_typ :-> int_typ  ),
--  ( "(*)"  , int_typ :-> int_typ :-> int_typ  ),
--  ( "div"  , int_typ :-> int_typ :-> int_typ  ),

--  ( "(+)"  , dist_typ :-> dist_typ :-> dist_typ  ),
--  ( "(-)"  , dist_typ :-> dist_typ :-> dist_typ  ),
--  ( "(*)"  , dist_typ :-> dist_typ :-> dist_typ  ),
 


--  ( "(==)" , dist_typ :-> dist_typ :-> bool_typ ),
--  ( "(<=)" , dist_typ :-> dist_typ :-> bool_typ ),

-- ( "(&&)" , bool_typ :-> bool_typ :-> bool_typ ),
-- ( "(||)" , bool_typ :-> bool_typ :-> bool_typ ),
-- ( "not"  , bool_typ :-> bool_typ ),



-- ( "0"                , dist_typ ),
-- ( "1"                , dist_typ ),
-- ( "2"                , dist_typ )

 ]





ff_old :: Prog_ -> (Double,Bool)
ff_old prog_ = 
  let w0  = ff_world_complet_ prog_
      w0' = steps w0 numSteps
      finalEnergy0 = fromIntegral $ solutionFliesSumEnergy w0'

      w1  = ff_world_complet2 prog_
      w1' = steps w1 numSteps
      finalEnergy1 = fromIntegral $ solutionFliesSumEnergy w1'

      finalEnergy2 = computeFFoneWorld prog_ numSteps wFun3 

      --finalEnergy = fromIntegral . length . solutionFlies $ w' 
   in ( finalEnergy0 + finalEnergy1 + finalEnergy2 , False )


ff :: Prog_ -> (Double,Bool)
ff prog_ = 
  let --wFuns = [ wFun3 ]
      wFuns = [ ff_world_complet_ , ff_world_complet2 , wFun3 ]
   in ( computeFF wFuns numSteps prog_ , False ) 

computeFF :: [(Prog_ -> World)] -> Int -> Prog_ -> Double
computeFF wFuns numSteps prog_ =
 let suma = sum $ map (computeFFoneWorld prog_ numSteps) wFuns 
  in max 0 ( suma  )

computeFFoneWorld :: Prog_ -> Int -> (Prog_ -> World) -> Double
computeFFoneWorld prog_ numSteps wFun = 
  let w0 = (wFun prog_)
      w1 = steps w0 numSteps
   in fromIntegral $ solutionFliesSumEnergy w1


numSteps :: Int
numSteps = 100


jsData = jsObj [ 
 ( "Levels" , jsObj [
  ( "w0" , worldToJSON ff_solutionFlyPos  ff_world_withEnvirFlies  ),
  ( "w1" , worldToJSON ff_solutionFlyPos2 ff_world_withEnvirFlies2 ),
  ( "w3" , wJS3 )
  ]),
 ( "im"     , imGraphInJSON prog_typ ctx )]








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




( wFun3 , wJS3 ) = mkWorld2 (35,20) [] [
--01234567890123456789012345678901234567890
 "........................................",
 ".WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.......W.W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.......WAW.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.......W.W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WAAAAAAAAAAAAAAAAAAAAAAAAAAA.........W.",
 ".WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW.",
 "........................................"]


mkWorld2 :: Pos -> [(Pos,(String,Prog))] -> [String] -> ( Prog_ -> World , JSValue )
mkWorld2 solutionPos otherFlies wStrs =
 let worldWithEnvirFlies = foldr (uncurry putFly) (worldFromStrs wStrs) otherFlies
  in ( \ prog_ -> putFly solutionPos ("_", prog_2prog prog_) worldWithEnvirFlies 
     , worldToJSON solutionPos worldWithEnvirFlies )   


mkWorld :: Pos -> [(Pos,(String,Prog))] -> [String] -> ( Prog_ -> World ) 
mkWorld solutionPos otherFlies wStrs prog_ = 
  foldr (uncurry putFly) (worldFromStrs wStrs) 
  ( (solutionPos ,( "_" , prog_2prog prog_ )) : otherFlies )




ff_world_complet2 :: Prog_ -> World
ff_world_complet2 prog_ = 
 foldr (uncurry putFly) ff_world_withEnvirFlies2
  [ ( ff_solutionFlyPos2 , ( "_" , prog_2prog prog_ ) ) ]

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
  

-- BUG v hs 8 v js 2 - W0 ten uplne defaultni

-- function (x0){return output_(if_(myWasSuccess_(x0),if_(myWasSuccess_(x0),travel_(rotCW_(rotCW_(rotCW_(cAppleDir_(x0))))),travel_(dLeft)),travel_(rotCW_(dDown))),zInc_(dSet_(dRight,xInc_(myRegs_(x0)))));}

 -- \ x0 -> output_ (if' (myWasSuccess_ x0) (if' (myWasSuccess_ x0) (travel_ ( │
 --  rotCW_ (rotCW_ (rotCW_ (cAppleDir_ x0))))) (travel_ dLeft)) (travel_ (rotC │
 --  W_ dDown))) (zInc_ (dSet_ dRight (xInc_ (myRegs_ x0))))                   

----------------------------

{--



runs 5
generations 25
pop size 1000

steps 100

fly energy 1







47 : gene 0

 ┌────────────────────────┐
 │ Run                1/5 │
 │ Genration            0 │
 ├────────────────────────┤
 │ Best          47.00000 │
 │ Average        9.54800 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (travel_ (if' ((==) 0 (myEnergy_ x0)) dLeft (dGet_ x0))) ( │
 │ yInc_ (xInc_ (dSet_ (cAppleDir_ x0) (myRegs_ x0))))                        │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(travel_(if_(equals(0,myEnergy_(x0)),dLeft,dGet_(x0))),yInc_(xInc_(dSet_(cAppleDir_(x0),myRegs_(x0)))));}">47</a>


 ┌────────────────────────┐
 │ Run                1/5 │
 │ Genration           11 │
 ├────────────────────────┤
 │ Best         187.00000 │
 │ Average       39.11800 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (if' ((==) (myEnergy_ x0) (zGet_ x0)) (travel_ (cAppleDir_ │
 │  x0)) (travel_ (nAppleDir_ x0))) (dSet_ dLeft (yInc_ (zSet_ 1 (xInc_ (myRe │
 │ gs_ x0)))))                                                                │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(if_(equals(myEnergy_(x0),zGet_(x0)),travel_(cAppleDir_(x0)),travel_(nAppleDir_(x0))),dSet_(dLeft,yInc_(zSet_(1,xInc_(myRegs_(x0))))));}">187</a>



 ┌────────────────────────┐
 │ Run                1/5 │
 │ Genration           13 │
 ├────────────────────────┤
 │ Best         188.00000 │
 │ Average       41.85200 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (if' ((==) (myEnergy_ x0) (nAppleEnergy_ x0)) (travel_ (cA │
 │ ppleDir_ x0)) (travel_ (nAppleDir_ x0))) (dSet_ (cAppleDir_ x0) (yInc_ (zS │
 │ et_ 1 (xInc_ (myRegs_ x0)))))                                              │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(if_(equals(myEnergy_(x0),1),travel_(if_(equals(0,myEnergy_(x0)),nAppleDir_(x0),if_(equals(0,nAppleEnergy_(x0)),dLeft,if_(equals(0,myEnergy_(x0)),dLeft,cAppleDir_(x0))))),travel_(nAppleDir_(x0))),dSet_(dLeft,yInc_(zSet_(1,dSet_(dLeft,yInc_(xInc_(dSet_(if_(lte(1,0),nAppleDir_(x0),dDown),yInc_(zSet_(1,xInc_(zSet_(1,myRegs_(x0)))))))))))));}">188</a>
------------------------

 ┌────────────────────────┐
 │ Run                2/5 │
 │ Genration            0 │
 ├────────────────────────┤
 │ Best          42.00000 │
 │ Average       12.09300 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (travel_ (if' ((==) (nFlyEnergy_ x0) 0) (cAppleDir_ x0) dD │
 │ own)) (zSet_ 2 (dSet_ dDown (myRegs_ x0)))                                 │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(travel_(if_(equals(nFlyEnergy_(x0),0),cAppleDir_(x0),dDown)),zSet_(2,dSet_(dDown,myRegs_(x0))));}">42</a>


 ┌────────────────────────┐
 │ Run                2/5 │
 │ Genration           11 │
 ├────────────────────────┤
 │ Best          47.00000 │
 │ Average       41.32400 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (travel_ (dGet_ x0)) (xInc_ (dSet_ (cAppleDir_ x0) (myRegs │
 │ _ x0)))                                                                    │
 └────────────────────────────────────────────────────────────────────────────┘

<a href="#" title="function (x0){return output_(travel_(dGet_(x0)),xInc_(dSet_(cAppleDir_(x0),myRegs_(x0))));}">47</a>
---------------------------------



BEST OF #1
<a href="#" title="function (x0){return if_(lte(2,nFlyEnergy_(x0)),output_(travel_(dRight),myRegs_(x0)),output_(travel_(dLeft),dSet_(dLeft,zInc_(myRegs_(x0)))));}">30</a>
<a href="#" title="function (x0){return if_(equals(yGet_(x0),1),output_(travel_(rotCW_(dDown)),myRegs_(x0)),output_(travel_(if_(myWasSuccess_(x0),dLeft,rotCW_(dLeft))),myRegs_(x0)));}">49</a>
<a href="#" title="function (x0){return if_(myWasSuccess_(x0),output_(travel_(dLeft),myRegs_(x0)),output_(travel_(nAppleDir_(x0)),myRegs_(x0)));}">55</a>
<a href="#" title="function (x0){return if_(myWasSuccess_(x0),if_(lte(yGet_(x0),1),output_(travel_(dLeft),myRegs_(x0)),output_(travel_(rotCW_(dRight)),ySet_(2,myRegs_(x0)))),output_(travel_(nAppleDir_(x0)),yInc_(dSet_(dDown,myRegs_(x0)))));}">68</a>
<a href="#" title="function (x0){return if_(lte(yGet_(x0),nAppleEnergy_(x0)),output_(travel_(dDown),yInc_(myRegs_(x0))),output_(travel_(nAppleDir_(x0)),yInc_(myRegs_(x0))));}">109</a>
<a href="#" title="function (x0){return if_(myWasSuccess_(x0),if_(lte(yGet_(x0),1),output_(travel_(dLeft),myRegs_(x0)),output_(travel_(nAppleDir_(x0)),myRegs_(x0))),output_(travel_(nAppleDir_(x0)),yInc_(dSet_(dDown,myRegs_(x0)))));}">124</a>
<a href="#" title="function (x0){return if_(myWasSuccess_(x0),if_(lte(yGet_(x0),1),output_(travel_(dLeft),myRegs_(x0)),output_(travel_(nAppleDir_(x0)),myRegs_(x0))),output_(travel_(dDown),yInc_(dSet_(dDown,myRegs_(x0)))));}">197</a>
<a href="#" title="function (x0){return if_(myWasSuccess_(x0),if_(lte(yGet_(x0),1),output_(travel_(dLeft),myRegs_(x0)),output_(travel_(nAppleDir_(x0)),myRegs_(x0))),output_(travel_(dDown),yInc_(zInc_(ySet_(1,myRegs_(x0))))));}">198</a>
<a href="#" title="function (x0){return if_(myWasSuccess_(x0),if_(lte(yGet_(x0),nFlyEnergy_(x0)),output_(travel_(dLeft),myRegs_(x0)),if_(lte(yGet_(x0),1),output_(travel_(dDown),yInc_(myRegs_(x0))),output_(travel_(nAppleDir_(x0)),myRegs_(x0)))),output_(travel_(dDown),yInc_(dSet_(dDown,myRegs_(x0)))));}">199</a>


 ┌────────────────────────┐
 │ Run                3/5 │
 │ Genration            0 │
 ├────────────────────────┤
 │ Best          30.00000 │
 │ Average        3.50800 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> if' ((<=) 2 (nFlyEnergy_ x0)) (output_ (travel_ dRight) (myRegs_ x │
 │ 0)) (output_ (travel_ dLeft) (dSet_ dLeft (zInc_ (myRegs_ x0))))           │
 └────────────────────────────────────────────────────────────────────────────┘

<a href="#" title="function (x0){return if_(lte(2,nFlyEnergy_(x0)),output_(travel_(dRight),myRegs_(x0)),output_(travel_(dLeft),dSet_(dLeft,zInc_(myRegs_(x0)))));}">30</a>

 ┌────────────────────────┐
 │ Run                3/5 │
 │ Genration            2 │
 ├────────────────────────┤
 │ Best          49.00000 │
 │ Average       17.47600 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> if' ((==) (yGet_ x0) 1) (output_ (travel_ (rotCW_ dDown)) (myRegs_ │
 │  x0)) (output_ (travel_ (if' (myWasSuccess_ x0) dLeft (rotCW_ dLeft))) (my │
 │ Regs_ x0))                                                                 │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return if_(equals(yGet_(x0),1),output_(travel_(rotCW_(dDown)),myRegs_(x0)),output_(travel_(if_(myWasSuccess_(x0),dLeft,rotCW_(dLeft))),myRegs_(x0)));}">49</a>


55

 ┌────────────────────────┐
 │ Run                3/5 │
 │ Genration            3 │
 ├────────────────────────┤
 │ Best          55.00000 │
 │ Average       25.97600 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> if' (myWasSuccess_ x0) (output_ (travel_ dLeft) (myRegs_ x0)) (out │
 │ put_ (travel_ (nAppleDir_ x0)) (myRegs_ x0))                               │
 └────────────────────────────────────────────────────────────────────────────┘


<a href="#" title="function (x0){return if_(myWasSuccess_(x0),output_(travel_(dLeft),myRegs_(x0)),output_(travel_(nAppleDir_(x0)),myRegs_(x0)));}">55</a>


68

 ┌────────────────────────┐
 │ Run                3/5 │
 │ Genration            5 │
 ├────────────────────────┤
 │ Best          68.00000 │
 │ Average       28.46700 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> if' (myWasSuccess_ x0) (if' ((<=) (yGet_ x0) 1) (output_ (travel_  │
 │ dLeft) (myRegs_ x0)) (output_ (travel_ (rotCW_ dRight)) (ySet_ 2 (myRegs_  │
 │ x0)))) (output_ (travel_ (nAppleDir_ x0)) (yInc_ (dSet_ dDown (myRegs_ x0) │
 │ )))                                                                        │
 └────────────────────────────────────────────────────────────────────────────┘

<a href="#" title="function (x0){return if_(myWasSuccess_(x0),if_(lte(yGet_(x0),1),output_(travel_(dLeft),myRegs_(x0)),output_(travel_(rotCW_(dRight)),ySet_(2,myRegs_(x0)))),output_(travel_(nAppleDir_(x0)),yInc_(dSet_(dDown,myRegs_(x0)))));}">68</a>

109


 ┌────────────────────────┐
 │ Run                3/5 │
 │ Genration            6 │
 ├────────────────────────┤
 │ Best         109.00000 │
 │ Average       28.52000 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> if' ((<=) (yGet_ x0) (nAppleEnergy_ x0)) (output_ (travel_ dDown)  │
 │ (yInc_ (myRegs_ x0))) (output_ (travel_ (nAppleDir_ x0)) (yInc_ (myRegs_ x │
 │ 0)))                                                                       │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return if_(lte(yGet_(x0),nAppleEnergy_(x0)),output_(travel_(dDown),yInc_(myRegs_(x0))),output_(travel_(nAppleDir_(x0)),yInc_(myRegs_(x0))));}">109</a>


124

 ┌────────────────────────┐
 │ Run                3/5 │
 │ Genration            7 │
 ├────────────────────────┤
 │ Best         124.00000 │
 │ Average       29.55500 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> if' (myWasSuccess_ x0) (if' ((<=) (yGet_ x0) 1) (output_ (travel_  │
 │ dLeft) (myRegs_ x0)) (output_ (travel_ (nAppleDir_ x0)) (myRegs_ x0))) (ou │
 │ tput_ (travel_ (nAppleDir_ x0)) (yInc_ (dSet_ dDown (myRegs_ x0))))        │
 └────────────────────────────────────────────────────────────────────────────┘


<a href="#" title="function (x0){return if_(myWasSuccess_(x0),if_(lte(yGet_(x0),1),output_(travel_(dLeft),myRegs_(x0)),output_(travel_(nAppleDir_(x0)),myRegs_(x0))),output_(travel_(nAppleDir_(x0)),yInc_(dSet_(dDown,myRegs_(x0)))));}">124</a>


197


 ┌────────────────────────┐
 │ Run                3/5 │
 │ Genration           11 │
 ├────────────────────────┤
 │ Best         197.00000 │
 │ Average       36.94500 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> if' (myWasSuccess_ x0) (if' ((<=) (yGet_ x0) 1) (output_ (travel_  │
 │ dLeft) (myRegs_ x0)) (output_ (travel_ (nAppleDir_ x0)) (myRegs_ x0))) (ou │
 │ tput_ (travel_ dDown) (yInc_ (dSet_ dDown (myRegs_ x0))))                  │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return if_(myWasSuccess_(x0),if_(lte(yGet_(x0),1),output_(travel_(dLeft),myRegs_(x0)),output_(travel_(nAppleDir_(x0)),myRegs_(x0))),output_(travel_(dDown),yInc_(dSet_(dDown,myRegs_(x0)))));}">197</a>

198


 ┌────────────────────────┐
 │ Run                3/5 │
 │ Genration           13 │
 ├────────────────────────┤
 │ Best         198.00000 │
 │ Average       51.79600 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> if' (myWasSuccess_ x0) (if' ((<=) (yGet_ x0) 1) (output_ (travel_  │
 │ dLeft) (myRegs_ x0)) (output_ (travel_ (nAppleDir_ x0)) (myRegs_ x0))) (ou │
 │ tput_ (travel_ dDown) (yInc_ (zInc_ (ySet_ 1 (myRegs_ x0)))))              │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return if_(myWasSuccess_(x0),if_(lte(yGet_(x0),1),output_(travel_(dLeft),myRegs_(x0)),output_(travel_(nAppleDir_(x0)),myRegs_(x0))),output_(travel_(dDown),yInc_(zInc_(ySet_(1,myRegs_(x0))))));}">198</a>

199


 ┌────────────────────────┐
 │ Run                3/5 │
 │ Genration           22 │
 ├────────────────────────┤
 │ Best         199.00000 │
 │ Average      136.56700 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> if' (myWasSuccess_ x0) (if' ((<=) (yGet_ x0) (nFlyEnergy_ x0)) (ou │
 │ tput_ (travel_ dLeft) (myRegs_ x0)) (if' ((<=) (yGet_ x0) 1) (output_ (tra │
 │ vel_ dDown) (yInc_ (myRegs_ x0))) (output_ (travel_ (nAppleDir_ x0)) (myRe │
 │ gs_ x0)))) (output_ (travel_ dDown) (yInc_ (dSet_ dDown (myRegs_ x0))))    │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return if_(myWasSuccess_(x0),if_(lte(yGet_(x0),nFlyEnergy_(x0)),output_(travel_(dLeft),myRegs_(x0)),if_(lte(yGet_(x0),1),output_(travel_(dDown),yInc_(myRegs_(x0))),output_(travel_(nAppleDir_(x0)),myRegs_(x0)))),output_(travel_(dDown),yInc_(dSet_(dDown,myRegs_(x0)))));}">199</a>

-------------------------------------------------


 ┌────────────────────────┐
 │ Run                4/5 │
 │ Genration            0 │
 ├────────────────────────┤
 │ Best          42.00000 │
 │ Average        9.69000 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (if' ((==) 2 1) (split_ dUp 1 (zInc_ (myRegs_ x0))) (trave │
 │ l_ (cAppleDir_ x0))) (yInc_ (myRegs_ x0))                                  │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(if_(equals(2,1),split_(dUp,1,zInc_(myRegs_(x0))),travel_(cAppleDir_(x0))),yInc_(myRegs_(x0)));}">42</a>

 ┌────────────────────────┐
 │ Run                4/5 │
 │ Genration           11 │
 ├────────────────────────┤
 │ Best          43.00000 │
 │ Average       32.63700 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (if' ((==) 2 2) (if' ((==) 2 (xGet_ x0)) (travel_ dUp) (tr │
 │ avel_ (cAppleDir_ x0))) (travel_ (nAppleDir_ x0))) (ySet_ 0 (ySet_ 1 (zInc │
 │ _ (xInc_ (myRegs_ x0)))))                                                  │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(if_(equals(2,2),if_(equals(2,xGet_(x0)),travel_(dUp),travel_(cAppleDir_(x0))),travel_(nAppleDir_(x0))),ySet_(0,ySet_(1,zInc_(xInc_(myRegs_(x0))))));}">43</a>


--------------------------------------------------------------


 ┌────────────────────────┐
 │ Run                5/5 │
 │ Genration            0 │
 ├────────────────────────┤
 │ Best          42.00000 │
 │ Average        4.54900 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (if' ((==) 1 0) (travel_ dLeft) (travel_ (cAppleDir_ x0))) │
 │  (zSet_ (zGet_ x0) (myRegs_ x0))                                           │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(if_(equals(1,0),travel_(dLeft),travel_(cAppleDir_(x0))),zSet_(zGet_(x0),myRegs_(x0)));}">42</a>



 ┌────────────────────────┐
 │ Run                5/5 │
 │ Genration            7 │
 ├────────────────────────┤
 │ Best          43.00000 │
 │ Average       36.21300 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (if' ((==) 2 1) (travel_ dRight) (if' ((==) 2 1) (travel_  │
 │ dUp) (if' ((==) 1 (xGet_ x0)) (travel_ dUp) (travel_ (cAppleDir_ x0))))) ( │
 │ xInc_ (myRegs_ x0))                                                        │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(if_(equals(2,1),travel_(dRight),if_(equals(2,1),travel_(dUp),if_(equals(1,xGet_(x0)),travel_(dUp),travel_(cAppleDir_(x0))))),xInc_(myRegs_(x0)));}">43</a>


 ┌────────────────────────┐
 │ Run                5/5 │
 │ Genration           10 │
 ├────────────────────────┤
 │ Best         188.00000 │
 │ Average       37.60500 │
 │ Worst          3.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (if' ((==) 1 (myEnergy_ x0)) (travel_ (cAppleDir_ x0)) (tr │
 │ avel_ (nAppleDir_ x0))) (dSet_ (rotCW_ dUp) (xInc_ (myRegs_ x0)))          │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(if_(equals(1,myEnergy_(x0)),travel_(cAppleDir_(x0)),travel_(nAppleDir_(x0))),dSet_(rotCW_(dUp),xInc_(myRegs_(x0))));}">188</a>


Run time was 1961.7 seconds.







--}



{-- 


runs 5
generations 30
pop size 1000

steps 100

fly energy 100 + easySplit


 ┌────────────────────────┐
 │ Run                1/5 │
 │ Genration            0 │
 ├────────────────────────┤
 │ Best         332.00000 │
 │ Average      311.83000 │
 │ Worst        300.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (travel_ (nAppleDir_ x0)) (dSet_ (if' ((==) (myEnergy_ x0) │
 │  0) dLeft (rotCW_ (rotCW_ dLeft))) (myRegs_ x0))                           │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(travel_(nAppleDir_(x0)),dSet_(if_(equals(myEnergy_(x0),0),dLeft,rotCW_(rotCW_(dLeft))),myRegs_(x0)));}">332</a>

 ┌────────────────────────┐
 │ Run                1/5 │
 │ Genration            4 │
 ├────────────────────────┤
 │ Best         339.00000 │
 │ Average      313.08000 │
 │ Worst        300.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (travel_ (cAppleDir_ x0)) (myRegs_ x0) │
 └────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(travel_(cAppleDir_(x0)),myRegs_(x0));}">339</a>


--------------------------------------------------------

BO2!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

gen 20<a href="#" title="function (x0){return if_(myWasSuccess_(x0),output_(easySplit(x0),zInc_(myRegs_(x0))),output_(travel_(if_(lte(yGet_(x0),1),dLeft,if_(lte(yGet_(x0),1),dLeft,nAppleDir_(x0)))),xSet_(2,xInc_(xInc_(yInc_(zInc_(xSet_(yGet_(x0),xInc_(dSet_(dLeft,myRegs_(x0)))))))))));}">1336</a>
gen 17<a href="#" title="function (x0){return if_(myWasSuccess_(x0),output_(split_(dUp,myEnergy_(x0),myRegs_(x0)),myRegs_(x0)),output_(travel_(dGet_(x0)),dSet_(dLeft,dSet_(dGet_(x0),zSet_(2,xInc_(myRegs_(x0)))))));}">817</a>
gen 7<a href="#" title="function (x0){return if_(myWasSuccess_(x0),output_(split_(dUp,myEnergy_(x0),myRegs_(x0)),myRegs_(x0)),output_(travel_(if_(lte(yGet_(x0),1),dLeft,rotCW_(dDown))),xSet_(2,xInc_(myRegs_(x0)))));}">815</a>
gen 1<a href="#" title="function (x0){return if_(myWasSuccess_(x0),output_(easySplit(x0),ySet_(nAppleEnergy_(x0),yInc_(dSet_(dLeft,myRegs_(x0))))),output_(travel_(dLeft),myRegs_(x0)));}">489</a>
gen 0<a href="#" title="function (x0){return if_(myWasSuccess_(x0),output_(travel_(rotCW_(dDown)),zSet_(if_(lte(2,myEnergy_(x0)),1,1),yInc_(yInc_(yInc_(myRegs_(x0)))))),output_(travel_(nAppleDir_(x0)),dSet_(dDown,xSet_(1,myRegs_(x0)))));}">352</a>


 ┌────────────────────────┐
 │ Run                2/5 │
 │ Genration            0 │
 ├────────────────────────┤
 │ Best         352.00000 │
 │ Average      317.49700 │
 │ Worst        302.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> if' (myWasSuccess_ x0) (output_ (travel_ (rotCW_ dDown)) (zSet_ (i │
 │ f' ((<=) 2 (myEnergy_ x0)) 1 1) (yInc_ (yInc_ (yInc_ (myRegs_ x0)))))) (ou │
 │ tput_ (travel_ (nAppleDir_ x0)) (dSet_ dDown (xSet_ 1 (myRegs_ x0))))      │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return if_(myWasSuccess_(x0),output_(travel_(rotCW_(dDown)),zSet_(if_(lte(2,myEnergy_(x0)),1,1),yInc_(yInc_(yInc_(myRegs_(x0)))))),output_(travel_(nAppleDir_(x0)),dSet_(dDown,xSet_(1,myRegs_(x0)))));}">352</a>



 ┌────────────────────────┐
 │ Run                2/5 │
 │ Genration            1 │
 ├────────────────────────┤
 │ Best         489.00000 │
 │ Average      317.41400 │
 │ Worst        300.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> if' (myWasSuccess_ x0) (output_ (easySplit x0) (ySet_ (nAppleEnerg │
 │ y_ x0) (yInc_ (dSet_ dLeft (myRegs_ x0))))) (output_ (travel_ dLeft) (myRe │
 │ gs_ x0))                                                                   │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return if_(myWasSuccess_(x0),output_(easySplit(x0),ySet_(nAppleEnergy_(x0),yInc_(dSet_(dLeft,myRegs_(x0))))),output_(travel_(dLeft),myRegs_(x0)));}">489</a>


 ┌────────────────────────┐
 │ Run                2/5 │
 │ Genration            7 │
 ├────────────────────────┤
 │ Best         815.00000 │
 │ Average      320.77500 │
 │ Worst        300.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> if' (myWasSuccess_ x0) (output_ (split_ dUp (myEnergy_ x0) (myRegs │
 │ _ x0)) (myRegs_ x0)) (output_ (travel_ (if' ((<=) (yGet_ x0) 1) dLeft (rot │
 │ CW_ dDown))) (xSet_ 2 (xInc_ (myRegs_ x0))))                               │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return if_(myWasSuccess_(x0),output_(split_(dUp,myEnergy_(x0),myRegs_(x0)),myRegs_(x0)),output_(travel_(if_(lte(yGet_(x0),1),dLeft,rotCW_(dDown))),xSet_(2,xInc_(myRegs_(x0)))));}">815</a>


 ┌────────────────────────┐
 │ Run                2/5 │
 │ Genration           17 │
 ├────────────────────────┤
 │ Best         817.00000 │
 │ Average      503.15900 │
 │ Worst        300.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> if' (myWasSuccess_ x0) (output_ (split_ dUp (myEnergy_ x0) (myRegs │
 │ _ x0)) (myRegs_ x0)) (output_ (travel_ (dGet_ x0)) (dSet_ dLeft (dSet_ (dG │
 │ et_ x0) (zSet_ 2 (xInc_ (myRegs_ x0))))))                                  │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return if_(myWasSuccess_(x0),output_(split_(dUp,myEnergy_(x0),myRegs_(x0)),myRegs_(x0)),output_(travel_(dGet_(x0)),dSet_(dLeft,dSet_(dGet_(x0),zSet_(2,xInc_(myRegs_(x0)))))));}">817</a>



 ┌────────────────────────┐
 │ Run                2/5 │
 │ Genration           20 │
 ├────────────────────────┤
 │ Best        1336.00000 │
 │ Average      612.33000 │
 │ Worst        300.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> if' (myWasSuccess_ x0) (output_ (easySplit x0) (zInc_ (myRegs_ x0) │
 │ )) (output_ (travel_ (if' ((<=) (yGet_ x0) 1) dLeft (if' ((<=) (yGet_ x0)  │
 │ 1) dLeft (nAppleDir_ x0)))) (xSet_ 2 (xInc_ (xInc_ (yInc_ (zInc_ (xSet_ (y │
 │ Get_ x0) (xInc_ (dSet_ dLeft (myRegs_ x0))))))))))                         │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return if_(myWasSuccess_(x0),output_(easySplit(x0),zInc_(myRegs_(x0))),output_(travel_(if_(lte(yGet_(x0),1),dLeft,if_(lte(yGet_(x0),1),dLeft,nAppleDir_(x0)))),xSet_(2,xInc_(xInc_(yInc_(zInc_(xSet_(yGet_(x0),xInc_(dSet_(dLeft,myRegs_(x0)))))))))));}">1336</a>
-----------------------------------------------------------

BO3 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


gen 0  <a href="#" title="function (x0){return output_(travel_(cAppleDir_(x0)),myRegs_(x0));}">339</a>
gen 7  <a href="#" title="function (x0){return output_(if_(equals(1,zGet_(x0)),travel_(nAppleDir_(x0)),split_(rotCW_(dUp),2,xInc_(ySet_(2,dSet_(rotCW_(dRight),zInc_(myRegs_(x0))))))),ySet_(1,dSet_(rotCW_(dLeft),myRegs_(x0))));}">446</a>
gen 14 <a href="#" title="function (x0){return output_(if_(equals(1,zGet_(x0)),travel_(nAppleDir_(x0)),split_(nAppleDir_(x0),2,xInc_(ySet_(2,dSet_(dLeft,zInc_(myRegs_(x0))))))),ySet_(1,dSet_(rotCW_(if_(lte(2,2),dUp,rotCW_(dUp))),zSet_(xGet_(x0),myRegs_(x0)))));}">626</a>
gen 28 <a href="#" title="function (x0){return output_(if_(equals(1,zGet_(x0)),if_(equals(1,zGet_(x0)),travel_(nAppleDir_(x0)),split_(nAppleDir_(x0),2,xInc_(ySet_(2,zInc_(myRegs_(x0)))))),split_(nAppleDir_(x0),2,xInc_(ySet_(yGet_(x0),myRegs_(x0))))),zSet_(xGet_(x0),myRegs_(x0)));}">658</a>


 ┌────────────────────────┐
 │ Run                3/5 │
 │ Genration            0 │
 ├────────────────────────┤
 │ Best         339.00000 │
 │ Average      307.19400 │
 │ Worst        300.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (travel_ (if' ((<=) 2 0) (dGet_ x0) (cAppleDir_ x0))) (zSe │
 │ t_ 1 (myRegs_ x0))                                                         │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(travel_(cAppleDir_(x0)),myRegs_(x0));}">339</a>

 ┌────────────────────────┐
 │ Run                3/5 │
 │ Genration            7 │
 ├────────────────────────┤
 │ Best         446.00000 │
 │ Average      309.65700 │
 │ Worst        300.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (if' ((==) 1 (zGet_ x0)) (travel_ (nAppleDir_ x0)) (split_ │
 │  (rotCW_ dUp) 2 (xInc_ (ySet_ 2 (dSet_ (rotCW_ dRight) (zInc_ (myRegs_ x0) │
 │ )))))) (ySet_ 1 (dSet_ (rotCW_ dLeft) (myRegs_ x0)))                       │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(if_(equals(1,zGet_(x0)),travel_(nAppleDir_(x0)),split_(rotCW_(dUp),2,xInc_(ySet_(2,dSet_(rotCW_(dRight),zInc_(myRegs_(x0))))))),ySet_(1,dSet_(rotCW_(dLeft),myRegs_(x0))));}">446</a>

 ┌────────────────────────┐
 │ Run                3/5 │
 │ Genration           14 │
 ├────────────────────────┤
 │ Best         626.00000 │
 │ Average      313.17300 │
 │ Worst        300.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (if' ((==) 1 (zGet_ x0)) (travel_ (nAppleDir_ x0)) (split_ │
 │  (nAppleDir_ x0) 2 (xInc_ (ySet_ 2 (dSet_ dLeft (zInc_ (myRegs_ x0)))))))  │
 │ (ySet_ 1 (dSet_ (rotCW_ (if' ((<=) 2 2) dUp (rotCW_ dUp))) (zSet_ (xGet_ x │
 │ 0) (myRegs_ x0))))                                                         │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(if_(equals(1,zGet_(x0)),travel_(nAppleDir_(x0)),split_(nAppleDir_(x0),2,xInc_(ySet_(2,dSet_(dLeft,zInc_(myRegs_(x0))))))),ySet_(1,dSet_(rotCW_(if_(lte(2,2),dUp,rotCW_(dUp))),zSet_(xGet_(x0),myRegs_(x0)))));}">626</a>

 ┌────────────────────────┐
 │ Run                3/5 │
 │ Genration           28 │
 ├────────────────────────┤
 │ Best         658.00000 │
 │ Average      330.24600 │
 │ Worst        300.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (if' ((==) 1 (zGet_ x0)) (if' ((==) 1 (zGet_ x0)) (travel_ │
 │  (nAppleDir_ x0)) (split_ (nAppleDir_ x0) 2 (xInc_ (ySet_ 2 (zInc_ (myRegs │
 │ _ x0)))))) (split_ (nAppleDir_ x0) 2 (xInc_ (ySet_ (yGet_ x0) (myRegs_ x0) │
 │ )))) (zSet_ (xGet_ x0) (myRegs_ x0))                                       │
 └────────────────────────────────────────────────────────────────────────────┘

<a href="#" title="function (x0){return output_(if_(equals(1,zGet_(x0)),if_(equals(1,zGet_(x0)),travel_(nAppleDir_(x0)),split_(nAppleDir_(x0),2,xInc_(ySet_(2,zInc_(myRegs_(x0)))))),split_(nAppleDir_(x0),2,xInc_(ySet_(yGet_(x0),myRegs_(x0))))),zSet_(xGet_(x0),myRegs_(x0)));}">658</a>

----------------------------------------------------------------------------------

 ┌────────────────────────┐
 │ Run                4/5 │
 │ Genration            0 │
 ├────────────────────────┤
 │ Best         339.00000 │
 │ Average      307.68700 │
 │ Worst        300.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (travel_ (cAppleDir_ x0)) (zInc_ (ySet_ 2 (zSet_ 2 (zSet_  │
 │ (xGet_ x0) (yInc_ (myRegs_ x0))))))                                        │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(travel_(cAppleDir_(x0)),zInc_(ySet_(2,zSet_(2,zSet_(xGet_(x0),yInc_(myRegs_(x0)))))));}">339</a>


 ┌────────────────────────┐
 │ Run                4/5 │
 │ Genration           18 │
 ├────────────────────────┤
 │ Best         344.00000 │
 │ Average      316.03500 │
 │ Worst        300.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (travel_ (dGet_ x0)) (zInc_ (zSet_ 0 (zInc_ (zInc_ (xSet_  │
 │ 2 (dSet_ (cAppleDir_ x0) (xInc_ (myRegs_ x0))))))))                        │
 └────────────────────────────────────────────────────────────────────────────┘

<a href="#" title="function (x0){return output_(travel_(dGet_(x0)),zInc_(zSet_(0,zInc_(zInc_(xSet_(2,dSet_(cAppleDir_(x0),xInc_(myRegs_(x0)))))))));}">344</a>

--------------------------------------------------------------------------------------



 ┌────────────────────────┐
 │ Run                5/5 │
 │ Genration            0 │
 ├────────────────────────┤
 │ Best         339.00000 │
 │ Average      308.80900 │
 │ Worst        300.00000 │
 └────────────────────────┘
 ┌────────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (travel_ (cAppleDir_ x0)) (xSet_ 2 (dSet_ dLeft (ySet_ (my │
 │ Energy_ x0) (xInc_ (zInc_ (myRegs_ x0))))))                                │
 └────────────────────────────────────────────────────────────────────────────┘
<a href="#" title="function (x0){return output_(travel_(cAppleDir_(x0)),xSet_(2,dSet_(dLeft,ySet_(myEnergy_(x0),xInc_(zInc_(myRegs_(x0)))))));}">339</a>

 ┌────────────────────────┐
 │ Run                5/5 │
 │ Genration            5 │
 ├────────────────────────┤
 │ Best         344.00000 │
 │ Average      311.66000 │
 │ Worst        300.00000 │
 └────────────────────────┘
 ┌───────────────────────────────────────────────────────────────────────────┐
 │ \ x0 -> output_ (travel_ (dGet_ x0)) (dSet_ (cAppleDir_ x0) (myRegs_ x0)) │
 └───────────────────────────────────────────────────────────────────────────┘

<a href="#" title="function (x0){return output_(travel_(dGet_(x0)),dSet_(cAppleDir_(x0),myRegs_(x0)));}">344</a>


 1534.518









































--}