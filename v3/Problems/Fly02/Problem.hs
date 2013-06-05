module Problems.Fly02.Problem where

import Problems.Fly02.Funs  --(  )
import Problems.Fly02.Fly2  --(  )

--import Heval


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

