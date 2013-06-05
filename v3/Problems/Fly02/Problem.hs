module Problems.Fly02.Problem where

import Problems.Fly02.Funs  --(  )
import Problems.Fly02.Fly2  --(  )

--import Heval

type Prog_ = Input_ -> Output_

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

