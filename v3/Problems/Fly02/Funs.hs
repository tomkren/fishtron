module Problems.Fly02.Funs where

s :: (a->b->c) -> (a->b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x y = x

i :: a -> a
i x = x


type Energy  = Int
type Pos     = (Int,Int)
type Success = Bool
type Dist    = Double

--type Input_  = ( [Pos] , Maybe Pos , Pos , Energy )

type Dir_    = Int

type Move_ = Either Dir_ (Dir_ ,Energy, Registers_)


type Registers_ = (Int,Int, Int, Dir_ )
 
type Input_  = ( (Energy,Dir_,Success) , (Dir_,Dist,Energy,  Dir_,Dist,Energy)  ,  (Dir_,Dist)  ,  Registers_ )

type Output_ = ( Move_ , Registers_ )



output_ :: Move_ -> Registers_ -> Output_
output_ move regs = (move,regs) 

if' :: Bool -> a -> a -> a
if' p q r = if p then q else r 


travel_ :: Dir_ -> Move_
travel_ dir = Left dir

split_ :: Dir_ -> Energy -> Registers_ -> Move_
split_ dir en regs = Right (dir,en,regs)

dUp , dDown , dLeft , dRight :: Dir_
dUp     = 1
dDown   = 2
dLeft   = 3 
dRight  = 4 



nAppleDir_ :: Input_ -> Dir_
nAppleDir_ ( _ , (x,_,_,_,_,_) , _ , _ ) = x
nAppleDist_ :: Input_ -> Dist_
nAppleDist_ ( _ , (_,x,_,_,_,_) , _ , _ ) = x
nAppleEnergy_ :: Input_ -> Energy
nAppleEnergy_ ( _ , (_,_,x,_,_,_) , _ , _ ) = x
nFlyDir_ :: Input_ -> Dir_  
nFlyDir_ ( _ , (_,_,_,x,_,_) , _ , _ ) = x
nFlyDist_ :: Input_ -> Dist 
nFlyDist_ ( _ , (_,_,_,_,x,_) , _ , _ ) = x
nFlyEnergy_ :: Input_ -> Energy
nFlyEnergy_ ( _ , (_,_,_,_,_,x) , _ , _ ) = x

cAppleDir_ :: Input_ -> Dir_
cAppleDir_ ( _ , _ , (x,_) , _ ) = x
cAppleDist_ :: Input_ -> Dist
cAppleDist_ ( _ , _ , (_,x) , _ ) = x



myRegs_ :: Input_ -> Registers_
myRegs_ ( _ , _ , _ , regs ) = regs

xGet_,yGet_,zGet_ :: Input_ -> Int
dGet_             :: Input_ -> Dir_
xGet_ = (\(x,_,_,_)->x) . myRegs_ 
yGet_ = (\(_,y,_,_)->y) . myRegs_ 
zGet_ = (\(_,_,z,_)->z) . myRegs_ 
dGet_ = (\(_,_,_,d)->d) . myRegs_ 

xSet,ySet,zSet :: Int  -> Registers_ -> Registers_
dSet           :: Dir_ -> Registers_ -> Registers_
xSet i (x,y,z,d) = (i,y,z,d)
ySet i (x,y,z,d) = (x,i,z,d)
zSet i (x,y,z,d) = (x,y,i,d)
dSet i (x,y,z,d) = (x,y,z,i)

xInc,yInc,zInc :: Registers -> Registers
xInc (x,y,z,d) = (x+1,y,z,d)
yInc (x,y,z,d) = (x,y+1,z,d)
zInc (x,y,z,d) = (x,y,z+1,d)



