module Problems.Fly.Fly2 where


import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import System.Random


import Problems.Fly.Funs 


data Object 
 = Fly   FlyData
 | Apple Energy
 | Wall  Energy
 | Free

data Dir 
  = DUp   
  | DDown 
  | DLeft 
  | DRight
  | DStay
 deriving ( Show )

data Move 
 = Travel Dir 
 | Split  Dir Energy 
 | Build  Dir Energy


type RelPos   = Pos -- relative pos
type ObjInfo  = (RelPos,Energy)  
type Prog     = Input -> Output
type IntState = Int -- Nebo rači Double ????
type Success   = Bool


data World = World { 
  mapa       :: Map Pos Object ,
  fliesToDo  :: [Pos]          ,
  doneFlies  :: [Pos]          ,
  applePoses :: [Pos]
 }


data FlyData = FlyData { 
  flyProg       :: Prog     ,
  flyProgName   :: String   ,

  flyEnergy     :: Energy   ,
  flyState      :: IntState ,
  flyLastMove   :: Move     ,
  flyWasSuccess :: Success
 }



data Input  = Input {
  myEnergy     :: Energy      ,
  myState      :: IntState    ,
  myLastMove   :: Move        ,   -- nebo radši lastTravel ???
  myWasSuccess :: Success     ,

  --myPos        :: Pos         , -- asi nadbytečná, když pracuju v relativních pozicích.. ? (vždy je 0,0..) 
  
  myApples     :: [ ObjInfo ] ,
  myFlies      :: [ ObjInfo ] 
  
 } 

data Output = Output {
  myMove       :: Dir      ,
  myNextState  :: IntState
} 





prepareInput :: World -> Pos -> FlyData -> Input
prepareInput w flyPos flyData = 
  Input { myState      = flyState     flyData ,
          myEnergy     = flyEnergy    flyData ,
          myLastMove   = flyLastMove  flyData ,
          myWasSuccess = flyWasSuccess flyData ,

          myApples     = getObjInfos w flyPos (applePoses w) ,
          myFlies      = getObjInfos w flyPos (( fliesToDo w ++ doneFlies w ) \\  [flyPos] )  
          
        } 




reactToOutput :: World -> Pos -> FlyData -> Output -> World
reactToOutput w flyPos flyData output = undefined





performMove = undefined


-- tryToTravelFly :: Dir -> Pos -> World -> (World,Pos,Success)
-- tryToTravelFly dir pos w = 
--  let pos' = posPlusDir pos dir 
--   in case objOnPos w pos' of
--       Free -> ( moveFromTo pos pos' w , pos' , True )
--       Apple-> ( eatApple   pos pos' w , pos' , True )
--       _    -> ( w                     , pos  , False ) -- TODO tady se řeší žraní much respektive zdí














objOnPos :: World -> Pos -> Object
objOnPos w pos = case Map.lookup pos (mapa w) of
  Nothing -> Free
  Just o  -> o


objEnergy :: Object -> Energy
objEnergy o = case o of
 Fly   flyData -> flyEnergy flyData
 Apple energy  -> energy 
 Wall  energy  -> energy
 Free          -> 0

getObjInfos :: World -> Pos -> [Pos] -> [ObjInfo]
getObjInfos w flyPos poses = 
  let relPoses = map ((flip minus) flyPos) poses
      energies = map ( objEnergy . objOnPos w ) poses
   in zip relPoses energies
















