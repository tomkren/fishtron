-- aktualni verze v Problems/Fly/Problem

module Fly where


import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import System.Random

data World = World { 
  mapa       :: Map Pos Object ,
  fliesToDo  :: [Pos] ,
  doneFlies  :: [Pos] ,
  applePoses :: [Pos]
 }




data Object = Fly FlyData
            | Apple
            | Wall
            | Free


data FlyData = FlyData { 
  flyProg   :: Prog ,
  flyEnergy :: Energy
 }

type Prog   = Input -> Output
type Energy = Int


data Input  = Input {
  nearestAppleDir :: Dir ,
  inputEnergy     :: Energy 
 }

data Output = Output {
  moveDir :: Dir
} 


type Input_ = ( Dir_ , Energy )

type Dir_ = Int

dStay   = 0 
dUp     = 1
dDown   = 2
dLeft   = 3 
dRight  = 4 

type Output_ = Dir_

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
input2input_ input = ( dir2dir_ $ nearestAppleDir input , 
                                  inputEnergy     input )

output_2output :: Output_ -> Output
output_2output dir_ = Output{ moveDir = dir_2dir dir_ } 

prog_2prog :: Prog_ -> Prog
prog_2prog prog_ = output_2output . prog_ . input2input_


type Pos = (Int,Int)

data Dir = DUp   
         | DDown 
         | DLeft 
         | DRight
         | DStay
  deriving ( Show )

steps :: Int -> World -> World
steps numSteps w = case numSteps of
 0 -> w
 n -> let w' = step w in steps (n-1) w' 

stepsList :: Int -> World -> [World]
stepsList numSteps w = case numSteps of
  0 -> [w]
  n -> let w' = step w in w : stepsList (n-1) w'

step :: World -> World
step w = case fliesToDo w of
  [] -> w{ fliesToDo = reverse (doneFlies w) ,
           doneFlies = [] }
  _  -> step $ stepCurrentFly w

stepCurrentFly :: World -> World
stepCurrentFly w = case (fliesToDo w) of
 flyPos:restFlies -> 
  case objOnPos w flyPos of
   Fly flyData -> 
     let input        = prepareInput w flyPos flyData
         output       = (flyProg flyData) input
         (w',flyPos') = tryToMoveFly (moveDir output) flyPos w
      in w'{ fliesToDo = restFlies ,
             doneFlies = flyPos' : (doneFlies w') }   
   _ -> error "There shoud be a fly!"
 [] -> error "No current fly!"
 
prepareInput :: World -> Pos -> FlyData -> Input
prepareInput w flyPos flyData = 
  Input { nearestAppleDir = getNearestAppleDir w flyPos ,
          inputEnergy     = flyEnergy flyData } 

getNearestAppleDir :: World -> Pos -> Dir
getNearestAppleDir w pos = case sort $ map (\pos'->(dist pos pos',pos')) $ applePoses w of
 []         -> DStay
 (_,pos'):_ -> dirToPos pos pos' 


tryToMoveFly :: Dir -> Pos -> World -> (World,Pos)
tryToMoveFly dir pos w = 
 let pos' = posPlusDir pos dir 
  in case objOnPos w pos' of
      Free -> ( moveFromTo pos pos' w , pos' )
      Apple-> ( eatApple   pos pos' w , pos' )
      _    -> ( w                     , pos  )


eatApple :: Pos -> Pos -> World -> World
eatApple flyPos applePos w0 = 
  let w1 = moveFromTo flyPos applePos w0
      w2 = updateEnergy (+1) applePos w1
   in w2 { applePoses = delete applePos (applePoses w2) }


updateEnergy :: (Energy->Energy) -> Pos -> World -> World
updateEnergy updateFun flyPos w = updateObj f flyPos w
 where f (Fly flyData) = Fly flyData{ flyEnergy = updateFun (flyEnergy flyData) } 
       f _             = error "Only fly has energy to update!"  

updateObj :: (Object->Object) -> Pos -> World -> World
updateObj updateFun pos w = 
  w { mapa = Map.update (Just . updateFun) pos (mapa w) }

moveFromTo :: Pos -> Pos -> World -> World
moveFromTo pos1 pos2 w = 
  let obj = objOnPos w pos1
   in putObjOnPos obj pos2 (deleteOnPos pos1 w)

posPlusDir :: Pos -> Dir -> Pos
posPlusDir (x,y) dir = case dir of
  DUp    -> (x  ,y-1)
  DDown  -> (x  ,y+1)
  DLeft  -> (x-1,y  )
  DRight -> (x+1,y  )
  DStay  -> (x  ,y  )

dirToPos :: Pos -> Pos -> Dir
dirToPos dirMy dirHer 
  |   dx  > dy && (-dx) > dy = DUp
  |   dx  > dy               = DRight
  | (-dx) > dy               = DLeft
  | otherwise                = DDown
 where (dx,dy) = dirHer `minus` dirMy

minus :: Pos -> Pos -> Pos
minus (x1,y1) (x2,y2) = (x1-x2,y1-y2) 

dist :: Pos -> Pos -> Double
dist (x1,y1) (x2,y2) = sqrt $ (d2 x1 x2) + (d2 y1 y2)
 where d2 a b = let c=a-b in fromIntegral $ c*c 


w2 = foldr (uncurry putFly) w1 
 [ ( (20,20) , prog1 ) ,
   ( (21,20) , prog2 ) ]

putFly :: Pos -> Prog -> World -> World
putFly pos prog w = 
  let flyData = FlyData { flyProg = prog , flyEnergy = 0 }
      w'      = putObjOnPos (Fly flyData) pos w
   in w'{ fliesToDo = pos:(fliesToDo w') }


putApple :: Pos -> World -> World
putApple pos w = (putObjOnPos Apple pos w){
   applePoses = pos:(applePoses w)
 }

prog1 input = Output (nearestAppleDir input)
prog2 _     = Output DRight

emptyWorld :: World
emptyWorld = World{
  mapa      = Map.empty ,
  fliesToDo = [] ,
  doneFlies = [] ,
  applePoses= [] 
 }

boxedWorld :: World
boxedWorld = foldr ($) emptyWorld 
 [ h (x,y) , 
   h (x,y+len-1) ,
   v (x,y+1) ,
   v (x+len-1,y+1)  ]
  where 
    (x,y) = (1,1)
    len = 38
    h = putHLine Wall len
    v = putVLine Wall (len-2)

w1 :: World  
w1 = foldr putApple boxedWorld applePoses
 where
  numApples = 20
  (xs,ys) = splitAt numApples $ take (2*numApples) $ randomRs (2,35) (mkStdGen 424242)
  applePoses = zip xs ys

objOnPos :: World -> Pos -> Object
objOnPos w pos = case Map.lookup pos (mapa w) of
  Nothing -> Free
  Just o  -> o

putObjOnPos :: Object -> Pos -> World -> World
putObjOnPos o pos w = w{ mapa = Map.insert pos o (mapa w) }

deleteOnPos :: Pos -> World -> World
deleteOnPos pos w = w{ mapa = Map.delete pos (mapa w) }

putHLine :: Object -> Int -> Pos -> World -> World
putHLine o len (x,y) w = foldr (putObjOnPos o) w points
  where points = [ (x',y) | x' <- [x..x+len-1] ]

putVLine :: Object -> Int -> Pos -> World -> World
putVLine o len (x,y) w = foldr (putObjOnPos o) w points
  where points = [ (x,y') | y' <- [y..y+len-1] ]


objChar :: Object -> Char
objChar o = case o of
 Fly _   -> 'F'
 Apple   -> 'A'
 Wall    -> 'W'
 Free    -> '.'  

instance Show World where
  show = showWorld defaultView

instance Show FlyData where
  show fd = "Energy : " ++ show (flyEnergy fd)

showWorld :: (Pos,Pos) -> World -> String
showWorld ((x1,y1),(x2,y2)) w = 
   "\n" ++ 
   fliesInfo w ++ "\n" ++
   mapka ++ "\n\n" ++ 
   "fliesToDo  : " ++ show (fliesToDo  w) ++ "\n" ++ 
   "doneFlies  : " ++ show (doneFlies  w) ++ "\n" ++
   "applePoses : " ++ show (applePoses w) ++ "\n" 
  where
    mapka  = intercalate "\n" $ narezej (2*(x2-x1+1)) $ concatMap (\p-> (objChar $ objOnPos w p) : " " ) points
    points = [ (x,y) | y <- [y1..y2] , x <- [x1..x2] ]


allFlyPoses :: World -> [Pos]
allFlyPoses w = (fliesToDo w) ++ reverse (doneFlies w) 

fliesInfo :: World -> String
fliesInfo w = concatMap f (allFlyPoses w)
  where f pos = case objOnPos w pos of 
                 Fly flyData -> show pos ++ " : \t" ++ show flyData ++ "\n"
                 _ -> error "There shoud be a fly on " ++ show pos ++ "."



narezej :: Int -> [a] -> [[a]]
narezej _ [] = []
narezej n xs = 
  let (ys,zs) = splitAt n xs 
   in ys : narezej n zs
 
defaultView :: (Pos,Pos)
defaultView = ((0,0),(39,39))


testShow = showWorld defaultView emptyWorld