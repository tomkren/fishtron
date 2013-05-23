module Problems.Fly02.Fly where


import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import System.Random


import Problems.Fly02.Funs 
--(Energy,Pos,minus,dist,head')

data Input  = Input {
  myApplePoses    :: [Pos] ,
  nearestFlyPos   :: Maybe Pos ,
  myPos           :: Pos ,
  inputEnergy     :: Energy 
 } 

data Output = Output {
  moveDir :: Dir
} 


data Dir = DUp   
         | DDown 
         | DLeft 
         | DRight
         | DStay
  deriving ( Show )


prog_1 input = output_ $ posToDir_ (myPos_ input) (head_ $ myApplePoses_ input)
prog_2 _     = output_ $ dRight 
prog_3 input = output_ $ posToDir_ (myPos_ input) (nearestFlyPos_ input)

prog1 input = Output $ posToDir (myPos input) (head_ $ myApplePoses input)
prog2 _     = Output $ DRight
prog3 input = Output $ posToDir (myPos input) (nearestFlyPos input)
prog4 input = Output $ posToDir (myPos input) (avg $ myApplePoses input)



prog1' = ( prog1 , "prog1" )
prog2' = ( prog2 , "prog2" )
prog3' = ( prog3 , "prog3" )
prog4' = ( prog4 , "prog4" )





energies :: World -> [Energy]
energies w = map f (allFlyPoses w)
  where f pos = case objOnPos w pos of 
                 Fly flyData -> (flyEnergy flyData)
                 _ -> error $ "There shoud be a fly on " ++ show pos ++ "."



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
  flyProg     :: Prog   ,
  flyProgName :: String ,
  flyEnergy   :: Energy 
 }

type Prog   = Input -> Output








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
   _ -> error "There should be a fly!"
 [] -> error "No current fly!"
 
prepareInput :: World -> Pos -> FlyData -> Input
prepareInput w flyPos flyData = 
  Input { myApplePoses    = getSortedPoses w flyPos (applePoses w) ,
          nearestFlyPos   = getNearestFlyPos   w flyPos ,
          inputEnergy     = flyEnergy flyData ,
          myPos           = flyPos } 



getSortedPoses :: World -> Pos -> [Pos] -> [Pos]
getSortedPoses w pos poses = 
  map snd . sort . map (\pos'->(dist pos pos',pos')) $ poses

getNearestPos :: World -> Pos -> [Pos] -> Maybe Pos
getNearestPos w pos poses = case poses of
  [] -> Nothing
  _  -> Just . snd . minimum $ map (\pos'->(dist pos pos',pos')) $ poses

getNearestApplePos :: World -> Pos -> Maybe Pos
getNearestApplePos w pos = getNearestPos w pos (applePoses w)

getNearestFlyPos   :: World -> Pos -> Maybe Pos
getNearestFlyPos w pos = 
  let flies = (( fliesToDo w ++ doneFlies w ) \\  [pos] )
   in getNearestPos w pos flies 


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

posToDir :: Pos -> Maybe Pos -> Dir
posToDir posMy Nothing = DStay 
posToDir posMy (Just posHer) 
  |   dx  > dy && (-dx) > dy = DUp
  |   dx  > dy               = DRight
  | (-dx) > dy               = DLeft
  | otherwise                = DDown
 where (dx,dy) = posHer `minus` posMy




w2 = foldr (uncurry putFly) w1 
 [ ( (10,10) , prog1' ) ,
   ( (30,30) , prog1' ) ]

putFly :: Pos -> (Prog , String) -> World -> World
putFly pos (prog,progName) w = 
  let flyData = FlyData { flyProg = prog , flyEnergy = 1 , flyProgName = progName }
      w'      = putObjOnPos (Fly flyData) pos w
   in w'{ fliesToDo = pos:(fliesToDo w') }


putApple :: Pos -> World -> World
putApple pos w = (putObjOnPos Apple pos w){
   applePoses = pos:(applePoses w)
 }



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
showWorld view@((x1,y1),(x2,y2)) w = 
   "\n" ++ 
   fliesInfo w ++ "\n" ++
   mapka ++ "\n\n" ++ 
   "fliesToDo  : " ++ show (fliesToDo  w) ++ "\n" ++ 
   "doneFlies  : " ++ show (doneFlies  w) ++ "\n" ++
   "applePoses : " ++ show (applePoses w) ++ "\n" 
  where
    mapka = intercalate "\n" $ map (concatMap (\o-> objChar o : " ")) $ worldToLists view w 



worldToLists :: (Pos,Pos) -> World -> [[Object]]
worldToLists ((x1,y1),(x2,y2)) w = 
  [ [ objOnPos w (x,y) | x <- [x1..x2] ] | y <- [y1..y2] ]


worldFromStrs :: [String] -> World
worldFromStrs strs = 
  foldr (\(pos,obj) w -> f obj pos w ) emptyWorld (worldFromStrs' strs)
 where
  f obj = case obj of
    Apple -> putApple 
    _     -> putObjOnPos obj

worldFromStrs' :: [String] -> [(Pos,Object)]
worldFromStrs' strs = 
  concatMap (\(str,y)-> map (\(ch,x)-> ((x,y),objectFromChar ch) ) (zip str [0..]) ) (zip strs [0..]) 

objectFromChar :: Char -> Object
objectFromChar ch = case ch of
  '.' -> Free
  'W' -> Wall
  'A' -> Apple



allFlyPoses :: World -> [Pos]
allFlyPoses w = reverse (doneFlies w) ++ (fliesToDo w)

fliesInfo :: World -> String
fliesInfo w = concatMap f (allFlyPoses w)
  where f pos = case objOnPos w pos of 
                 Fly flyData -> show pos ++ " : \t" ++ show flyData ++ "\n"
                 _ -> error $ "There shoud be a fly on " ++ show pos ++ "."





instance Show Object where
  show o = [ objChar o ]

narezej :: Int -> [a] -> [[a]]
narezej _ [] = []
narezej n xs = 
  let (ys,zs) = splitAt n xs 
   in ys : narezej n zs
 
defaultView :: (Pos,Pos)
defaultView = ((0,0),(39,39))


testShow = showWorld defaultView emptyWorld