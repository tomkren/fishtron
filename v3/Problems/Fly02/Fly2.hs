module Problems.Fly.Fly2 where


import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Ord (comparing)
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
--  | DStay
 deriving ( Show )

data Move 
 = Travel Dir 
 | Split  Dir Energy Registers
-- | Build  Dir Energy


type RelPos   = Pos -- relative pos
type ObjInfo  = (RelPos,Energy)  
type Prog     = Input -> Output
type Success  = Bool
type Dist     = Double


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
  flyLastTravel :: Dir      ,
  flyWasSuccess :: Success  ,

  flyRegs :: Registers

 }



data Input  = Input {
  myEnergy     :: Energy   ,
  myLastTravel :: Dir      ,
  myWasSuccess :: Success  ,

  nAppleDir    :: Dir  ,
  nAppleDist   :: Dist ,
  nAppleEnergy :: Energy ,
  nFlyDir      :: Dir  ,
  nFlyDist     :: Dist ,
  nFlyEnergy   :: Energy,
  
  cAppleDir    :: Dir,
  cAppleDist   :: Dist,

  myRegs :: Registers

 } 

data Output = Output {
  myMove       :: Move ,
  myNextRegs :: Registers
} 


steps :: World -> Int -> World
steps w numSteps = case numSteps of
 0 -> w
 n -> let w' = step w in steps w' (n-1) 

stepsList :: World -> Int -> [World]
stepsList w numSteps = case numSteps of
  0 -> [w]
  n -> let w' = step w in w : stepsList w' (n-1) 

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
     let input  = prepareInput w flyPos flyData
         output = (flyProg flyData) input
      in reactToOutput output flyPos w   
   _ -> error "stepCurrentFly : There should be a fly!"
 [] ->  error "stepCurrentFly : No current fly!"




prepareInput :: World -> Pos -> FlyData -> Input
prepareInput w flyPos flyData =

 let aPoses               = applePoses w
     (nADist,nADir,nAEn)  = nearestInfo w flyPos aPoses 
     (nFDist,nFDir,nFEn)  = nearestInfo w flyPos (( fliesToDo w ++ doneFlies w ) \\  [flyPos] )  
     
     (cADist , cADir )    = centerInfo w flyPos aPoses 

  in Input { myEnergy     = flyEnergy     flyData 
           , myLastTravel = flyLastTravel flyData 
           , myWasSuccess = flyWasSuccess flyData 

           , nAppleDir    = nADir  
           , nAppleDist   = nADist 
           , nAppleEnergy = nAEn   
           , nFlyDir      = nFDir  
           , nFlyDist     = nFDist 
           , nFlyEnergy   = nFEn   

           , cAppleDir    = cADir  
           , cAppleDist   = cADist 

           , myRegs = flyRegs flyData
           } 

nearestInfo :: World -> Pos -> [Pos] -> (Dist,Dir,Energy)
nearestInfo _ _      []    = (999999, DRight , 0)
nearestInfo w flyPos poses = 
  let nearestPos = minimumBy (comparing (dist flyPos)) poses
   in ( dist flyPos nearestPos , posToDir flyPos nearestPos , objEnergy $ objOnPos w nearestPos )


centerInfo :: World -> Pos -> [Pos] -> (Dist,Dir)
centerInfo _ _ [] = ( 999999 , DRight )
centerInfo w flyPos poses = 
  let energies         = map (energyOnPos w) poses
      posOfWeightedAvg = weightedAvg poses energies
   in ( dist flyPos posOfWeightedAvg , posToDir flyPos posOfWeightedAvg )
 
weightedAvg :: [Pos] -> [Energy] -> Pos
weightedAvg xs ens = 
  let (xs1,xs2) = unzip xs
      f         = round . weightedMean ens
   in ( f xs1 , f xs2 ) 
 where
  weightedMean :: [Int] -> [Int] -> Double
  weightedMean ws xs = 
    let weightedSum = sum $ map (uncurry (*)) (zip ws xs)
     in (fromIntegral $ weightedSum) / (fromIntegral $ sum ws)


posToDir :: Pos -> Pos -> Dir
posToDir posMy posHer = relPosToDir $ posHer `minus` posMy



reactToOutput :: Output -> Pos -> World -> World
reactToOutput herOutput herPos w0 = 
  let herMove      =  myMove      herOutput     
      herNextRegs  =  myNextRegs  herOutput

      ( w1 , posChangeInfo , success ) = performMove herPos herMove w0  
      
      f flyData = flyData{
        flyLastTravel = case herMove of Travel d -> d 
                                        _        -> flyLastTravel flyData ,
        flyWasSuccess = success  ,
        flyRegs = herNextRegs    } 

      w2 = case hasCurrFlySurvived posChangeInfo of
            Nothing      -> w1
            Just herPos' -> updateFlyData f herPos' w1

   in stepFliesQueue posChangeInfo w2

data PosChangeInfo
 = CurrNewPos Pos
 | CurrDead 
 | OtherDead Pos
 | SplitChange MotherPos ChildPos 

type MotherPos = Pos
type ChildPos  = Pos

stepFliesQueue :: PosChangeInfo -> World -> World
stepFliesQueue posChangeInfo w =
 case fliesToDo w of
  []     -> error "stepFliesQueue : there shoud be a fly in fliesToDo!"
  _:rest -> case posChangeInfo of      
             CurrNewPos newPos              -> w{ fliesToDo = rest ,
                                                  doneFlies = newPos : (doneFlies w) }
             CurrDead                       -> w{ fliesToDo = rest }
             OtherDead deadPos              -> w{ fliesToDo = delete deadPos rest ,
                                                  doneFlies = deadPos : ( delete deadPos (doneFlies w) ) }
             SplitChange motherPos childPos -> w{ fliesToDo = childPos : rest ,
                                                  doneFlies = motherPos : (doneFlies w) }

hasCurrFlySurvived :: PosChangeInfo -> Maybe Pos
hasCurrFlySurvived posChangeInfo = case posChangeInfo of
 CurrDead                -> Nothing
 OtherDead   pos         -> Just pos
 CurrNewPos  pos         -> Just pos 
 SplitChange motherPos _ -> Just motherPos



performMove :: Pos -> Move -> World -> (World,PosChangeInfo,Success)
performMove flyPos move w = case move of
 Travel dir              -> doTravel dir              flyPos w
 Split  dir en childRegs -> doSplit  dir en childRegs flyPos w
-- Build  dir en -> undefined


doTravel :: Dir -> Pos -> World -> (World,PosChangeInfo,Success)
--doTravel DStay pos w = ( w , CurrNewPos pos , False )
doTravel dir pos w = 
 let pos' = posPlusDir pos dir 
  in case objOnPos w pos' of
      Free         -> ( moveFromTo      pos pos' w , CurrNewPos pos' , True )
      Apple energy -> ( eatApple energy pos pos' w , CurrNewPos pos' , True )
      Fly   _      ->   flyCollision    pos pos' w 
      _            -> ( w                          , CurrNewPos pos  , False ) -- TODO tady se řeší i žraní zdí


doSplit :: Dir -> Energy -> Registers -> Pos -> World -> (World,PosChangeInfo,Success)
--doSplit DStay _ _ _ flyPos w = ( w , CurrNewPos flyPos , False )
doSplit dir childEnergy childRegs flyPos w =
  let childPos = posPlusDir flyPos dir
   in case objOnPos w childPos of
       Free -> let correctChildEnergy = min childEnergy ((energyOnPos w flyPos) -1)
                in if correctChildEnergy > 0 
                    then let childFly = mkChild (getFlyData w flyPos) correctChildEnergy childRegs dir
                             w' = putObjOnPos childFly childPos (updateEnergy (\e->e-correctChildEnergy) flyPos w) 
                          in ( w' , SplitChange flyPos childPos , True )
                    else ( w , CurrNewPos flyPos , False )
       _    -> ( w , CurrNewPos flyPos , False )


data Registers = Registers{ 
   xReg :: Int,
   yReg :: Int,
   zReg :: Int,
   dReg :: Dir
 }

instance Show Registers where
  show rs = " x=" ++ (show $ xReg rs) ++
            " y=" ++ (show $ yReg rs) ++
            " z=" ++ (show $ zReg rs) ++
            " d=" ++ (show $ dReg rs)

defaultRegs :: Registers
defaultRegs = Registers
 {xReg = 0
 ,yReg = 0
 ,zReg = 0
 ,dReg = DRight
 }

xGet,yGet,zGet :: Input -> Int
dGet           :: Input -> Dir
xGet = xReg . myRegs
yGet = yReg . myRegs
zGet = zReg . myRegs
dGet = dReg . myRegs

xSet,ySet,zSet :: Int -> Registers -> Registers
dSet           :: Dir -> Registers -> Registers
xSet i rs = rs{ xReg = i }
ySet i rs = rs{ yReg = i }
zSet i rs = rs{ zReg = i }
dSet i rs = rs{ dReg = i }

xInc,yInc,zInc :: Registers -> Registers
xInc rs = rs{ xReg = 1 + xReg rs }
yInc rs = rs{ yReg = 1 + yReg rs }
zInc rs = rs{ zReg = 1 + zReg rs }



mkChild :: FlyData -> Energy -> Registers -> Dir -> Object
mkChild motherData childEnergy childRegs bornDir = 
  Fly $ FlyData  
   { flyProg       = flyProg     motherData 
   , flyProgName   = flyProgName motherData 
   , flyEnergy     = childEnergy                        
   , flyLastTravel = bornDir                
   , flyWasSuccess = True  
   , flyRegs       = childRegs
   }




-- = Fly   FlyData
-- | Apple Energy
-- | Wall  Energy
-- | Free


moveFromTo :: Pos -> Pos -> World -> World
moveFromTo pos1 pos2 w = 
  let obj = objOnPos w pos1
   in putObjOnPos obj pos2 (deleteOnPos pos1 w)

eatApple :: Energy -> Pos -> Pos -> World -> World
eatApple energy flyPos applePos w0 = 
  let w1 = moveFromTo flyPos applePos w0
      w2 = updateEnergy (+energy) applePos w1
   in w2 { applePoses = delete applePos (applePoses w2) }


flyCollision :: Pos -> Pos -> World -> ( World , PosChangeInfo , Success )
flyCollision herPos oponentPos w = 
 let herFlyData = getFlyData w herPos
     opoFlyData = getFlyData w oponentPos
     herEnergy  = flyEnergy herFlyData
     opoEnergy  = flyEnergy opoFlyData

     sheWon = herEnergy >= opoEnergy 

     flyData' = (if sheWon then herFlyData else opoFlyData){ flyEnergy = herEnergy + opoEnergy }

     w' = putObjOnPos (Fly flyData') oponentPos (deleteOnPos herPos w)
     
  in ( w' , if sheWon then OtherDead oponentPos else CurrDead , sheWon )
       


posPlusDir :: Pos -> Dir -> Pos
posPlusDir (x,y) dir = case dir of
  DUp    -> (x  ,y-1)
  DDown  -> (x  ,y+1)
  DLeft  -> (x-1,y  )
  DRight -> (x+1,y  )
--  DStay  -> (x  ,y  )



getFlyData :: World -> Pos -> FlyData
getFlyData w pos = case objOnPos w pos of
  Fly flyData -> flyData
  _ -> error "getFlyData : ther emust be a fly on the pos.."

updateFlyData :: (FlyData->FlyData) -> Pos -> World -> World
updateFlyData updateFun flyPos w = updateObj f flyPos w
 where f (Fly flyData) = Fly (updateFun flyData) 
       f _             = error "Only fly has FlyData to update!"  

updateEnergy :: (Energy->Energy) -> Pos -> World -> World
updateEnergy updateFun flyPos w = updateObj f flyPos w
 where f (Fly flyData) = Fly flyData{ flyEnergy = updateFun (flyEnergy flyData) } 
       f _             = error "Only fly has energy to update!"  

updateObj :: (Object->Object) -> Pos -> World -> World
updateObj updateFun pos w = 
  w { mapa = Map.update (Just . updateFun) pos (mapa w) }

putObjOnPos :: Object -> Pos -> World -> World
putObjOnPos o pos w = w{ mapa = Map.insert pos o (mapa w) }

deleteOnPos :: Pos -> World -> World
deleteOnPos pos w = w{ mapa = Map.delete pos (mapa w) }




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

energyOnPos :: World -> Pos -> Energy
energyOnPos w pos = objEnergy $ objOnPos w pos

getObjInfos :: World -> Pos -> [Pos] -> [ObjInfo]
getObjInfos w flyPos poses = 
  let relPoses = map ((flip minus) flyPos) poses
      energies = map ( objEnergy . objOnPos w ) poses
   in zip relPoses energies





-------------------------------------------------------------------------------------


instance Show World where
  show = showWorld defaultView

instance Show FlyData where
  show fd = (flyProgName fd) ++ 
            " en=" ++ show (flyEnergy     fd) ++ 
            " su=" ++ show (flyWasSuccess fd) ++
            " "    ++ show (flyRegs       fd)

showWorld :: (Pos,Pos) -> World -> String
showWorld view@((x1,y1),(x2,y2)) w = 
   "\n" ++ 
   fliesInfo w ++ "\n" ++
   mapka ++ "\n\n" ++ 
   "fliesToDo  : " ++ show (fliesToDo  w) ++ "\n" ++ 
   "doneFlies  : " ++ show (doneFlies  w) ++ "\n" ++
   "applePoses : " ++ show (applePoses w) ++ "\n" 
  where
    mapka = intercalate "\n" . 
            map (intercalate " " . map (\o-> [objChar o])) $ 
            worldToLists view w 

defaultView :: (Pos,Pos)
defaultView = ((0,0),(39,39))

fliesInfo :: World -> String
fliesInfo w = concatMap f (allFlyPoses w)
  where f pos = case objOnPos w pos of 
                 Fly flyData -> show pos ++ " : \t" ++ show flyData ++ "\n"
                 _ -> error $ "There shoud be a fly on " ++ show pos ++ "."

objChar :: Object -> Char
objChar o = case o of
 Fly _   -> 'F'
 Apple _ -> 'A'
 Wall  _ -> 'W'
 Free    -> '.'  

worldToLists :: (Pos,Pos) -> World -> [[Object]]
worldToLists ((x1,y1),(x2,y2)) w = 
  [ [ objOnPos w (x,y) | x <- [x1..x2] ] | y <- [y1..y2] ]

allFlyPoses :: World -> [Pos]
allFlyPoses w = reverse (doneFlies w) ++ (fliesToDo w)







----------------------------------------------------------------

-- type ObjInfo  = (RelPos,Energy)  



--asi chytřejší takle:   
nearest :: [ObjInfo] -> x -> ( Dist -> Dir -> x ) -> x
nearest infos defaultForEmptyInfos f =
  case infos of
   [] -> defaultForEmptyInfos
   _  -> let (rp,_) = minimumBy ( comparing (abso . fst) ) infos
          in f (sqrt . fromIntegral . abso $ rp) (relPosToDir rp)

nearestDir :: [ObjInfo] -> Dir
--nearestDir [] = DStay
nearestDir xs = relPosToDir . fst $ minimumBy (comparing (abso . fst)) xs

relPosToDir :: RelPos -> Dir
relPosToDir (dx,dy) 
  |   dx  > dy && (-dx) > dy = DUp
  |   dx  > dy               = DRight
  | (-dx) > dy               = DLeft
  | otherwise                = DDown


abso :: RelPos -> Int
abso (x,y) = x*x + y*y



rot180 :: Dir -> Dir
rot180 DUp     = DDown
rot180 DRight  = DLeft
rot180 DDown   = DUp
rot180 DLeft   = DRight
--rot180 DStay   = DStay



--oldOutput :: Move -> IntState -> Output
--oldOutput move state = Output move state defaultRegs



-- furt doprava
prog1 = ( "prog1" , \ x -> Output (Travel DRight) (myRegs x) )

-- za nejbližším jablkem
-- prog2 = ( "prog2" , \ x -> Output (Travel (nearestDir (myApples x)) ) 0 )
prog2 = ( "prog2" , \ x -> Output (Travel (nAppleDir x) ) (myRegs x) )


prog3 = ( "prog3" , prog )
 where
  prog x = Output ( if (nAppleDist x) < (nFlyDist x) 
                    then Travel (nAppleDir x)
                    else ( if (nFlyEnergy x) < (myEnergy x) 
                           then Travel (nFlyDir x) 
                           else Travel (rot180 (nFlyDir x) )  )  ) (myRegs x)

prog4 = ( "prog4" , prog )
 where
  prog i = if (xGet i) > 5 
           then Output ( Split DDown (myEnergy i `div` 2) defaultRegs ) ( xSet 0 $ myRegs i) 
           else if (yGet i) > 5
                then Output ( Travel (nAppleDir i) ) ( xInc        $ myRegs i)
                else Output ( Travel DRight        ) ( xInc . yInc $ myRegs i) 

w1 = foldr (uncurry putFly) wNoFlies 
 [ ( (10,10) , prog4 ) 
 --, ( (30,30) , prog3 ) 
 ]


-- TODO
-- predikát pro isMyChild : dir -> bool
--              isFly     : ----||---- 
--              isApple   : ----||----
--              isFree    : ----||----








wNoFlies :: World  
wNoFlies = foldr putApple boxedWorld applePoses
 where
  numApples = 20
  (xs,ys) = splitAt numApples $ take (2*numApples) $ randomRs (2,35) (mkStdGen 424242)
  applePoses = zip xs ys

boxedWorld :: World
boxedWorld = foldr ($) emptyWorld 
 [ h (x,y) , 
   h (x,y+len-1) ,
   v (x,y+1) ,
   v (x+len-1,y+1)  ]
  where 
    (x,y) = (1,1)
    len = 38
    wallEnergy = 9999
    h = putHLine (Wall wallEnergy) len
    v = putVLine (Wall wallEnergy) (len-2)

emptyWorld :: World
emptyWorld = World{
  mapa      = Map.empty ,
  fliesToDo = [] ,
  doneFlies = [] ,
  applePoses= [] 
 }


 

putFly :: Pos -> (String,Prog) -> World -> World
putFly pos (progName,prog) w = 
  let flyData = FlyData { 
                  flyProg       = prog , 
                  flyProgName   = progName ,
                  flyEnergy     = 1      , 
                  flyLastTravel = DRight ,
                  flyWasSuccess = True ,
                  
                  flyRegs = defaultRegs
                }
      w' = putObjOnPos (Fly flyData) pos w
   in w'{ fliesToDo = pos:(fliesToDo w') }

putApple :: Pos -> World -> World
putApple pos w = (putObjOnPos (Apple 1) pos w){
   applePoses = pos:(applePoses w)
 }

putHLine :: Object -> Int -> Pos -> World -> World
putHLine o len (x,y) w = foldr (putObjOnPos o) w points
  where points = [ (x',y) | x' <- [x..x+len-1] ]

putVLine :: Object -> Int -> Pos -> World -> World
putVLine o len (x,y) w = foldr (putObjOnPos o) w points
  where points = [ (x,y') | y' <- [y..y+len-1] ]
