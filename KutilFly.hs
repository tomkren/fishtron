module KutilFly where

import Data.Maybe
import Data.List
import qualified Data.Map as Map
type Map k a = Map.Map k a


{-- TODO : kouká se na to jako na frontu ale operace sou neefektivní : (++) pro zařazení na konec 
           zajistit, aby nešlo že jsou dvě mouchy v jedný pozici najednou 
           pořádně ošetřit FreeSlot
--}

type BoxId = Int
type SlotPort = Int
type NumSlots = Int
type NumFreeSlots = Int
type BoxFun = [Slot] -> Maybe [Slot]
type Pos = (Int,Int)
type ObjId = Int
type FunName = String

data Prog = Prog (Map BoxId Box) [BoxId] 
data Box = Box Slots [(BoxId,SlotPort)] BoxFun FunName
data Slots = Slots (Map SlotPort Slot) NumSlots NumFreeSlots deriving (Show)
data Slot = FreeSlot | IntSlot Int | BoolSlot Bool | DirSlot Dir | ListSlot [Slot]
data Dir = DUp | DDown | DLeft | DRight | DRandom deriving (Show)


type ProgSchema = [BoxSchema]
type FunSchema  = (FunName,BoxFun,NumSlots)
data BoxSchema  = BS BoxId FunSchema [(SlotPort,Slot)] [(BoxId,SlotPort)] 


type SlotAddr = (BoxId,SlotPort)

data SensorType   = SApple | SGoal | STouch deriving (Eq , Ord)
data EffectorType = EGoal deriving (Eq , Ord)

data Agent = Agent Prog Sensors Effectors Goal (LastMove,WasTold)
type Sensors   = Map SensorType [SlotAddr]
type Effectors = [(EffectorType,SlotAddr)]
type Goal      = Pos
type LastMove  = Dir
type WasTold   = Bool 

type SensorsSchema = [(SensorType,SlotAddr)]


data World = World (Map Pos [Obj]) [[Prog]]    
data Obj = OFly Agent | OWall

type WorldSchema = [ObjSchema]
data ObjSchema   = Fly Pos Goal SensorsSchema Effectors ProgSchema 
                 | Wall Pos
                 | LongWall Pos Pos


Just world1 = fromWordlSchema [
  Wall (-7,-3) ,
  LongWall (-18,-18) (-18,18) ,
  LongWall (-18,-18) (18,-18) ,
  LongWall (-18,18) (18,18) ,
  LongWall (18,-18) (18,18) ,
  fly1,fly2
 ]

fly1 =
 Fly (1,1) (4,3) 
  [ (SGoal, (1,0) ) , (STouch, (3,0) ) ]
  [ (EGoal, (2,0) ) , (EGoal ,  (4,0) ) ]
  [ 
    BS 1 ide     [] [(2,0)], 
    BS 2 ide     [] [],
    BS 3 rot180  [] [(4,0)],
    BS 4 ide     [] [] 
  ]

fly2 = 
 Fly (10,1) (4,8) 
  [ (SGoal,(1,0)) , (STouch,(5,1))]
  [ (EGoal,(3,0)) , (EGoal,(4,0))  ]
  [ 
    BS 1 ide  [] [(2,0)],
    BS 2 copy [] [(3,0),(4,0)],
    BS 3 ide  [] [],
    BS 4 ide  [] []
  ]

steps :: World -> Int -> Int -> [World]
steps w n n' = steps'' (steps' w n) n'  

steps' :: World -> Int -> World
steps' w 0 = w
steps' w n = steps' (stepWorld w) (n-1)

steps'' :: World ->  Int -> [World]
steps'' w 0 = []
steps'' w n = w : steps'' (stepWorld w) (n-1)

stepWorld :: World -> World
stepWorld (World objMap _ ) 
  = World objMap' progss'
 where
 (flies,otherObjs) = separateFlies objMap
 flies2 = map ( (doSTouch objMap) . doSGoal ) flies
 (flies3 , progss') = unzip $ map fullStepAgent' flies2
 flies4 = map (agentNextPos objMap) flies3
 objMap' = foldr (uncurry insertToListMap) Map.empty $ map (\(pos,fly)->(pos,OFly fly)) flies4 ++ otherObjs

liftPos :: (Agent -> Agent) -> ((Pos,Agent) -> (Pos,Agent))
liftPos f (pos,agent) = (pos, f agent) 

moveGoal :: Agent -> [Dir] -> Agent
moveGoal (Agent p s e goal (lastMove,_) ) dirs
 = Agent p s e (foldr move goal dirs) (if null dirs then lastMove else head dirs , False  )

move :: Dir -> Pos -> Pos
move dir (x,y) = case dir of
  DUp     -> (x  ,y-1)
  DDown   -> (x  ,y+1)
  DLeft   -> (x-1,y  )
  DRight  -> (x+1,y  )
  DRandom -> error "DRandom zatim neimplementovan"

fullStepAgent' :: (Pos,Agent) -> ( (Pos,Agent) , [Prog] )
fullStepAgent' (pos,ag) = let (ag',pgs) = fullStepAgent ag in ( (pos,ag') , pgs )
fullStepAgent :: Agent -> ( Agent , [Prog] )
fullStepAgent a@(Agent prog s e g l)
 = if isQueueEmpty prog 
    then ( a , [prog] )
    else let (a',p)   = stepAgent a 
             (a'',ps) = fullStepAgent a'
          in (a'',p++ps)  

stepAgent :: Agent -> ( Agent , [Prog] )
stepAgent ( Agent prog s e g l)
 = let prog' = stepProg prog
    in (doEGoal $ Agent prog' s e g l , [prog,prog'] ) -- because doEGoal may also changes prog by cuting the effectors

doEGoal' = liftPos doEGoal
doEGoal :: Agent -> Agent
doEGoal a@(Agent prog s effs g l) 
  = moveGoal (Agent prog' s effs g l) dirs 
 where
 addrs        = map snd $ filter (\(efType,_)-> efType == EGoal ) effs
 (vals,prog') = foldr (\ad (xs,pr) -> let (x,pr') = cutSlot ad pr in (x:xs,pr') ) ([],prog) addrs
 dirs         = map (\(DirSlot d)->d) $ filter (\v->case v of DirSlot _ -> True ; _ -> False ) $ catMaybes vals
 




doSTouch :: Map Pos [Obj] -> (Pos,Agent) -> (Pos,Agent)
doSTouch objMap ( pos , a@(Agent prog sensors e g l ) )
  = ( pos , case touchDir objMap pos of 
             []      -> a
             (dir:_) ->  Agent (prog' dir) sensors e g l {-- error (show dir)--} )
 where 
  prog' dir = let addrs = lookupInListMap STouch sensors
               in foldr (insertToSensor (DirSlot dir)) prog addrs 

touchDir :: Map Pos [Obj] -> Pos -> [Dir]
touchDir objMap pos 
 = map snd $ filter (\(pos',_)-> not . null $ lookupInListMap pos' objMap) $ neighbors pos

neighbors :: Pos -> [(Pos,Dir)]
neighbors p@(x,y) = [ ((x,y-1),DUp) , ((x,y+1),DDown) , ((x-1,y),DLeft) , ((x+1,y),DRight) ]

doSGoal :: (Pos,Agent) -> (Pos,Agent)
doSGoal ( pos , a@(Agent prog sensors effs goal (lastMove,wasTold) ) ) 
  = ( pos , if not wasTold && pos == goal then a' else a )
 where 
  addrs = lookupInListMap SGoal sensors
  prog' = foldr (insertToSensor (DirSlot lastMove)) prog addrs
  a' = Agent prog' sensors effs goal (lastMove,True)

agentNextPos :: Map Pos [Obj] -> (Pos,Agent) -> (Pos,Agent)
agentNextPos objMap ( pos , a@(Agent _ _ _ goal _) ) 
  = ( if null (lookupInListMap pos' objMap) then pos' else pos , a )  
 where pos' = nextPos pos goal

getFlies :: Map Pos [Obj] -> [(Pos,Agent)]
getFlies objMap = foldr (\(pos,os) acc -> foldr (f pos) [] os ++ acc ) [] $ Map.toList objMap
 where 
 f pos = \ o acc -> case o of 
  OFly agent -> (pos,agent):acc  
  _          -> acc

getFlies2 = fst . separateFlies
 
separateFlies :: Map Pos [Obj] -> ( [(Pos,Agent)] , [(Pos,Obj)] )
separateFlies objMap = foldr ff ([],[]) $ Map.toList objMap
 where 
 ff :: (Pos,[Obj]) ->  ( [(Pos,Agent)] , [(Pos,Obj)] ) -> ( [(Pos,Agent)] , [(Pos,Obj)] )
 ff (pos,os) (acc1,acc2) = let (ags,objs) = foldr (f pos) ([],[]) os in ( ags ++ acc1 , objs ++ acc2 ) 
 f :: Pos -> Obj -> ( [(Pos,Agent)] , [(Pos,Obj)] ) -> ( [(Pos,Agent)] , [(Pos,Obj)] )
 f pos o (acc1,acc2) = case o of 
  OFly agent -> ( (pos,agent):acc1 ,           acc2 )   
  obj        -> (             acc1 , (pos,obj):acc2 )
 
insertToListMap :: (Ord k) => k -> a -> Map k [a] -> Map k [a]
insertToListMap key val listMap 
 = Map.insertWith (\[x] xs -> x:xs) key [val] listMap 

lookupInListMap :: (Ord k) => k -> Map k [a] -> [a]
lookupInListMap key listMap 
 = case Map.lookup key listMap of
  Nothing -> []
  Just xs -> xs

fromWordlSchema :: WorldSchema -> Maybe World
fromWordlSchema objSchemas = do 
  objMap <- foldr f (Just Map.empty) objSchemas
  return $ World objMap []
 where
 f :: ObjSchema -> Maybe (Map Pos [Obj]) -> Maybe (Map Pos [Obj])
 f objSchema acc = do
  objMap <- acc
  xs <- fromObjSchema objSchema 
  return $ foldr (\ (obj,pos) acc -> insertToListMap pos obj acc ) objMap xs

fromSensorsSchema :: [(SensorType,SlotAddr)] -> Map SensorType [SlotAddr]
fromSensorsSchema 
 = foldr (\(senType,slotAddr) senMap -> insertToListMap senType slotAddr senMap ) Map.empty

fromObjSchema :: ObjSchema -> Maybe [(Obj , Pos)]
fromObjSchema x = case x of
  Fly pos goal senSch effs progSch -> do
    prog <- fromProgSchema progSch
    let agent = Agent prog (fromSensorsSchema senSch) effs goal (DRight,False)
    return [( OFly agent , pos )]
  Wall pos -> Just [( OWall , pos )]
  LongWall pos1 pos2 -> Just $ map (\pos -> ( OWall , pos ) ) $ mkLine pos1 pos2


mkLine :: Pos -> Pos -> [Pos]
mkLine p1@(x1,y1) p2@(x2,y2) 
 | x1 == x2  = [(x1,y) | y <- [(min y1 y2)..(max y1 y2)] ]
 | otherwise = map (\x->(x,lineFun p1 p2 x)) [(min x1 x2)..(max x1 x2)]
 
nextPos :: Pos -> Pos -> Pos
nextPos p1@(x1,y1) p2@(x2,y2)
 | p1 == p2  = p1
 | x1 == x2  = if y2 > y1 then (x1,y1+1) else (x1,y1-1)
 | otherwise = let f x = let fx = lineFun p1 p2 x
                          in if fx == y1 then y1 else 
                             if fx > y1  then y1+1 else y1-1
                in if x1 < x2 then (x1+1,f (x1+1)) else (x1-1,f (x1-1)) 

lineFun :: Pos -> Pos -> (Int->Int)
lineFun (x1,y1) (x2,y2) 
 = let a = fromIntegral (y1-y2) /  fromIntegral (x1-x2) 
       b = fromIntegral y1 - a * fromIntegral x1
    in \ x -> round $ a*( fromIntegral x ) + b

fromProgSchema :: ProgSchema -> Maybe Prog
fromProgSchema boxSchemas = do
  (boxMap , fullBoxes) <- foldr f (Just (Map.empty,[]) ) boxSchemas
  return $ Prog boxMap fullBoxes
 where
 f :: BoxSchema -> Maybe (Map BoxId Box , [BoxId]) -> Maybe (Map BoxId Box , [BoxId])
 f boxSchema acc = do 
  (boxMap,fullBoxes) <- acc
  (boxId , box) <- boxFromSchema boxSchema
  let fullBoxes' = if isBoxFull box then boxId:fullBoxes else fullBoxes
  return ( Map.insert boxId box boxMap , fullBoxes' )

boxFromSchema :: BoxSchema -> Maybe (BoxId, Box) 
boxFromSchema (BS boxId (funName,fun,numSlots) slotsList joints) = do
  let f (port,val) mSlots = do slots <- mSlots ; insertToSlots slots port val
  slots <- foldr f (Just $ mkSlots numSlots) slotsList 
  return $ ( boxId , Box slots joints fun funName )

Just prog1 = fromProgSchema [
  BS 1 plus [(0,IntSlot 2),(1,IntSlot 3)] [(2,0)] ,
  BS 2 ide  [ ]                           [(3,0)] ,
  BS 3 plus [(1,IntSlot 4)]               [(4,0)] ,
  BS 4 copy [ ]                           [(1,0),(5,0)] ,
  BS 5 copy [ ]                           [(1,1),(3,1)] 
 ]

Just prog2 = fromProgSchema [
  BS 1 rot   [(0,DirSlot DRight),(1,DirSlot DDown)]  [(2,0)] ,
  BS 2 rotCW [ ]                                     [(3,0)] ,
  BS 3 rot   [(1,DirSlot DLeft)]                     [(4,0)] ,
  BS 4 copy  [ ]                                     [(1,0),(5,0)] ,
  BS 5 copy  [ ]                                     [(1,1),(3,1)] 
 ]

stepsProg :: Prog -> Int -> [Prog]
stepsProg p (-1) = []
stepsProg p n    = p : stepsProg (stepProg p) (n-1)


stepProg :: Prog -> Prog
stepProg p@(Prog boxMap queue) = case queue of
  [] -> p
  fullBox:queue' -> let (boxMap',newFulls) = fire boxMap fullBox 
                     in Prog boxMap' $ queue' ++ newFulls 

fire :: Map BoxId Box -> BoxId -> (Map BoxId Box,[BoxId])
fire boxMap boxId1 = case Map.lookup boxId1 boxMap of
  Nothing -> (boxMap',[])
  Just box1@(Box _ joints _ _) -> case fire' box1 of
    Nothing -> (boxMap',[])
    Just rets -> foldr f (boxMap',[]) $ zip joints rets   
 where
 boxMap' = Map.update (Just . clearSlots) boxId1 boxMap
 f :: ((BoxId,SlotPort),Slot) -> (Map BoxId Box,[BoxId]) -> (Map BoxId Box,[BoxId])
 f ((boxId2,port),val) acc@(boxMap,fulls) = case Map.lookup boxId2 boxMap of
  Nothing -> acc
  Just box2@(Box slots _ _ _) -> case insertToSlots slots port val  of
    Nothing -> acc
    Just slots' -> (Map.insert boxId2 (setSlots box2 slots') boxMap, 
                    if areSlotsFull slots' then boxId2:fulls else fulls )
 fire' :: Box -> Maybe [Slot]
 fire' (Box (Slots slotMap _ free) js fun _) 
  | free == 0 = fun . map snd $ Map.toAscList slotMap
  | otherwise = Nothing  

getSlot :: SlotAddr -> Prog -> Maybe Slot
getSlot (boxId,port) (Prog boxMap _) = do
 (Box (Slots slotMap _ _) _ _ _) <- Map.lookup boxId boxMap
 Map.lookup port slotMap 

cutSlot ::  SlotAddr -> Prog -> ( Maybe Slot , Prog )
cutSlot addr prog = case cutSlot' addr prog of
  Just ( slot , prog' ) -> ( Just slot , prog' )
  Nothing               -> ( Nothing   , prog  ) 
 where
  cutSlot' :: SlotAddr -> Prog -> Maybe ( Slot , Prog )
  cutSlot' (boxId,port) (Prog boxMap fulls) = do
   box@(Box (Slots slotMap num free) _ _ _) <- Map.lookup boxId boxMap
   retSlot <- Map.lookup port slotMap
   let slotMap' = Map.delete port slotMap
   let box' = setSlots box (Slots slotMap' num (free+1) ) 
   return $ ( retSlot , Prog (Map.insert boxId box' boxMap) (if free==0 then fulls \\ [boxId] else fulls) )

insertToSensor :: Slot -> (BoxId,SlotPort) -> Prog -> Prog
insertToSensor val addr@(boxId,_) (Prog boxMap fulls) 
  = Prog boxMap' $ if isFull then fulls ++ [boxId] else fulls  
 where 
 (boxMap' , isFull) = insertToSensor' val addr boxMap
 insertToSensor' :: Slot -> (BoxId,SlotPort) -> Map BoxId Box -> (Map BoxId Box,Bool)
 insertToSensor' val (boxId,port) boxMap 
  = case Map.lookup boxId boxMap of
   Nothing -> (boxMap,False)
   Just box@(Box slots _ _ _) -> case insertToSlots slots port val of
    Nothing -> (boxMap,False)
    Just slots' -> let box' = setSlots box slots'
                    in ( Map.insert boxId box' boxMap , isBoxFull box' )

isQueueEmpty :: Prog -> Bool
isQueueEmpty (Prog _ queue) = null queue 
  
mkProg :: [(BoxId,BoxFun,NumSlots,[(BoxId,SlotPort)],FunName)] -> Prog
mkProg xs = Prog ( foldr (\(boxId,fun,num,js,name)->Map.insert boxId $ mkBox fun num js name ) Map.empty xs ) []

mkBox :: BoxFun -> NumSlots -> [(BoxId,SlotPort)] -> FunName -> Box
mkBox fun numSlots joints name = Box (mkSlots numSlots) joints fun name

mkSlots :: Int -> Slots
mkSlots numSlots = Slots Map.empty numSlots numSlots


setSlots :: Box -> Slots -> Box
setSlots (Box _ js fun name ) slots = Box slots js fun name 

insertToSlots :: Slots -> SlotPort -> Slot -> Maybe Slots
insertToSlots (Slots slotMap num free) port value 
  | port >= 0 && port < num = let free' = case Map.lookup port slotMap of 
                                           Nothing -> free-1
                                           _       -> free
                               in Just $ Slots (Map.insert port value slotMap) num free'
  | otherwise = Nothing

areSlotsFull :: Slots -> Bool
areSlotsFull (Slots _ numSlots numFreeSlots ) = numFreeSlots == 0 && numSlots > 0

isBoxFull :: Box -> Bool
isBoxFull (Box slots _ _ _) = areSlotsFull slots

clearSlots :: Box -> Box
clearSlots (Box (Slots _ numSlots _) js fun name) = Box (mkSlots numSlots) js fun name


instance Show Box   where show = showBox
instance Show Prog  where show = showProg
instance Show Slot  where show = showSlot
instance Show World where show = showWorld (0,0)
instance Show Agent where show = showAgent


showWorld :: Pos -> World -> String
showWorld pos@(posX,posY) (World objMap progss) 
  = hLine ++
    "\nprogss:" ++ (concatMap (\x->dotLine++show x) progss) ++ 
    hLine ++
    (concatMap (\(pos,agent)-> "\nPos : " ++ show pos ++ show agent) flies) ++   
    hNumLine ++  
    (unlines $ map (\(i,s)->s++" "++show i) $ zip [posY-yHalf..] 
             $ map (\s->' ':s)$ map (intersperse ' ') $ map ( map draw' ) poss)
 where
 (xHalf,yHalf) = (21,21)
 (xStart,xFin)  = (posX-xHalf,posX+xHalf)
 hLine = "\n" ++ map (\_->'-') [1..85]
 dotLine = "\n" ++ map (\_->'.') [1..45] ++ "\n"
 hNumLine = "\n" ++ concatMap 
  (\i->(if i < 10 && i >= 0  then " "++show i else (if i<(-9) then show (-i) else show i) ) ) 
  [xStart..xFin] ++ "\n"
 poss :: [[Pos]]
 poss  = [[(x,y)|x<-[(posX-xHalf)..(posX+xHalf)]]|y<-[(posY-yHalf)..(posY+yHalf)]] 
 draw obj = case obj of 
  OWall -> 'W' 
  OFly _ -> 'F' 
 draw' pos@(px,py) = case lookupInListMap pos objMap of 
  [] -> if pos `elem` goals then 'x' else 
         if (mod px 5,mod py 5)/=(0,0) then '.' else 
          if pos/=(0,0) then ',' else ';' 
  (x:_) -> draw x
 flies = getFlies objMap
 goals = map (\(_,Agent _ _ _ goal _)->goal) flies 

showAgent :: Agent -> String
showAgent (Agent prog sensors effectors goal lastMove)
 = "\ngoal : " ++ show goal ++
   "\nlastMove : " ++ show lastMove ++ 
   show prog

showProg :: Prog -> String
showProg (Prog boxMap fullBoxes) 
 = (:) '\n' $ foldr (\(boxId,box) str -> 
  fillStr 3 (show boxId) ++ ": " ++ 
  showBox box ++ "\n" ++ 
  str ) "" $ Map.toAscList boxMap 

showBox :: Box -> String
showBox (Box slots js _ funName)  
 = fillStr 10 funName ++ ": " ++ 
   fillStr 15 (showSlots slots) ++ ": " ++ 
   show js

showSlots :: Slots -> String
showSlots (Slots slotsMap numSlots _) 
 = show $ map snd $ fill (Map.toAscList slotsMap) (0,numSlots-1) FreeSlot  

showSlot :: Slot -> String
showSlot x = case x of
  FreeSlot   -> "_"
  IntSlot  i -> show i
  BoolSlot b -> show b
  DirSlot  d -> show d
  ListSlot ss-> show ss

fill :: [(Int,a)] -> (Int,Int) -> a -> [(Int,a)]
fill [] (i,j) val = map (\n->(n,val)) [i..j]
fill xx@((k,x):xs) (i,j) x' 
 | k > i     = (i,x') : fill xx (i+1,j) x'
 | otherwise = (k,x ) : fill xs (i+1,j) x'  

fillStr :: Int -> String -> String
fillStr len str = let l = length str in str ++ [' '|_<-[0..len-l-1]] 

fpow :: (a->a) -> Int -> (a->a) 
fpow f 0 x = x
fpow f n x = f $ fpow f (n-1) x

plus  = ("plus" , plus'  , 2)
plus' :: [Slot] -> Maybe [Slot]
plus' [IntSlot x , IntSlot y] = Just $ [IntSlot $ x + y]
plus' _ = Nothing
 
ide   = ("id"   , ide'   , 1)
ide' :: [Slot] -> Maybe [Slot]
ide' [x] = Just $ [x]
ide' _ = Nothing

copy  = ("copy" , copy'  , 1)
copy' :: [Slot] -> Maybe [Slot]
copy' [x] = Just [x,x] 
copy' _   = Nothing

rot = ("rot" , rot' , 2)
rot' :: [Slot] -> Maybe [Slot]
rot' [DirSlot d1,DirSlot d2] = Just [DirSlot $ dRot d1 d2]
rot' _ = Nothing

rotCW = ("rotCW", rotCW' , 1)
rotCW' :: [Slot] -> Maybe [Slot]
rotCW' [DirSlot d] = Just [DirSlot $ dRotCW d]
rotCW' _ = Nothing

rot180 = ("rot180", rot180' , 1)
rot180' :: [Slot] -> Maybe [Slot]
rot180' [DirSlot d] = Just [DirSlot $ dRot180 d]
rot180' _ = Nothing

dRotCW :: Dir -> Dir
dRotCW DUp     = DRight
dRotCW DRight  = DDown
dRotCW DDown   = DLeft
dRotCW DLeft   = DUp
dRotCW DRandom = DRandom

dRotCCW :: Dir -> Dir
dRotCCW DUp     = DLeft
dRotCCW DRight  = DUp
dRotCCW DDown   = DRight
dRotCCW DLeft   = DDown
dRotCCW DRandom = DRandom

dRot180 :: Dir -> Dir
dRot180 DUp     = DDown
dRot180 DRight  = DLeft
dRot180 DDown   = DUp
dRot180 DLeft   = DRight
dRot180 DRandom = DRandom

dRot :: Dir -> Dir -> Dir
dRot DUp     = id
dRot DDown   = dRot180
dRot DRight  = dRotCW
dRot DLeft   = dRotCCW
dRot DRandom = (\_->DRandom) 