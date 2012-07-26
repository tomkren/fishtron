module FF where
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Char
import Data.Maybe
import Util
import Debug.Trace


l1 = mkSea [
 "                     ",
 "         b           ",
 "                      ",
 "      aaaaa          ",
 "       11           ",
 "       $$              ",
 "                      ",
 "                      ",
 "                      ",
 "                      "
 ]

l2 = mkSea [
  "                      ",
  "      bbb             ",
  "      b                ",       
  "       11             ",
  "                      ",
  "                      ",
  "                      ",
  "                      ",
  "                      "
  ]

l3 = mkSea [
  "        kk             ",
  "        aa              ",
  "        a              ",
  "        aa11          ",
  "        a$            ",       
  "        az             ",
  "        a             ",
  "        a$$             ",
  "        ay            ",
  "        axx              ",
  "        aa              ",
  "                      ",
  "$$$$$$$$$$$$$$$$$$$$$$"
  ]

l4 = mkSea [
  "                    ",
  "                    ",
  "                    ",
  "                    ",
  "                    ",
  "                    ",
  "       11a           ",
  "       aaa             ",
  "       22a             ",
  "                    ",
  "$$$$$$$$$$$$$$$$$$$$$$"
  ]

l5 = mkSea [
  "        c            ",
  "                       ",
  "       aaa             ",
  "       22a             ",
  "        b            ",
  "$$$$$$$$$$$$$$$$$$$$$$"
  ]

l6 = mkSea [
  "                       ",
  "       aaa             ",
  "       22a             ",
  "         b            ",
  "$$$$$$$$$$$$$$$$$$$$$$"
  ]

l7 = mkSea [
  "                       ",
  "       aaa             ",
  "       22a             ",
  "          b            ",
  "$$$$$$$$$$$$$$$$$$$$$$"
  ]

l8 = mkSea [
  "                       ",
  "                       ",
  "       aaa A             ",
  "       11a A             ",
  "$$$$$$$$$$$$$$$$$$$$$$"
  ]

l9 = mkSea [
  "                      ",
  "                 222  ",
  "           AAAAA 222  ",
  "       11  a   b      ",
  "$$$$$$$$$$$$$$$$$$$$$$"
  ]

l0@(Sea l0_posMap l0_obMap l0_moving l0_rec l0_fid _) = mkSea [
 "        s                ",
 "       ss            g   ",
 "     $ saaaa             ",
 "        b                ",
 "        cc$$       ffff  ",
 "        d       eeef11     ",
 "   h            eee     ",
 "   h    33               ",
 "   h    w                ",
 "  $$$$$$$$$$$$$$$$$$$$$  ",
 "                         ",
 "                         "
 ]
lvl_0 = Map.toAscList l0_obMap

lvl1@(Sea l1_posMap l1_obMap l1_moving l1_rec l1_fid _) = mkSea [
 "$$$$$$$$$$$$$$$$$  ",
 "$11     cc      $  ",
 "$     bbb    a  $  ",
 "$$$ $ b   $$$$  $  ",
 " e$ $ $$$$$$$$  $  ",
 " f$ $$$$$$$$$$  $  ",
 " f              $  ",
 " $$$  $$$$$$$$  $  ",
 " $$$         $  $$$",
 " $$$          g   $",
 "$$$$$$$$$$$$$$$$$$$"]

-- test1 = steps lvl1 sol1

sol1 = [Rs 14,D,Ls 10,Rs 10,Ds 4,Ls 16]

ahoj = [ 
 "   AAA              OOOOO       J " ,
 "  A   A   H     H  O   $ O      J " ,
 " A     A  H     H  O     O      J " ,
 " AAAAAAA  HHHHHHH  O     O      J " ,
 " A     A  H     H  O     O      J " ,
 " A     A           O     O  J   J " ,
 "       AAA          OOOOO    JJJ  " ,
 "         AA                       " ,
 "$$$$$$$$$          $$$$$$$$$$$$$$$"]
--0123456789012345678901234567890123456789
exSea@(Sea exPosMap exObMap exMoving exRectangle exFid _) = mkSea ahoj
exObLst = Map.toAscList exObMap
exA = snd $ exObLst !! 0
exO = snd $ exObLst !! 1

zvon = [
  [ ],
  [(3,5)],
  [(2,2),(6,6)],
  [(1,7)],
  [(0,8)],
  [(2,3),(6,7)]
 ]

--------------------------------------------------------------------

data Ob = Ob
 { getObType       :: ObType  
 , getShape        :: Shape   
 , getSignifs      :: Signifs 
 , getPos          :: Pos    
 , getPx           :: Px }   

data Sea = Sea
 { getPosMap     :: PosMap 
 , getObMap      :: ObMap 
 , getMoving     :: Moving
 , getRectangle  :: Rectangle 
 , getFishes     :: Fishes
 , getGameStatus :: GameStatus }

data ObType = ObFix | ObStd | ObSteel | ObFish FishType deriving (Show)
data GameStatus = Normal | GameOver | YouWin deriving (Eq)

data FishType = SmallFish | BigFish | DeadFish deriving (Show)

data MovingStatus = Mov | MayMov | Stop

type PosMap = Map Pos  ObId
type ObMap  = Map ObId Ob
type Maps   = (PosMap,ObMap)
type Moving = [ObId]
type Rectangle = (Pos,Pos)
type Fishes = [ObId]
type ObId   = Int
type Px     = Char
type Pos      = (Int,Int)

type Sigs = [Pos]
type SigsDown  = Sigs
type SigsUp    = Sigs
type SigsRight = Sigs
type SigsLeft  = Sigs
type Signifs = (SigsDown, SigsUp, SigsRight, SigsLeft)

type Shape = [[ HLine ]] 
type HLine = (Int,Int)

data Dir = DUp | DDown | DLeft | DRight deriving (Show,Eq)
data Cmd = S | U | D | L | R | Us Int | Ds Int | Ls Int | Rs Int



-- fish STEPing --

steps :: Sea -> [Cmd] -> [Sea]
steps sea cmds = fishSteps sea $ concatMap toDir cmds 

fishSteps :: Sea -> [Either () Dir] -> [Sea]
fishSteps sea ds 
 | isGameOver sea = [sea]
 | otherwise = case ds of
  [] -> fallSteps sea
  (dir:dirs) ->
   let seas = fallSteps sea
       lSea = last seas
       sea' = fishStep lSea dir
    in seas ++ ( if isGameOver lSea then [] else fishSteps sea' dirs  )


fishStep :: Sea -> Either () Dir -> Sea
fishStep sea@(Sea posMap obMap [] rec fishes@(fishId:restFishes) status) eDir = 
  case eDir of
   Left _ -> Sea posMap obMap [] rec (restFishes++[fishId]) status
   Right dir ->
    case getMovables dir maps fishId of
     Nothing -> sea
     Just pusheds -> 
      let maps'@(posMap',obMap') = foldr (move dir) maps pusheds
          moving'                = mkMoving maps'   -- TODO    U N E F F E C T I V E
          ( obMap'' , status' )  = updateKilling dir maps' status pusheds fishes
          status''               = if (status' == Normal) && (isGoal obMap'' rec fishId) 
                                   then YouWin else status' 
       in Sea posMap' obMap'' moving' rec fishes status'' 
 where
  maps = (posMap,obMap)

getMovables :: Dir -> Maps -> ObId -> Maybe [ObId]
getMovables dir maps@(_,obMap) fishId = 
  let (normal,walls,fishes) = halfDeepNeigbors2 dir maps fishId  
   in case (walls,fishes) of
    ([],[fishId']) -> 
     if (fishId == fishId') && (checkForSteel fishId normal )   
      then Just (fishId:normal) 
      else Nothing
    _ -> Nothing
 where
  checkForSteel :: ObId -> [ObId] -> Bool
  checkForSteel fishId oids = 
   ( isBigFish' obMap fishId ) || ( and $ map ( not . (isSteel' obMap) ) oids ) 


updateKilling :: Dir -> Maps -> GameStatus -> [ObId] -> [ObId] -> (ObMap,GameStatus)
updateKilling dir maps@(_,obMap) status pusheds fishes 
  | dir == DUp || dir == DDown = (obMap,status)
  | otherwise                  = foldr f (obMap,status) fishes
 where 
 f :: ObId -> (ObMap,GameStatus) -> (ObMap,GameStatus)
 f fishId acc@( obMap , status ) = 
   if ( isSpineKilled maps pusheds fishId ) || ( isSteelKilled maps fishId )
    then ( kill fishId obMap , GameOver )   
    else acc

isSpineKilled :: Maps -> [ObId] -> ObId -> Bool
isSpineKilled maps pusheds fishId =
  let (onSpine,_,_) = neighbors2 DUp maps fishId
      onSpine'      = onSpine `intersect` pusheds
      onSpine''     = filter (not . (isFixedByWall maps)) onSpine'
   in not $ null onSpine''

isSteelKilled :: Maps -> ObId -> Bool
isSteelKilled maps@(_,obMap) fishId =
 let (aboveSpine,_,_) = halfDeepNeigbors2 DUp maps fishId
     steels           = filter (isSteel' obMap) aboveSpine
     badSteels        = filter (not . (isFixedByWall maps)) steels
  in not $ null badSteels

isFixedByWall :: Maps -> ObId -> Bool
isFixedByWall maps oid =  not $ null walls 
 where (_,walls,_) = halfDeepNeigbors2 DDown maps oid


{--
updateWinning :: Maps -> Rectangle -> GameStatus -> Fishes -> (Maps,GameStatus,Fishes)
updateWinning maps@(posMap,obMap) rec status fishes@(fishId:restFishes) 
 | (status == Normal) && (isGoal obMap rec fishId) = 
   let 

   bude potřeba předelat mkMoving kvuli odebírání oběktů ze scény
   protože je tam to [1..size] pač se mkMovingvolá při každym pohnutí

   současný winninig stačí libovolná rybka venku 
--}

isGoal :: ObMap -> Rectangle -> ObId -> Bool
isGoal obMap rec fishId =
  or $ map (rectangleTouched rec) fishPoses 
 where 
  fishPoses = posesOfOb $ getOb obMap fishId 

  rectangleTouched :: Rectangle -> Pos -> Bool
  rectangleTouched ((x1,y1),(x2,y2)) (x,y) 
   |  x1 >= x  = True
   |  y1 >= y  = True
   |  x2 <= x  = True
   |  y2 <= y  = True
   | otherwise = False 
  



-- FALL STEPing --



fallSteps :: Sea -> [Sea]
fallSteps sea 
 | isGameOver sea = [sea]
 | otherwise = case getMoving sea of
  [] -> [sea]
  _  ->  sea : (fallSteps $ fallStep sea)

fallStep :: Sea -> Sea
fallStep (Sea posMap obMap moving rec fishes status) =  
   let maps'@(posMap',obMap') = foldr (move DDown) (posMap,obMap) moving 
       moving'                = updateMoving maps' moving
       stoppeds               = moving \\ moving'
       killedFishes           = getKilledFishes2 maps' stoppeds
       status'                = if null killedFishes then status else GameOver -- status -- TODO
       obMap''                = foldr kill obMap' killedFishes
    in trace (show ( pxs stoppeds , pxs killedFishes )) 
     $ Sea posMap' obMap'' moving' rec fishes status' 
 where
  pxs = map (pxByObId obMap) 


kill :: ObId -> ObMap -> ObMap
kill fishId obMap = Map.adjust killFish fishId obMap

move :: Dir -> ObId -> Maps -> Maps
move dir oid (posMap,obMap) = ( posMap'' , obMap' )
 where
  obMap'        = Map.adjust ( adjustPos dir ) oid obMap 
  Just ob       = Map.lookup oid obMap
  Just ob'      = Map.lookup oid obMap'
  posesToInsert = getSigPoses         dir  ob
  posesToDelete = getSigPoses (rot180 dir) ob'
  posMap'       = foldr      (mapDeletus oid) posMap  posesToDelete
  posMap''      = foldr (flip Map.insert oid) posMap' posesToInsert

  mapDeletus :: ObId -> Pos -> PosMap -> PosMap 
  mapDeletus oid = Map.update (\oid'-> if oid' == oid then Nothing else Just oid' ) 

updateMoving :: Maps -> [ObId] -> [ObId]
updateMoving maps@(posMap,obMap) moving = 
  let (hotovo,moving') = updateMoving' moving moving True []
   in if hotovo then moving' else updateMoving maps moving'   
 where
  updateMoving' :: [ObId] -> [ObId] -> Bool -> [ObId] -> (Bool,[ObId])
  updateMoving' []         _      hotovo acc = (hotovo,acc)
  updateMoving' (oid:rest) moving hotovo acc
   | isFixedNow maps oid moving 
               = updateMoving' rest (moving\\[oid]) False  acc 
   | otherwise = updateMoving' rest  moving         hotovo (oid:acc) 

  isFixedNow :: Maps -> ObId -> [ObId] -> Bool
  isFixedNow maps oid moving 
   = not . null $ (neighbors DDown maps oid) \\ moving

getKilledFishes :: Maps -> [ObId] -> [ObId]
getKilledFishes maps@(_,obMap) stoppeds 
  = if null walls then fishes else [] 
 where
  dns    = halfDeepNeigbors DDown maps stoppeds
  walls  = filter (isWall' obMap) $ dns 
  fishes = filter (isFish' obMap) $ dns


getKilledFishes2 :: Maps -> [ObId] -> [ObId]
getKilledFishes2 maps
  = concatMap getKilledFishes2' 
 where
  getKilledFishes2' :: ObId -> [ObId]
  getKilledFishes2' stopped =
   let (_,walls,fishes) = halfDeepNeigbors2 DDown maps stopped
    in if null walls then fishes else []
 
-- neighbors --

deepNeigbors :: Dir -> Maps -> [ObId] -> [ObId]
deepNeigbors dir maps@(_,obMap) oids 
  = deepNeigbors' dir maps [] oids
 where
  deepNeigbors' :: Dir -> Maps -> [ObId] -> [ObId] -> [ObId]
  deepNeigbors' _   _    acc []     = acc
  deepNeigbors' dir maps acc (x:xs) = 
   let childs  = neighbors dir maps x
       childs' = childs \\ acc
    in deepNeigbors' dir maps (x:acc) (childs'++xs)

halfDeepNeigbors :: Dir -> Maps -> [ObId] -> [ObId]
halfDeepNeigbors dir maps@(_,obMap) oids 
  = deepNeigbors' dir maps [] oids
 where
  deepNeigbors' :: Dir -> Maps -> [ObId] -> [ObId] -> [ObId]
  deepNeigbors' _   _    acc []     = acc
  deepNeigbors' dir maps acc (x:xs) = 
   let childs  = neighbors dir maps x
       childs' =  childs \\ acc
       childs'' 
        | isWallOrFish' obMap x = filter (not . isWallOrFish' obMap) $ childs'
        | otherwise = childs' 
    in deepNeigbors' dir maps (x:acc) (childs''++xs)

neighbors :: Dir -> Maps -> ObId -> [ObId]
neighbors dir (posMap,obMap) oid
  = nub . catMaybes . map (\pos-> Map.lookup pos posMap ) $ 
    getSigPoses dir (fromJust $ Map.lookup oid obMap ) 

neighbors2 :: Dir -> Maps -> ObId -> ([ObId],[ObId],[ObId])
neighbors2 dir maps@(_,obMap) oid = obSort obMap $ neighbors dir maps oid

obSort :: ObMap -> [ObId] -> ([ObId],[ObId],[ObId])
obSort obMap = foldr f ([],[],[])
 where
  f oid (ns,ws,fs) = case getObType ob of
    ObStd    -> ( oid:ns ,     ws ,     fs )
    ObSteel  -> ( oid:ns ,     ws ,     fs )
    ObFix    -> (     ns , oid:ws ,     fs )
    ObFish _ -> (     ns ,     ws , oid:fs )
   where ob = getOb obMap oid 

{--
halfDeepNeigbors3 :: Dir -> Maps -> ObId -> ([ObId],[ObId],[ObId]) -- (normal,walls,fishes)
halfDeepNeigbors3 dir maps@(_,obMap) oid 
  = deepNeigbors' dir maps ([],[],[]) [oid]
 where
  deepNeigbors' :: Dir -> Maps -> ([ObId],[ObId],[ObId]) -> [ObId] -> ([ObId],[ObId],[ObId])
  deepNeigbors' _   _    acc                       []     = acc
  deepNeigbors' dir maps acc@(normal,walls,fishes) (x:xs) = 
   let (isW,isF) = isWall_isFish' obMap x
       childs    = neighbors dir maps x
       childs'   =  childs \\ (flattenAcc acc)
       childs'' 
        | isW || isF = filter (not . isWallOrFish' obMap) $ childs'
        | otherwise  = childs'
       acc'
        | isW       = (  normal,x:walls,  fishes)
        | isF       = (  normal,  walls,x:fishes) 
        | otherwise = (x:normal,  walls,  fishes) 
    in deepNeigbors' dir maps acc' (childs''++xs)

  flattenAcc :: ([a],[a],[a]) -> [a]
  flattenAcc (normal,walls,fishes) = fishes ++ ( walls ++ normal )
--}


halfDeepNeigbors2 :: Dir -> Maps -> ObId -> ([ObId],[ObId],[ObId]) -- (normal,walls,fishes)
halfDeepNeigbors2 dir maps@(_,obMap) firstOid 
  = deepNeigbors' dir maps ([],[],[]) [firstOid]
 where
  deepNeigbors' :: Dir -> Maps -> ([ObId],[ObId],[ObId]) -> [ObId] -> ([ObId],[ObId],[ObId])
  deepNeigbors' _   _    acc                       []     = acc
  deepNeigbors' dir maps acc@(normal,walls,fishes) (x:xs) = 
   let (isW,isF) = isWall_isFish' obMap x
       childs    = neighbors dir maps x
       childs'     
        | (isW || isF) && (x /= firstOid) = []
        | otherwise  = childs \\ (flattenAcc acc) 
       acc'
        | isW       = (  normal,x:walls,  fishes)
        | isF       = (  normal,  walls,x:fishes) 
        | otherwise = (x:normal,  walls,  fishes) 
    in deepNeigbors' dir maps acc' (childs'++xs)

  flattenAcc :: ([a],[a],[a]) -> [a]
  flattenAcc (normal,walls,fishes) = fishes ++ ( walls ++ normal )


-- MAKING SEA --


mkSea :: [String] -> Sea
mkSea strs 
  = let obs    = mkObs strs 
        obMap  = mkObMap obs
        posMap = mkPosMap obMap
     in Sea posMap obMap (mkMoving (posMap,obMap)) (getRectanFromStrs strs) (findFishes obMap) Normal
 where
  mkObMap :: [Ob] -> ObMap  
  mkObMap obs = Map.fromList $ zip [1..] obs

  mkPosMap :: ObMap -> PosMap
  mkPosMap obMap 
   = Map.fromList $ concatMap (\(oid,ob)->[ (pos,oid) | pos <- posesOfOb ob ] ) $ Map.toList obMap
  
  getRectanFromStrs :: [String] -> Rectangle
  getRectanFromStrs strs 
   = ( (0,0) , ( if null strs then 0 else (-1) + (maximum $ map length strs) , (-1) + length strs ) )  

  findFishes :: ObMap -> [ObId]
  findFishes obMap
   = map fst $ filter (\(_,ob)-> isFish ob ) $ Map.toList obMap
  
mkMoving :: Maps -> [ObId]
mkMoving maps@(_,obMap) 
  = [1..Map.size obMap] \\ fixeds
 where fixeds = halfDeepNeigbors DUp maps $ wallsAndFishes obMap 

wallsAndFishes :: ObMap -> [ObId]
wallsAndFishes obMap 
 = map fst $ filter (\(_,ob)-> isWallOrFish ob ) $ Map.toList obMap 


-- MAKING OBJECTS --

mkObs :: [String] -> [Ob]
mkObs strs = catMaybes $ map (mkOb strs) $ inChars strs

mkOb :: [String] -> Px -> Maybe Ob
mkOb strs px = do
 let sh = map (\str -> reverse $ f' $ foldl f [(0,0)] str) $ strs
 (pos,sh') <- shift sh
 return $ Ob obType sh' (mkSignifs sh') pos px
 where
  obType = case px of 
   '$' -> ObFix
   '1' -> ObFish SmallFish
   '2' -> ObFish BigFish 
   _   -> if isUpper px then ObSteel else ObStd

  shift :: [[HLine]] -> Maybe ( Pos , [[HLine]] )
  shift ss = do
    miX <- minimX ss
    let ss' = dropWhile null $ map (\hls->map (\(s,f)->(s-miX,f-miX)) hls ) ss
    let (miY,ss'')  = dropEmps 0 $ reverse $ ss'  
    return ( (miX , miY ) , ss''  )
  
  dropEmps :: Int -> [[HLine]] -> (Int,[[HLine]])
  dropEmps acc []     = (acc,[])
  dropEmps acc xxs@(x:xs) = case x of 
    [] -> dropEmps (acc+1) xs
    _  -> (acc,xxs)
  
  minimX :: [[HLine]] -> Maybe Int
  minimX ss = case ss of
   [] -> Nothing
   ss -> case concatMap (\hls->case hls of [] -> [] ; (s,_):_->[s]) ss of
    [] -> Nothing
    ms -> Just $ minimum ms
  
  f' :: [HLine] -> [HLine]
  f' ((s,f):xs) = if s == f then xs else (s,f-1):xs
  f :: [ HLine ] -> Char -> [ HLine ]
  f xs@((s,f):rest) ch 
   | ch == px  = (s  ,f+1)        :rest 
   | s  == f   = (s+1,s+1)        :rest 
   | otherwise = (f+1,f+1):(s,f-1):rest 


mkSignifs :: Shape -> ([Pos],[Pos],[Pos],[Pos])
mkSignifs shape = ( getSigs DDown  ps ,
                    getSigs DUp    ps ,
                    getSigs DRight ps ,
                    getSigs DLeft  ps )
 where 
  ps = toPoses shape  

  getSigs :: Dir -> [Pos] -> [Pos]
  getSigs _ [] = []
  getSigs dir ps 
    = map (\p-> plus2D p $ dirDelta dir ) $ 
      if dir == DUp || dir == DDown 
       then grs1 [] hlines 
       else grs2 [] vlines
   where
    grs1 :: [Pos] -> [[Pos]] -> [Pos]
    grs1 _   [] = []
    grs1 lst (act:rest) = (minus1 act lst) ++ (grs1 act rest)
  
    grs2 :: [Pos] -> [[Pos]] -> [Pos]
    grs2 _   [] = []
    grs2 lst (act:rest) = (minus2 act lst) ++ (grs2 act rest)
      
    minus1 :: [Pos] -> [Pos] -> [Pos]
    minus1 [] _ = []
    minus1 act@((_,y):_) lst = map (\x->(x,y)) $ (map fst act) \\ (map fst lst)
  
    minus2 :: [Pos] -> [Pos] -> [Pos]
    minus2 [] _ = []
    minus2 act@((x,_):_) lst = map (\y->(x,y)) $ (map snd act) \\ (map snd lst)
  
    xs = map fst ps
    ys = map snd ps
    xMax = maximum $ xs 
    xMin = minimum $ xs
    yMax = maximum $ ys 
    yMin = minimum $ ys
    list = case dir of
      DRight -> [xMax,xMax-1..xMin]
      DLeft  -> [xMin..xMax]
      DUp    -> [yMax,yMax-1..yMin]
      DDown  -> [yMin..yMax]
    vlines , hlines :: [[Pos]]
    vlines = zipWith (\x ps-> (filter (\(x',_)->x==x') ps)  ) list (repeat ps)
    hlines = zipWith (\y ps-> (filter (\(_,y')->y==y') ps)  ) list (repeat ps)



-- GENERAL OVER OBs --

getSigPoses :: Dir -> Ob -> [Pos]
getSigPoses dir (Ob _ _ signifs pos _ ) = map (`plus2D` pos) $ byDir dir signifs


posesOfOb :: Ob -> [Pos]
posesOfOb (Ob _ sh _ (posX,posY) _) 
 = concatMap (\(y,hls)-> concatMap (\(s,f) -> [(posX+x,posY+y)| x <- [s..f] ] ) hls ) $ zip [0..] sh 

isFish :: Ob -> Bool
isFish ob = case getObType ob of
 ObFish _ -> True
 _ -> False

isWall :: Ob -> Bool
isWall ob = case getObType ob of
 ObFix -> True
 _ -> False

isWallOrFish :: Ob -> Bool
isWallOrFish ob = isWall ob || isFish ob

isWall_isFish :: Ob -> (Bool,Bool)
isWall_isFish ob = ( isWall ob , isFish ob )

adjustPos :: Dir -> Ob -> Ob
adjustPos dir (Ob t sh sigs pos px) 
 = Ob t sh sigs (pos `plus2D` dirDelta dir ) px

killFish :: Ob -> Ob
killFish (Ob (ObFish _) sh sigs pos px ) 
 = Ob (ObFish DeadFish) sh sigs pos px
killFish _ = error "Killing not-fish is illegal!"

isDeadFish :: Ob -> Bool
isDeadFish ob = case getObType ob of
 ObFish DeadFish -> True
 _ -> False

isBigFish :: Ob -> Bool
isBigFish ob = case getObType ob of
 ObFish BigFish -> True
 _ -> False

isSteel :: Ob -> Bool
isSteel ob = case getObType ob of 
 ObSteel -> True
 _ -> False


-- General over ObId ---

getOb :: ObMap -> ObId -> Ob
getOb obMap oid = fromJust $ Map.lookup oid obMap

pxByObId :: ObMap -> ObId -> Px
pxByObId obMap = getPx . (getOb obMap)

isFish'       = liftToObId isFish 
isWall'       = liftToObId isWall  
isWallOrFish' = liftToObId isWallOrFish
isWall_isFish'= liftToObId isWall_isFish
isBigFish'    = liftToObId isBigFish
isSteel'      = liftToObId isSteel

liftToObId :: (Ob -> a) -> (ObMap -> ObId -> a)
liftToObId f obMap = f . (getOb obMap)

-- general over sea --

isGameOver :: Sea -> Bool
isGameOver sea = GameOver == getGameStatus sea

-- GENERAL GENERAL --


plus2D :: Pos -> Pos -> Pos
plus2D (x,y) (a,b) = (x+a,y+b)

dirDelta :: Dir -> Pos
dirDelta dir = case dir of
 DDown  -> ( 0,-1) 
 DUp    -> ( 0, 1)
 DRight -> ( 1, 0)
 DLeft  -> (-1, 0)

byDir :: Dir -> (a,a,a,a) -> a
byDir dir (d,u,r,l) = case dir of
 DDown -> d
 DUp -> u
 DRight -> r
 DLeft -> l

rot180 :: Dir -> Dir
rot180 DUp     = DDown
rot180 DRight  = DLeft
rot180 DDown   = DUp
rot180 DLeft   = DRight

toPoses :: [[HLine]] -> [Pos]
toPoses shape = concat $ zipWith (\xs y -> map (\x->(x,y)) xs) (toInts shape) [0..] 

toInts :: [[HLine]] -> [[Int]]
toInts = map $ concatMap $ \ (s,f) -> [s..f] 

inChars :: [String] -> [Char]
inChars strs = sort $ (nub $ concat strs) \\ [' ']

toDir :: Cmd -> [Either () Dir]
toDir cmd = case cmd of
 S -> [Left ()]
 _ -> map Right $ toDir' cmd

toDir' :: Cmd -> [Dir]
toDir' cmd = case cmd of
 U -> [DUp   ]
 D -> [DDown ]
 L -> [DLeft ]
 R -> [DRight]
 Us n -> replicate n DUp
 Ds n -> replicate n DDown
 Ls n -> replicate n DLeft
 Rs n -> replicate n DRight


-- SHOWING --



instance Show Ob  where show = showOb
instance Show Sea where show = showSea

showSea :: Sea -> String
showSea sea@(Sea posMap obMap _  ((x1,y1),(x2,y2)) _ status)
 = (++) (( (++) "moving: " $ intersperse ',' $ map (pxByObId obMap) $ getMoving sea)++".") $
   (++) ('\n' : (case status of Normal -> "" ; YouWin -> "YOU WIN!\n\n" ; GameOver -> "GAME OVER!\n\n")) $ 
   concat [ [ px | 
        x <- [x1..x2] , 
        let obId = Map.lookup (x,y) posMap ,
        let px = case obId of 
                  Nothing -> ' ' 
                  Just oid -> 
                   let Just ob@(Ob _ _ _ _ px) = Map.lookup oid obMap 
                    in if isDeadFish ob then '#' else px 
             ] ++ "\n" |
       my <- [(-y2)..(-y1)] , 
       let y = -my]

showOb :: Ob -> String
showOb (Ob t sh (sd,su,sr,sl) pos px) 
 = "\n" ++ (showShape px sh)++ show pos ++ " " ++ (show t) 
   -- ++ (showPoses sd) ++ (showPoses su) ++ (showPoses sr)++ (showPoses sl)  

showPoses :: [Pos] -> String
showPoses [] = ""
showPoses ps 
  = (:) '\n' $ concatMap (\ps-> (map pxForPos ps)++"\n" ) 
    [ [(x,y) | x <- [xMin..xMax] ] | y' <- [(-yMax)..(-yMin)] , let y = -y' ] 
 where 
  px = '#'
  pxForPos pos = case Map.lookup pos pmap of 
   Nothing -> ' '
   _       -> px
  pmap = Map.fromList $ zip ps (repeat ())
  xMax = maximum $ map fst ps
  yMax = maximum $ map snd ps
  xMin = minimum $ map fst ps
  yMin = minimum $ map snd ps

showShape :: Px -> Shape -> String
showShape px ss = concatMap f' $ reverse ss
 where
  f' [] = "\n"
  f' xs@((s,_):_) = (replicate (s) ' ') ++ (f xs)
  f :: [HLine] -> String
  f []                      = "\n"
  f [(s,f)]                 =  replicate (f -s +1) px ++ "\n"
  f ((s1,f1):(x@(s2,_)):xs) = (replicate (f1-s1+1) px) ++ (replicate (s2-f1-1) ' ') ++ (f $ x:xs)




