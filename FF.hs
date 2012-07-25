module FF where
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Maybe
import Util
import Debug.Trace


lvl0@(Sea l0_posMap l0_obMap l0_moving l0_rec l0_fid _) = mkSea [
 "        s                ",
 "       ss            g   ",
 "     $ saaaa             ",
 "        b                ",
 "        cc$$       ffff  ",
 "        d       eeef$   ",
 "   h            eee 11   ",
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

data ObType = ObFix | ObStd | ObFish FishType deriving (Show)
data GameStatus = Normal | GameOver | YouWin

data FishType = SmallFish | BigFish deriving (Show)

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
data Cmd = U | D | L | R | Us Int | Ds Int | Ls Int | Rs Int


-- FALL STEPing --

fallSteps :: Sea -> [Sea]
fallSteps sea = case getMoving sea of
 [] -> [sea]
 _  ->  sea : (fallSteps $ fallStep sea)

fallStep :: Sea -> Sea
fallStep (Sea posMap obMap moving rec fishes status) = 
 let maps@(posMap',obMap') = foldr (move DDown) (posMap,obMap) moving 
     moving'               = updateMoving maps moving
     status'               = status -- TODO
     stoppeds              = moving \\ moving'
     pxs                   = map (pxByObId obMap)
  in trace (show ( pxs stoppeds ,
                   pxs $ getKilledFishes maps stoppeds)) 
   $ Sea posMap' obMap' moving' rec fishes status' 

move :: Dir -> ObId -> Maps -> (PosMap,ObMap)
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
getKilledFishes maps@(_,obMap) stoppeds =
 filter (isFish' obMap) $ deepNeigbors DDown maps stoppeds


-- neighbors --


deepNeigbors :: Dir -> Maps -> [ObId] -> [ObId]
deepNeigbors dir maps oids = deepNeigbors' dir maps [] oids
 where
  deepNeigbors' :: Dir -> Maps -> [ObId] -> [ObId] -> [ObId]
  deepNeigbors' _   _    acc []     = acc
  deepNeigbors' dir maps acc (x:xs) = 
   let childs  = neighbors dir maps x
       childs' = childs \\ acc
    in deepNeigbors' dir maps (x:acc) (childs'++xs)

neighbors :: Dir -> Maps -> ObId -> [ObId]
neighbors dir (posMap,obMap) oid
  = nub . catMaybes . map (\pos-> Map.lookup pos posMap ) $ 
    getSigPoses dir (fromJust $ Map.lookup oid obMap ) 


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
 where fixeds = deepNeigbors DUp maps $ wallsAndFishes obMap 

wallsAndFishes :: ObMap -> [ObId]
wallsAndFishes obMap 
 = map fst $ filter (\(_,ob)-> isWall ob || isFish ob ) $ Map.toList obMap 


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
   _   -> ObStd

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

adjustPos :: Dir -> Ob -> Ob
adjustPos dir (Ob t sh sigs pos px) 
 = Ob t sh sigs (pos `plus2D` dirDelta dir ) px


-- General over ObId ---

getOb :: ObMap -> ObId -> Ob
getOb obMap oid = fromJust $ Map.lookup oid obMap

pxByObId :: ObMap -> ObId -> Px
pxByObId obMap = getPx . (getOb obMap)

isFish' , isWall' :: ObMap -> ObId -> Bool
isFish' = liftToObId isFish 
isWall' = liftToObId isWall  

liftToObId :: (Ob -> a) -> (ObMap -> ObId -> a)
liftToObId f obMap = f . (getOb obMap)

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
                  Just oid -> let Just (Ob _ _ _ _ px) = Map.lookup oid obMap in px ] ++ "\n" |
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




