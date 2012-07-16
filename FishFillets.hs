module FishFillets where
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Maybe
import Util


type PosMap    = Map Pos  ObId
type ObMap     = Map ObId Ob
type MultiObMap= Map ObId ObId
type Moving    = [ObId]
type Rectangle = (Pos,Pos)

data Sea = Sea PosMap ObMap Moving Rectangle FishId

type Pos      = (Int,Int)
type HLine    = (Int,Int)
type ObId   = Int
type Px     = Char
type FishId = Maybe ObId

type Sigs = [Pos]

type SigsDown  = Sigs
type SigsUp    = Sigs
type SigsRight = Sigs
type SigsLeft  = Sigs

type Signifs = (SigsDown, SigsUp, SigsRight, SigsLeft)

data Ob = Ob ObType Shape Signifs Pos Px -- Pos je dolního levýho rohu 

data ObType = ObFix | ObStd | ObFish deriving (Show)

type Shape = [[ HLine ]] 

data Dir = DUp | DDown | DLeft | DRight deriving (Show,Eq)

instance Show Ob  where show = showOb
instance Show Sea where show = showSea

steps :: Sea -> [Dir] -> [Sea]
steps sea dirs 
 = let seas = fallSteps sea
       sea' = last seas
    in undefined

fishStep :: Sea -> Dir -> Sea
fishStep sea@(Sea posMap obMap _ _ m_fid) dir 
 = case m_fid of
  Nothing  -> sea
  Just fid -> 
   let fish@(Ob _ _ signifs pos _) = fromJust $ Map.lookup fid obMap
       fish' =  updatePos fish dir 
    in undefined

updatePos :: Ob -> Dir -> Ob
updatePos (Ob t sh signifs pos px) dir 
 = Ob t sh signifs (pos `plus2D` (dirDelta dir)) px

fallSteps :: Sea -> [Sea]
fallSteps sea@(Sea _ _ [] _ _) = [sea]
fallSteps sea = sea : (fallSteps $ fallStep sea)

fallStep :: Sea -> Sea 
fallStep sea 
 = let (Sea posMap obMap _ rec fid) = last $ subSteps sea
    in Sea posMap obMap (mkMoving posMap obMap) rec fid

subSteps :: Sea -> [Sea]
subSteps sea@(Sea _ _ [] _ _) = [sea]
subSteps sea = subSteps' [] sea
 where
  subSteps' :: [ObId] -> Sea -> [Sea]
  subSteps' _ sea@(Sea _ _ [] _ _) = [sea]
  subSteps' moved sea 
   = let (sea',moved') = subStep sea moved 
      in sea : ( subSteps' moved' sea' )

subStep :: Sea -> [ObId] -> (Sea,[ObId])
subStep sea@(Sea _ _ [] _ _) moved = (sea,moved) 
subStep (Sea posMap obMap moving rec fid) moved  
  = let moved' = moving ++ moved 
     in ( Sea posMap' obMap' (mkMoving2 moved' posMap' obMap') rec fid , moved' )
 where 
  moveIt :: ObId -> ObMap -> ObMap
  moveIt obId acc = Map.adjust (\(Ob t sh si pos px)->Ob t sh si (plus2D pos (0,-1)) px ) obId acc 
  obMap'  = foldr moveIt obMap moving
  posMap' = mkPosMap obMap' -- UNEFFECTIVE !!! TODO  


showSea :: Sea -> String
showSea (Sea posMap obMap _  ((x1,y1),(x2,y2)) _ )
 = (:) '\n' $ concat [ [ px | 
        x <- [x1..x2] , 
        let obId = Map.lookup (x,y) posMap ,
        let px = case obId of 
                  Nothing -> ' ' 
                  Just oid -> let Just (Ob _ _ _ _ px) = Map.lookup oid obMap in px ] ++ "\n" |
       my <- [(-y2)..(-y1)] , 
       let y = -my]

mkObMap :: [Ob] -> ObMap 
mkObMap obs = Map.fromList $ zip [1..] obs

mkPosMap :: ObMap -> PosMap
mkPosMap obMap 
 = Map.fromList $ concatMap (\(obId,ob)->[ (pos,obId) | pos <- posList ob ] ) $ Map.toList obMap

posList :: Ob -> [Pos]
posList (Ob _ sh _ (posX,posY) _) 
 = concatMap (\(y,hls)-> concatMap (\(s,f) -> [(posX+x,posY+y)| x <- [s..f] ] ) hls ) $ zip [0..] sh 

mkObs :: [String] -> [Ob]
mkObs strs = catMaybes $ map (mkOb strs) $ inChars strs

getRectanFromStrs :: [String] -> Rectangle
getRectanFromStrs strs = ( (0,0) , ( if null strs then 0 else (-1) + (maximum $ map length strs) , (-1) + length strs ) )  

mkSea :: [String] -> Sea
mkSea strs 
 = let obs    = mkObs strs 
       obMap  = mkObMap obs
       posMap = mkPosMap obMap
    in Sea posMap obMap (mkMoving posMap obMap) (getRectanFromStrs strs) (findFish obMap)

findFish :: ObMap -> Maybe ObId
findFish obMap = f $ Map.toList obMap
 where 
  f [] = Nothing
  f ((oid,Ob ObFish _ _ _ _):_) = Just oid
  f (_:xs) = f xs 

mkMoving :: PosMap -> ObMap -> [ObId]
mkMoving posMap obMap = map fst $ filter (\(_,ob)->isMoving posMap ob) $ Map.toList obMap

mkMoving2 :: [ObId] -> PosMap -> ObMap -> [ObId]
mkMoving2 moved posMap obMap 
 = map fst $ filter (\(obId,ob)-> ( not $ obId `elem` moved ) && (isMoving posMap ob)  ) $ Map.toList obMap

isMoving :: PosMap -> Ob -> Bool
isMoving _ (Ob ObFix  _ _ _ _) = False
isMoving _ (Ob ObFish _ _ _ _) = False
isMoving posMap (Ob t shape (sigsDown,_,_,_) pos _)
 = and [ isNothing r | r <- map (\sig-> Map.lookup (plus2D sig pos) posMap ) sigsDown  ]

isMovable :: [ObId] -> PosMap -> ObMap -> Ob -> Dir -> Bool
isMovable _ _ _ (Ob ObFix  _ _ _ _) _ = False
isMovable _ _ _ (Ob ObFish _ _ _ _) _ = False
isMovable okZavaz posMap obMap ob dir 
 = case (getZavazi posMap ob dir) \\ okZavaz of
  []    -> True
  zavaz -> and $ map (\z->isMovable (zavaz++okZavaz) posMap obMap (fromJust $ Map.lookup z obMap ) dir ) zavaz 


findMultiobjects :: Dir -> Sea -> [[ObId]]
findMultiobjects dir sea = map snd $ Map.toList $ f $ Map.toList $ findMultiobjects' dir sea
 where 
  f :: (Ord a) => [(a,a)] -> Map a [a]
  f xs = foldr (\ (obId,mulId) acc -> insertToListMap mulId obId acc ) Map.empty xs

findMultiobjects' :: Dir -> Sea -> MultiObMap
findMultiobjects' dir sea@(Sea _ obMap _ _ _ )
  = let rels = f $ findMultiobjects'' dir sea
        ff a acc = fromJust $ Map.lookup a acc
     in foldl (\acc (a,b)-> let mi = min (ff a acc) (ff b acc) in Map.insert a mi (Map.insert b mi acc) ) 
              (Map.fromList [ (oid,oid) | oid <- map fst $ Map.toList obMap ] ) 
              rels
 where
  f :: (Ord a) => [(a,a)] -> [(a,a)]
  f xs = sort $ map g xs
   where 
    g x@(a,b) = if a < b then x else (b,a)

findMultiobjects'' :: Dir -> Sea -> [(ObId,ObId)] -- MultiObMap
findMultiobjects'' dir (Sea posMap obMap moving rec _) 
  = nub $ concatMap (\ x -> holdedBy dir x posMap ) obs 
 where obs = Map.toAscList obMap


holdedBy :: Dir -> (ObId,Ob) -> PosMap -> [(ObId,ObId)]
holdedBy dir (obId,ob) posMap 
  = map (\oid->(oid,obId)) $ catMaybes $ map (\pos-> Map.lookup pos posMap ) $ getSignifs dir ob 

byDir :: Dir -> (a,a,a,a) -> a
byDir dir (d,u,r,l) = case dir of
 DDown -> d
 DUp -> u
 DRight -> r
 DLeft -> l

getZavazi :: PosMap -> Ob -> Dir -> [ObId]
getZavazi posMap (Ob _ _ sigs pos _ ) dir 
 = nub $ catMaybes $ map (\ p -> let pos' = plus2D p pos in Map.lookup pos' posMap ) $ byDir dir sigs
 

movingSignifs :: Shape -> [Pos]
movingSignifs shape
  = map (\(x,y)->(x,y-1)) $ movingSignifs' 0 [] $ toInts shape
 where
  movingSignifs' :: Int -> [Int] -> [[Int]] -> [Pos]
  movingSignifs' _ _ [] = []
  movingSignifs' y lst (act:rest) 
   = (zip (act \\ lst) (repeat y) ) ++ 
     (movingSignifs' (y+1) act rest)

mkSignifs :: Shape -> ([Pos],[Pos],[Pos],[Pos])
mkSignifs shape = ( getSigs DDown  ps ,
                    getSigs DUp    ps ,
                    getSigs DRight ps ,
                    getSigs DLeft  ps )
 where ps = toPoses shape  

getSignifs :: Dir -> Ob -> [Pos]
getSignifs dir (Ob _ _ signifs pos _ ) = map (`plus2D` pos) $ byDir dir signifs

dirDelta :: Dir -> Pos
dirDelta dir = case dir of
 DDown  -> ( 0,-1) 
 DUp    -> ( 0, 1)
 DRight -> ( 1, 0)
 DLeft  -> (-1, 0)
 
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

toInts :: [[HLine]] -> [[Int]]
toInts = map $ concatMap $ \ (s,f) -> [s..f] 

toPoses :: [[HLine]] -> [Pos]
toPoses shape = concat $ zipWith (\xs y -> map (\x->(x,y)) xs) (toInts shape) [0..] 


lvl0@(Sea l0_posMap l0_obMap l0_moving l0_rec l0_fid ) = mkSea [
 "                         ",
 "                         ",
 "        aaaa    11       ",
 "        b                ",
 "        cc$$       fff   ",
 "        d       eee  f   ",
 "                eee      ",
 "                         ",
 "                         ",
 "  $$$$$$$$$$$$$$$$$$$$$  ",
 "                         ",
 "                         "
 ]

lvl_0 = Map.toAscList l0_obMap

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
exSea@(Sea exPosMap exObMap exMoving exRectangle exFid) = mkSea ahoj
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


inChars :: [String] -> [Char]
inChars strs = (nub $ concat strs) \\ [' ']

showOb :: Ob -> String
showOb (Ob t sh (sd,su,sr,sl) pos px) 
 = "\n" ++ (showShape px sh)++ show pos ++ " " ++ (show t) 
   ++ (showPoses sd) ++ (showPoses su) ++ (showPoses sr)++ (showPoses sl)  

showPoses :: [Pos] -> String
showPoses [] = ""
showPoses ps 
  = (:) '\n' $ concatMap (\ps-> (map (\pos->case Map.lookup pos pmap of Nothing -> ' ' ; _ -> px) ps)++"\n" ) 
    [ [(x,y) | x <- [xMin..xMax] ] | y' <- [(-yMax)..(-yMin)] , let y = -y' ] 
 where 
  px = '#'
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

mkOb :: [String] -> Px -> Maybe Ob
mkOb strs px = do
 let sh = map (\str -> reverse $ f' $ foldl f [(0,0)] str) $ strs
 (pos,sh') <- shift sh
 return $ Ob obType sh' (mkSignifs sh') pos px
 where
  obType = case px of 
   '$' -> ObFix
   '1' -> ObFish 
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
   

plus2D :: Pos -> Pos -> Pos
plus2D (x,y) (a,b) = (x+a,y+b)