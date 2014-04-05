module Problems.Ant2.Funs where


data Ant = L | R | M | IFA Ant Ant | P2 Ant Ant | P3 Ant Ant Ant 

data World = World Size Limit (AntPos,Dir) Stats [Pos] [Pos] 


type Size  = (Int,Int)
type Limit = Int
type Pos   = (Int,Int)
type AntPos= Pos
data Dir   = DUp | DDown | DLeft | DRight
data Action= ALeft | ARight | AMove
type Steps = Int
type Eaten = Int
type Stats = (Eaten,Steps)




ff :: Ant -> (Double,Bool)
ff ant =
 let w = runAnt antWorld ant
     eaten = fromIntegral $ score w
  in ( 1.0 / ( 1.0 + (89.0 - eaten) ) , eaten == 89 )

runAnt :: World -> Ant -> World
runAnt w ant 
 | remain w > 0 = runAnt (stepAnt w ant) ant
 | otherwise    = w 
          

stepAnt :: World -> Ant -> World
stepAnt w1 ant = case ant of
 L -> action w1 ALeft
 R -> action w1 ARight
 M -> action w1 AMove
 IFA a1 a2 | isFoodAhead w1 -> stepAnt w1 a1
           | otherwise      -> stepAnt w1 a2 
 P2  a1 a2    -> let w2 = stepAnt w1 a1
                  in stepAnt w2 a2
 P3  a1 a2 a3 -> let w2 = stepAnt w1 a1
                     w3 = stepAnt w2 a2
                  in stepAnt w3 a3


-- -------------------------------------------------


isFoodAhead :: World -> Bool
isFoodAhead (World size _ (antPos,antDir) _ _ set ) = elem (moveForward size antDir antPos) set

remain :: World -> Int
remain (World _ limit _ (_,steps) _ _) = limit - steps

score :: World -> Int
score (World _ _ _ (eaten,_) _ _) = eaten

action :: World -> Action -> World
action w@(World size limit (antPos,antDir) (eaten,steps) path set) a 
 = if steps >= limit then w else case a of
  ALeft  -> World size limit (antPos, turnLeft  antDir) (eaten,steps+1) path set 
  ARight -> World size limit (antPos, turnRight antDir) (eaten,steps+1) path set
  AMove  -> let antPos' = moveForward size antDir antPos
                (isThere,foodNum) = if elem antPos' set then (True,1) else (False,0)
             in World size limit (antPos', antDir) (eaten+foodNum,steps+1) (antPos:path) $ 
                if isThere then (delete antPos' set) else set



delete                  :: (Eq a) => a -> [a] -> [a]
delete                  =  deleteBy (==)

deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _  _ []        = []
deleteBy eq x (y:ys)    = if x `eq` y then ys else y : deleteBy eq x ys



moveForward :: Size -> Dir -> Pos -> Pos
moveForward (iMax,jMax) dir (i,j) = case dir of
  DUp    -> if i == 1    then (iMax,j   ) else (i-1,j  )
  DLeft  -> if j == 1    then (i   ,jMax) else (i  ,j-1)
  DDown  -> if i == iMax then (1   ,j   ) else (i+1,j  )
  DRight -> if j == jMax then (i   ,1   ) else (i  ,j+1)

turnLeft :: Dir -> Dir
turnLeft dir = case dir of
  DUp    -> DLeft
  DLeft  -> DDown
  DDown  -> DRight
  DRight -> DUp

turnRight :: Dir -> Dir
turnRight dir = case dir of
  DUp    -> DRight
  DLeft  -> DUp
  DDown  -> DLeft
  DRight -> DDown


mkWorld :: Int -> [[Char]] -> World
mkWorld limit w = World (length w, maximum $ map length w ) limit ((1,1),DRight) (0,0) [] $ 
            foldr (\ (ch,pos) acc-> if ch == 'F' then (pos:acc) else acc ) [] w'
  where w'= concat $ map (\(row,i)-> zip row (map (\j->(i,j)) [1..] ) ) $ zip w [1..]

antWorld = mkWorld 600 
  [ ".FFF............................"
  , "...F............................"
  , "...F.....................FFF...." 
  , "...F....................F....F.." 
  , "...F....................F....F.." 
  , "...FFFF.FFFFF........FF........." 
  , "............F................F.." 
  , "............F.......F..........." 
  , "............F.......F..........." 
  , "............F.......F........F.." 
  , "....................F..........." 
  , "............F..................." 
  , "............F................F.." 
  , "............F.......F..........." 
  , "............F.......F.....FFF..." 
  , ".................F.....F........" 
  , "................................" 
  , "............F..................."             
  , "............F...F.......F......."
  , "............F...F..........F...." 
  , "............F...F..............."             
  , "............F...F..............."
  , "............F.............F....." 
  , "............F..........F........" 
  , "...FF..FFFFF....F..............." 
  , ".F..............F..............." 
  , ".F..............F..............." 
  , ".F......FFFFFFF................." 
  , ".F.....F........................" 
  , ".......F........................" 
  , "..FFFF.........................." 
  , "................................" ]