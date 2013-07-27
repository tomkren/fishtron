{-# LANGUAGE DeriveDataTypeable #-}
module Problems.Ant2.Problem where


import qualified Data.Set as Set
import Data.Set (Set)
import Data.Typeable

data Ant = L | R | M | IFA Ant Ant | P2 Ant Ant | P3 Ant Ant Ant deriving (Typeable)

data World = World Size Limit (AntPos,Dir) Stats ( Set.Set Pos ) ( Set.Set Pos ) 


type Size  = (Int,Int)
type Limit = Int
type Pos   = (Int,Int)
type AntPos= Pos
data Dir   = DUp | DDown | DLeft | DRight
data Action= ALeft | ARight | AMove
type Steps = Int
type Eaten = Int
type Stats = (Eaten,Steps)


instance Show World where show = showWorld





winner = IFA M (P3 L (P2 (IFA M R) (P2 R (P2 L R) ) ) (P2 (IFA M L) M) )

--inner2 = ifFoodAhead doMove 
--                    (progn3 doLeft 
--                            (progn2 (ifFoodAhead doMove doRight )
--                                    doRight ) 
--                            (progn2 (ifFoodAhead doMove doLeft) doMove) )



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
isFoodAhead (World size _ (antPos,antDir) _ _ set ) = Set.member (moveForward size antDir antPos) set

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
                (isThere,foodNum) = if Set.member antPos' set then (True,1) else (False,0)
             in World size limit (antPos', antDir) (eaten+foodNum,steps+1) (Set.insert antPos path) $ 
                if isThere then Set.delete antPos' set else set

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
mkWorld limit w = World (length w, maximum $ map length w ) limit ((1,1),DRight) (0,0) Set.empty $ 
            foldr (\ (ch,pos) acc-> if ch == 'F' then Set.insert pos acc else acc ) Set.empty w'
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

showWorld :: World -> String
showWorld (World (iMax,jMax) limit (antPos,dir) (eaten,steps) path set) 
  = (concatMap (\pos@(_,j) -> (if j==1 then "\n" else [] )++
                             (if pos == antPos then (showDir dir)++" " 
                              else if Set.member pos set  then "F " 
                              else if Set.member pos path then "@ " else ". ")  ) 
               [(i,j)|i<-[1..iMax],j<-[1..jMax]])++"\n"++
    "\nfood eaten        : " ++ show eaten ++ 
    "\nactions performed : " ++ show steps ++ 
    "\nlimit             : " ++ show limit ++ "\n"
  where showDir d = case d of DUp -> "A" ; DDown -> "V" ; DLeft -> "<" ; DRight -> ">"
