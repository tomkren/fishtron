--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE TypeSynonymInstances #-}

-- Problém s mravencem a jídlem z Kozy

module Ant (
 AAnt, toAAnt , toAAnt2 , toAAnt3, unAAnt , 
 Ant , World , antWorld , evalAnt , runAnt,
 doLeft, doRight , doMove , ifFoodAhead , progn2 , progn3
) where 

import qualified Data.Set as Set
import Control.Monad.State

--import Data.Typeable
--import Data.Dynamic
--import Data.Functor.Identity
--import Data.Maybe
--import System.Random
--
--import Base
--import Util
--import Dist
--import Evaluation
--import Parser
--import GP_old
--import Enviroments
--import Prover

type Size  = (Int,Int)
type Limit = Int
type Pos   = (Int,Int)
type AntPos= Pos
data Dir   = DUp | DDown | DLeft | DRight
data Action= ALeft | ARight | AMove
data World = World Size Limit (AntPos,Dir) Stats ( Set.Set Pos ) ( Set.Set Pos ) --deriving (Show)

type Steps = Int
type Eaten = Int
type Stats = (Eaten,Steps)

type Ant = State World ()

newtype AAnt = AAnt { unAAnt :: Ant } 

toAAnt :: Ant -> AAnt
toAAnt ant = AAnt ant

toAAnt2 :: (Ant -> Ant -> Ant) -> (AAnt -> AAnt -> AAnt)
toAAnt2 f aa1 aa2 = AAnt $ f (unAAnt aa1) (unAAnt aa2) 

toAAnt3 :: (Ant -> Ant -> Ant -> Ant) -> (AAnt -> AAnt -> AAnt -> AAnt)
toAAnt3 f aa1 aa2 aa3 = AAnt $ f (unAAnt aa1) (unAAnt aa2) (unAAnt aa3) 


---------data ANT = ANT (State World ()) deriving Typeable

--ant :: Typ
--ant  = Typ "Ant"
--
--
--instance Typeable Ant where
--  typeOf _ = mkTyConApp (mkTyCon "Ant") []
--
-- 
--antEnv :: Env
--antEnv =  [
--          ( "l"   , ant                         ,  toDyn' doLeft )
--          ,("r"   , ant                         ,  toDyn' doRight )
--          ,("m"   , ant                         ,  toDyn' doMove )
--          ,("ifa" , ant :-> ant :-> ant         ,  toDyn' ifFoodAhead )
--          ,("p2"  , ant :-> ant :-> ant         ,  toDyn' progn2 )
--          ,("p3"  , ant :-> ant :-> ant :-> ant ,  toDyn' progn3 )
--          ]
--
--ffAnt :: TTerm -> FitVal
--ffAnt ttant =  case (compute 100 $ ttant )::Either TTerm Ant of
--  Left  _   -> 0
--  Right ant -> fromIntegral $ evalAnt w ant
--
--runTTAnt :: TTerm -> World
--runTTAnt ttant = case (compute 100 $ ttant )::Either TTerm Ant of
--  Left  _   -> w
--  Right ant -> runAnt w ant
--
--{-- --}
--
--
--testCaseAnt = map (runTTAnt.fst.fromJust.distMax) $ gp (mkStdGen 4242424) (dk_ antEnv) ant ffAnt 500 10
--
--
--test = runAnt w winner
--
winner = ifFoodAhead doMove 
                     (progn3 doLeft 
                             (progn2 (ifFoodAhead doMove doRight )
                                     (progn2 doRight (progn2 doLeft doRight)  ) ) 
                             (progn2 (ifFoodAhead doMove doLeft) doMove) )

winner2 = ifFoodAhead doMove 
                     (progn3 doLeft 
                             (progn2 (ifFoodAhead doMove doRight )
                                     doRight ) 
                             (progn2 (ifFoodAhead doMove doLeft) doMove) )


evalAnt :: World -> Ant -> Int
evalAnt w ant = score $ runAnt w ant 

runAnt :: World -> Ant -> World
runAnt w ant = snd $ runState (untilLimit ant) w

untilLimit :: Ant -> Ant
untilLimit ant = do
  rem <- getRemain
  if rem > 0 then do ant ; untilLimit ant else doNOP 

--runAnt' :: World -> Int -> Ant -> World
--runAnt' w n ant = snd $ runState (doNTimes n ant) w
--
--doNTimes :: Int -> Ant -> Ant
--doNTimes 0 ant = state $ \ w -> ( ()  , w )
--doNTimes n ant = do ant ; doNTimes (n-1) ant

doNOP :: Ant
doNOP = state $ \ w -> ( ()  , w )

doLeft , doRight , doMove :: Ant
doLeft  = doAction ALeft
doRight = doAction ARight
doMove  = doAction AMove

ifFoodAhead :: Ant -> Ant -> Ant
ifFoodAhead ant1 ant2 = do 
  ifa <- isFoodAhead'
  if ifa then ant1 else ant2

progn2 :: Ant -> Ant -> Ant
progn2 ant1 ant2 = do ant1 ; ant2

progn3 :: Ant -> Ant -> Ant -> Ant
progn3 ant1 ant2 ant3 = do ant1 ; ant2 ; ant3

doAction :: Action -> Ant
doAction a = state $ \ w -> ( () , action w a )

getRemain :: State World Int
getRemain = state $ \ w -> ( remain w  , w )

isFoodAhead' :: State World Bool
isFoodAhead' = state $ \ w -> ( isFoodAhead w  , w )

isFoodAhead :: World -> Bool
isFoodAhead (World size _ (antPos,antDir) _ _ set ) = Set.member (moveForward size antDir antPos) set


instance Show World where show = showWorld

mkWorld :: Int -> [[Char]] -> World
mkWorld limit w = World (length w, maximum $ map length w ) limit ((1,1),DRight) (0,0) Set.empty $ 
            foldr (\ (ch,pos) acc-> if ch == 'F' then Set.insert pos acc else acc ) Set.empty w'
  where w'= concat $ map (\(row,i)-> zip row (map (\j->(i,j)) [1..] ) ) $ zip w [1..]

actions :: World -> [Action] -> World
actions = foldl action 

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

----isFood :: World -> Pos -> Bool
----isFood (World _ _ _ _ _ set) pos = Set.member pos set

remain :: World -> Int
remain (World _ limit _ (_,steps) _ _) = limit - steps

score :: World -> Int
score (World _ _ _ (eaten,_) _ _) = eaten


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

