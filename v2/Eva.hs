{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}

module Eva where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.Random
import Control.Monad.State
import Data.Time.Clock
import Data.Typeable
import System.Directory

import Utils
import Heval

type Eva = StateT StdGen ( StateT Stats IO ) 

class ( Randable m , Logable m  ) => EvaMonad m where 
 eval   :: (Typeable a) =>  String  -> a -> m  a
 evals  :: (Typeable a) => [String] -> a -> m [a]
 statIt :: StatRecord -> m ()

instance EvaMonad Eva where 
 eval  str  as = liftIO $ heval  str  as
 evals strs as = liftIO $ hevals strs as
 statIt = statIt_
 --statIt sr = do
 -- stats <- lift get
 -- lift . put $ (sr:stats) 

instance Logable Eva where
 logIt = liftIO . putStrLn 

instance Randable Eva where
 randLift f = do
  gen <- get
  let (val,gen') = f gen
  put gen'
  return val 

runEva :: Eva a -> IO a
runEva eva = do
 gen <- getStdGen
 runEvaWith gen eva 

runEvaWith :: StdGen -> Eva a -> IO a
runEvaWith gen eva = do
 startTime <- getCurrentTime
 putStrLn $ "\nstdGen: " ++ show gen
 ((ret,gen'),stats) <- runStateT (runStateT eva gen) (Map.empty,Map.empty) 
 --writeFile "graph.txt" (showStats stats)
 writeStats stats
 finishTime <- getCurrentTime
 putStrLn $ "Total time: " ++ show (diffUTCTime finishTime startTime) ++ "\n"
 return ret

-- Stating stuff --------------------------------------

type RunID = Int
type GenID = Int
data GenInfoType = BestOfGen | AvgOfGen | WorstOfGen deriving (Eq,Ord,Show)

type GenInfos = Map GenInfoType Double
type RunInfos = Map GenID GenInfos
type Stats    = (Map RunID RunInfos,Map String String)

data StatRecord = 
 GenInfo RunID GenID GenInfoType Double |
 StrInfo String String

statIt_ :: StatRecord -> Eva () --RunID -> GenID -> GenInfoType -> Double -> Eva ()
statIt_ sr = do 
 (mapa1,mapa2) <- lift get
 case sr of
  GenInfo runID genID git val -> lift . put $ ( statIt' mapa1 runID genID git val , mapa2 )
  StrInfo key val             -> lift . put $ ( mapa1 , Map.insert key val mapa2 )

statIt' :: Map RunID RunInfos -> RunID -> GenID -> GenInfoType -> Double -> Map RunID RunInfos
statIt' stats runID genID git val = case Map.lookup runID stats of
 Nothing -> Map.insert runID ( Map.singleton genID ( Map.singleton git val ) ) stats
 Just runInfos -> case Map.lookup genID runInfos of
  Nothing -> Map.insert runID ( Map.insert genID ( Map.singleton git val ) runInfos ) stats
  Just genInfos -> case Map.lookup git genInfos of
   Nothing -> Map.insert runID ( Map.insert genID ( Map.insert git val genInfos ) runInfos ) stats
   Just _  -> error "statIt': rewriting statistical value should be illegal"

getStat :: Map RunID RunInfos -> RunID -> GenID -> GenInfoType -> Double
getStat stats runID genID git = case Map.lookup runID stats of
 Just runInfos -> case Map.lookup genID runInfos of
  Just genInfos -> case Map.lookup git genInfos of
   Just value -> value

showStats :: Map RunID RunInfos -> [(RunID,String)]
showStats stats = 
  [ ( runID , oneRunGraphStr ++ 
              oneDataStream runInfos BestOfGen ++
              oneDataStream runInfos AvgOfGen  ++
              oneDataStream runInfos WorstOfGen ) | 
    (runID,runInfos) <- Map.toAscList stats ] 

oneDataStream :: RunInfos -> GenInfoType -> String
oneDataStream runInfos git 
 = ( concat [ show genID++" "++show val++"\n" | 
              (genID,genInfos) <- Map.toAscList runInfos ,
              let Just val = Map.lookup git genInfos ] ) ++ "e\n"

writeStats :: Stats -> IO ()
writeStats (stats,strStats) = do
  isThere <- doesDirectoryExist problemName 
  problemName' <- if isThere then checkFreeName 2 problemName else return problemName
  createDirectory problemName' 
  setCurrentDirectory problemName'
  createDirectory dirName
  setCurrentDirectory ".."  
  mapM_ ( \ (runID,str) -> writeFile ( problemName' ++ "/" ++ dirName ++"/" ++ show runID ++ "-graph.gpl") str ) (showStats stats)
 where 
  Just problemName = Map.lookup "problemName" strStats
  dirName = "eachGene"


checkFreeName :: Int -> String -> IO String
checkFreeName i base = 
  let tryName = (base++"-"++show i)
   in do 
    itExist <- doesDirectoryExist tryName 
    if itExist
    then checkFreeName (i+1) base 
    else return tryName 

testSt = statIt' Map.empty 1 0 BestOfGen 100.1

-- type Stats_ = [StatRecord]
-- 
-- data StatRecord = 
--  SR_Best  Int Double |
--  SR_Avg   Int Double |
--  SR_Worst Int Double
--  deriving (Show)
-- 
-- showStats_ :: [StatRecord] -> String
-- showStats_ rs = concat [ show i ++ " " ++ (intercalate " " (map show ds)) ++ "\n" | (i,ds) <- rows ] 
--  where 
--   rows :: [(Int,[Double])]
--   rows = Map.toAscList $ foldr f Map.empty rs
--   f :: StatRecord -> Map Int [Double] -> Map Int [Double]
--   f r mapa = case r of
--    SR_Best  x y -> Map.insertWith (\[y,_,_] [_,y2,y3]->[y,y2,y3]) x [y,undefined,undefined] mapa
--    SR_Avg   x y -> Map.insertWith (\[_,y,_] [y1,_,y3]->[y1,y,y3]) x [undefined,y,undefined] mapa
--    SR_Worst x y -> Map.insertWith (\[_,_,y] [y1,y2,_]->[y1,y2,y]) x [undefined,undefined,y] mapa


oneRunGraphStr :: String
oneRunGraphStr = 
 "plot \'-\' title 'best'  with linespoints ,\\\n" ++
 "     \'-\' title 'avg'   with linespoints ,\\\n" ++
 "     \'-\' title 'worst' with linespoints\n"



--------------------------------------

testEva :: Eva ([Int],Int,Double)
testEva = do
  statIt $ StrInfo "problemName" "XXX"
  logIt "zacatek"
  xs<- evals ["666","6+8","(\\x->x+100)1"] (1::Int) 
  y <- getRandomL [1..100]
  z <- getNormal (0,1)
  boxThem [ "mam tyto skvele ulovky" , show xs , show y , show z]
  statIt $ GenInfo 1 0 BestOfGen 0.2
  statIt $ GenInfo 1 0 AvgOfGen 0.1
  statIt $ GenInfo 1 0 WorstOfGen 0.002
   
  statIt $ GenInfo 1 1 BestOfGen 0.3
  statIt $ GenInfo 1 1 AvgOfGen 0.2
  statIt $ GenInfo 1 1 WorstOfGen 0.1
 
  statIt $ GenInfo 2 0 BestOfGen 0.25
  statIt $ GenInfo 2 0 AvgOfGen 0.15
  statIt $ GenInfo 2 0 WorstOfGen 0.0025
   
  statIt $ GenInfo 2 1 BestOfGen 0.35
  statIt $ GenInfo 2 1 AvgOfGen 0.25
  statIt $ GenInfo 2 1 WorstOfGen 0.15

  logIt "konec"
  return (xs,y,z)

testEva2 :: (EvaMonad m) => m ([Int],Int,Double)
testEva2 = do
  logIt "zacatek"
  xs<- evals ["666","6+8","(\\x->x+100)1"] (1::Int) 
  y <- getRandomL [1..100]
  z <- getNormal (0,1)
  boxThem [ "mam tyto skvele ulovky" , show xs , show y , show z]
  --statIt $ SR_Best 1 1.25
  --statIt $ SR_Avg  1 0.3
  --statIt $ SR_Worst 1 0.01
  --statIt $ SR_Best 2 1.28
  --statIt $ SR_Avg  2 0.4
  --statIt $ SR_Worst 2 0.001
  logIt "konec"
  return (xs,y,z)

