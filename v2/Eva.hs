{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}

module Eva where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.Random
import Control.Monad.State
import Data.Time.Clock
import Data.Typeable

import Text.JSON ( encode , JSValue )

import Utils
import Heval

import ServerInterface


type Eva = StateT StdGen ( StateT Stats IO ) 

class ( Randable m , Logable m  ) => EvaMonad m where 
 eval      :: (Typeable a) =>  String  -> a -> m  a
 evals     :: (Typeable a) => [String] -> a -> m [a]
 evalsWith :: (Typeable a) => String -> [String] -> a -> m [a]
 statIt    :: StatRecord -> m ()

instance EvaMonad Eva where 
 eval  str  as          = liftIO $ heval  str  as
 evals strs as          = liftIO $ hevals strs as
 evalsWith file strs as = liftIO $ hevalsWith file strs as
 statIt = statIt_
 --statIt sr = do
 -- stats <- lift get
 -- lift . put $ (sr:stats) 

instance Logable Eva where
 --logIt = liftIO . putStrLn
 logIt2 str = liftIO . putStrLn $ str

 logIt str = do
  liftIO . putStrLn $ str
  (stats,strs) <- lift get
  case Map.lookup "jobID" strs of
    Nothing -> return ()
    Just jobID -> do
      let stdout = case Map.lookup "stdout" strs of
                     Nothing -> []
                     Just s  -> s
          strs'  = Map.insert "stdout" ( stdout ++ "\n" ++ str) strs
      lift . put $ ( stats , strs' )

    -- case Map.lookup "stdout" strs of
    --  Nothing -> do
    --    let strs' = Map.insert "stdout" str strs
    --    lift . put $ ( stats , strs' )
    --  Just stdout -> do
    --    let strs' = Map.insert "stdout" ( stdout ++ "\n" ++ str) strs
        
 --logIt str = do
 -- (_,strs) <- lift get
 -- case Map.lookup "jobID" strs of
 --   Nothing    -> liftIO . putStrLn $ str
 --   Just jobID -> do
 --     liftIO $ writeNextOutput (read jobID) (encode $ stdoutCmd str)
 --     liftIO . putStrLn $ str 

flushStdout :: Eva JSValue
flushStdout = do
  (stats,strs) <- lift get
  case Map.lookup "jobID" strs of
    Nothing -> return jsEmptyObj
    Just jobID -> do
      let stdout = case Map.lookup "stdout" strs of
                     Nothing -> []
                     Just s  -> s
          strs'  = Map.insert "stdout" [] strs
      lift . put $ ( stats , strs' )  
      return $ stdoutCmd stdout
      

sendJSON :: JSValue -> Eva ()
sendJSON json = do
  let jsonStr = encode json
  (_,strs) <- lift get
  case Map.lookup "jobID" strs of
    Nothing -> 
      --liftIO . putStrLn $ jsonStr
      return ()
    Just jobID -> do
      liftIO $ writeNextOutput (read jobID) jsonStr
      --liftIO . putStrLn $ jsonStr
  

evaSplitStdGen :: Eva StdGen
evaSplitStdGen = do
  gen0 <- get
  let (gen1,gen2) = split gen0
  put gen1
  return gen2

instance Randable Eva where
 randLift f = do
  gen <- get
  let (val,gen') = f gen
  put gen'
  return val 

runEva :: Eva a -> IO (a,Stats)
runEva eva = do
 gen <- getStdGen
 runEvaWith gen eva 

runEvaWith :: StdGen -> Eva a -> IO (a,Stats)
runEvaWith gen eva = do
 startTime <- getCurrentTime
 putStrLn $ "\nstdGen: " ++ show gen
 ((ret,gen'),stats) <- runStateT (runStateT eva gen) (Map.empty , Map.empty) 
 finishTime <- getCurrentTime
 putStrLn $ "Total time: " ++ show (diffUTCTime finishTime startTime) ++ "\n"
 return (ret,stats)

-- Stating stuff --------------------------------------

type RunID = Int
type GenID = Int
data GenInfoType = BestOfGen | AvgOfGen | WorstOfGen deriving (Eq,Ord,Show)

type GenInfos = Map GenInfoType Double
type RunInfos = Map GenID GenInfos
type Stats    = ( Map RunID RunInfos , Map String String )

data StatRecord = 
 GenInfo RunID GenID GenInfoType Double |
 StrInfo String String

statIt_ :: StatRecord -> Eva () --RunID -> GenID -> GenInfoType -> Double -> Eva ()
statIt_ sr = do 
 (stats,strs) <- lift get
 case sr of
  GenInfo runID genID git val -> lift . put $ ( statIt' stats runID genID git val , strs )
  StrInfo key val             -> lift . put $ ( stats , Map.insert key val strs )  

statIt' :: Map RunID RunInfos -> RunID -> GenID -> GenInfoType -> Double -> Map RunID RunInfos
statIt' stats runID genID git val = case Map.lookup runID stats of
 Nothing -> Map.insert runID ( Map.singleton genID ( Map.singleton git val ) ) stats
 Just runInfos -> case Map.lookup genID runInfos of
  Nothing -> Map.insert runID ( Map.insert genID ( Map.singleton git val ) runInfos ) stats
  Just genInfos -> case Map.lookup git genInfos of
   Nothing -> Map.insert runID ( Map.insert genID ( Map.insert git val genInfos ) runInfos ) stats
   Just _  -> error "statIt': rewriting statistical value should be illegal"




-- testSt = statIt' Map.empty 1 0 BestOfGen 100.1





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

