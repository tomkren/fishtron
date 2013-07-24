{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Eva 
( Eva
, runEva
, evalsWith
, evals
, setOutputBuffer
, sendJSON
, flushStdout
, evaSplitStdGen
) where


import System.Random       ( StdGen, getStdGen, split )
import Control.Monad.State ( StateT, runStateT, put, get, liftIO )
import Data.Typeable       ( Typeable )
import Data.Time.Clock     ( getCurrentTime, diffUTCTime )

import Text.JSON ( encode , JSValue )

import ServerInterface (jsEmptyObj, stdoutCmd, writeNextOutput ,OutputBuffer) 
import Heval (hevalsWith, hevals)
import Utils (Randable(..),Logable(..))


type Eva = StateT EvaState IO

type JobID = Int

data EvaState = EvaState {
     evaGen          :: StdGen ,
     evaOutputBuffer :: Maybe OutputBuffer,
     evaStdout       :: String
  }

runEva :: Eva a -> IO a
runEva eva = do
 gen <- getStdGen
 runEvaWith gen eva 

runEvaWith :: StdGen -> Eva a -> IO a
runEvaWith gen eva = do
 startTime <- getCurrentTime
 putStrLn $ "\nstdGen: " ++ show gen
 ( ret , evaState' ) <- runStateT eva (initEvaState gen)
 finishTime <- getCurrentTime
 putStrLn $ "Total time: " ++ show (diffUTCTime finishTime startTime) ++ "\n"
 return ret

initEvaState :: StdGen -> EvaState
initEvaState gen = EvaState { 
  evaGen          = gen,
  evaOutputBuffer = Nothing,
  evaStdout       = "" 
 }


setOutputBuffer :: OutputBuffer -> Eva ()
setOutputBuffer buff = do
  evaState <- get
  put $ evaState{ evaOutputBuffer = Just buff }


evalsWith :: (Typeable a) => String -> [String] -> a -> Eva [a]
evalsWith file strs as = liftIO $ hevalsWith file strs as

evals :: (Typeable a) => [String] -> a -> Eva [a]
evals strs as = liftIO $ hevals strs as


flushStdout :: Eva JSValue
flushStdout = do
  evaState <- get
  case evaOutputBuffer evaState of
    Nothing -> return jsEmptyObj
    Just _  -> do
      let stdout = evaStdout evaState
      put $ evaState{ evaStdout = "" }  
      return $ stdoutCmd stdout


sendJSON :: JSValue -> Eva ()
sendJSON json = do
  let jsonStr = encode json
  evaState <- get
  case evaOutputBuffer evaState of
    Nothing    -> return ()
    Just buff  -> liftIO $ writeNextOutput buff jsonStr


evaSplitStdGen :: Eva StdGen
evaSplitStdGen = do
  evaState <- get
  let gen0        = evaGen evaState 
      (gen1,gen2) = split gen0
  put $ evaState{ evaGen = gen1 }
  return gen2

instance Randable Eva where
 randLift f = do
  evaState <- get
  let gen        = evaGen evaState
      (val,gen') = f gen
  put $ evaState{ evaGen = gen' }
  return val 


instance Logable Eva where
 logIt2 str = liftIO . putStrLn $ str
 logIt  str = do
  liftIO . putStrLn $ str
  evaState <- get
  case evaOutputBuffer evaState of
    Nothing -> return ()
    Just _  -> do
      let stdout  = evaStdout evaState
          stdout' = stdout ++ "\n" ++ str 
      put $ evaState{ evaStdout = stdout' }