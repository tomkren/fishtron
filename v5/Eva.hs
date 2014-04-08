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
, amIStillAlive
, resetAfterStop

, eva_getPopInfo
, eva_resetPopInfo 
, eva_inc_xo_ok,eva_inc_xo_tooBig,eva_inc_xo_fail,eva_inc_rep,eva_inc_elite 
, eva_popi_setBest 
, eva_popi_setPop
, eva_popi_addXOL
, eva_nextRC

) where


import System.Random       ( StdGen, getStdGen, split )
import Control.Monad.State ( StateT, runStateT, put, get, liftIO )
import Data.Typeable       ( Typeable )
import Data.Time.Clock     ( getCurrentTime, diffUTCTime )

import Text.JSON ( encode , JSValue )

import ServerInterface (jsEmptyObj, stdoutCmd, writeNextOutput ,OutputBuffer,ProcessData(..),isKilled,resetProcessData) 
import Heval (hevalsWith, hevals)
import Utils (Randable(..),Logable(..),JShow)

import PopulationInfo (PopInfo(..), initPopInfo, resetPopInfo,
                       inc_xo_ok, inc_xo_tooBig, inc_xo_fail, inc_rep, inc_elite, inc_nextRC,
                       popi_setBest, popi_setPop, XOLType, popi_addXOL)

type Eva = StateT EvaState IO

type JobID = Int

data EvaState = EvaState {
     evaGen          :: StdGen ,
     evaOutputBuffer :: Maybe OutputBuffer,
     evaStdout       :: String,
     evaPopInfo      :: PopInfo
  }


-- PopInfo stuff -------

eva_getPopInfo :: Eva PopInfo
eva_getPopInfo = do
  evaState <- get
  return $ evaPopInfo evaState

eva_resetPopInfo :: Eva ()
eva_resetPopInfo = do
  evaState <- get
  put $ evaState{ evaPopInfo = resetPopInfo $ evaPopInfo evaState }

eva_nextRC :: Eva Int
eva_nextRC = do
  evaState <- get
  let rc = popi_nextRC $ evaPopInfo evaState
  eva_popi_update inc_nextRC
  return rc


eva_popi_addXOL :: (JShow term) => (XOLType,XOLType) -> [term] -> ([Int],[Int]) -> Eva ()
eva_popi_addXOL xolt xs poses = eva_popi_update (popi_addXOL xolt xs poses)

eva_popi_setPop  :: (JShow term) => [(term,Double)] -> Eva ()
eva_popi_setPop xs = eva_popi_update (popi_setPop xs)

eva_popi_setBest :: (JShow term) => term -> Double -> Eva ()
eva_popi_setBest term fitVal = eva_popi_update (popi_setBest term fitVal) 

-- :: Eva ()
eva_inc_xo_ok     = eva_popi_update inc_xo_ok
eva_inc_xo_tooBig = eva_popi_update inc_xo_tooBig
eva_inc_xo_fail   = eva_popi_update inc_xo_fail
eva_inc_rep       = eva_popi_update inc_rep
eva_inc_elite     = eva_popi_update inc_elite

eva_popi_update :: (PopInfo -> PopInfo) -> Eva ()
eva_popi_update f = do
  evaState <- get
  put $ evaState{ evaPopInfo = f $ evaPopInfo evaState }  


------------------------



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
  evaStdout       = "" ,
  evaPopInfo      = initPopInfo
 }

resetAfterStop :: Eva ()
resetAfterStop = do
   evaState <- get
   case evaOutputBuffer evaState of
    Nothing   -> return ()
    Just buff -> liftIO . resetProcessData $ buff
  

amIStillAlive :: Eva Bool
amIStillAlive = do
   evaState <- get
   case evaOutputBuffer evaState of
    Nothing   -> return True
    Just buff -> do
      isDead <- liftIO $ isKilled buff
      return . not $ isDead


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