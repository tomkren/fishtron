module Job ( problemList_ , job_ , job_new, problemList_new ) where

import GP_Core ( nRunsByServer )
import Text.JSON
import JSONUtils
import ProblemUtils ( ProblemOpts , po2json, poCode, json2po , runProblemOpts, runProblemOpts_new )
import Data.Typeable ( Typeable )
import Data.Map (Map)
import qualified Data.Map as Map

import ServerInterface ( OutputBuffer ) 



import qualified Problems.SSR.Problem        as SSR
import qualified Problems.BigCtx.Problem     as BigCtx
import qualified Problems.Fly.Problem        as Fly
import qualified Problems.Fly02.Problem      as Fly02
import qualified Problems.Ant.Problem        as Ant
import qualified Problems.BA.Problem         as BA
import qualified Problems.EvenParity.Problem as EvenParity


regs_new = 
 [ reg_new Fly02.reg
 , reg_new Fly.reg 
 , reg_new BigCtx.reg_head
 , reg_new EvenParity.reg
 , reg_new BigCtx.reg
 , reg_new SSR.reg 
 , reg_new Ant.reg
 , reg_new (BA.reg :: POU) 
 ]



job_new :: OutputBuffer -> String -> IO ()
job_new buff cmd = case ( decode cmd :: Result JSValue ) of
  Error str  -> do
    putStrLn $ "ERROR in job_new : " ++ str
    putStrLn $ cmd
  Ok jsvalue -> do
    let code = fromJsStr $ jsProp jsvalue "code" 
        Just runFun = Map.lookup code problemTab_new 
    putStrLn code
    runFun jsvalue buff

problemList_new :: JSValue
problemList_new = jsArr $ snd jobs_new

jobs_new :: ( [ (Code , JSValue -> OutputBuffer -> IO()) ] , [JSValue] )
jobs_new = unzip regs_new

reg_new :: Typeable a => ProblemOpts a -> ((String, JSValue -> OutputBuffer -> IO ()), JSValue)
reg_new po = ( (poCode po , \ jsv buff -> runProblemOpts_new (json2po po jsv) buff ) , po2json po )


problemTab_new :: Map Code ( JSValue -> OutputBuffer -> IO () )
problemTab_new  = Map.fromList $ fst jobs_new 






regs = 
 [ reg Fly02.reg
 , reg Fly.reg 
 , reg BigCtx.reg_head
 , reg EvenParity.reg
 , reg BigCtx.reg
 , reg SSR.reg 
 , reg Ant.reg
 , reg (BA.reg :: POU) 
 ]


problemList_ :: JSValue
problemList_ = jsArr $ snd jobs

job_ :: JobID -> String -> IO ()
job_ jobID cmd = case ( decode cmd :: Result JSValue ) of
  Error str  -> putStrLn $ "ERROR in job_ : " ++ str
  Ok jsvalue -> do
    let code = fromJsStr $ jsProp jsvalue "code" 
        Just runFun = Map.lookup code problemTab 
    putStrLn code
    runFun jsvalue jobID 

 

type JobID = String
type Code  = String
type POU   = ProblemOpts ()

reg :: Typeable a => ProblemOpts a -> ((String, JSValue -> JobID -> IO ()), JSValue)
reg po = ( (poCode po , \ jsv jobID -> runProblemOpts (json2po po jsv) jobID ) , po2json po )

jobs :: ( [ (Code , JSValue -> JobID -> IO()) ] , [JSValue] )
jobs = unzip regs


problemTab :: Map Code ( JSValue -> JobID -> IO () )
problemTab  = Map.fromList $ fst jobs 



