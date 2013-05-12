module Job ( problemList_ , job_ ) where

import GP_Core ( nRunsByServer )
import Text.JSON
import JSONUtils
import ProblemUtils ( ProblemOpts , po2json, poCode, json2po , runProblemOpts )
import Data.Typeable ( Typeable )
import Data.Map (Map)
import qualified Data.Map as Map



import qualified Problems.SSR.Problem        as SSR
import qualified Problems.BigCtx.Problem     as BigCtx
import qualified Problems.Fly.Problem        as Fly
import qualified Problems.Ant.Problem        as Ant
import qualified Problems.BA.Problem         as BA
import qualified Problems.EvenParity.Problem as EvenParity

regs = 
 [ reg EvenParity.reg
 , reg BigCtx.reg
 , reg Fly.reg 
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



