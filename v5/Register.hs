module Register ( job, problemList ) where

import Text.JSON (decode, Result(..), JSValue )
import JSONUtils (fromJsStr,jsProp,jsArr)
import ProblemUtils ( ProblemOpts , po2json, poCode, json2po , runProblemOpts )
import Data.Typeable ( Typeable )
import Data.Map (Map)
import qualified Data.Map as Map (lookup, fromList)

import ServerInterface ( OutputBuffer ) 



import qualified Problems.SSR.Problem        as SSR
import qualified Problems.BigCtx.Problem     as BigCtx
import qualified Problems.Fly.Problem        as Fly
import qualified Problems.Fly02.Problem      as Fly02
import qualified Problems.Ant.Problem        as Ant
import qualified Problems.Ant2.Problem_      as Ant2
import qualified Problems.BA.Problem         as BA
import qualified Problems.EvenParity.Problem as EvenParity


regs = 
 [ reg SSR.reg
 , reg EvenParity.rege 
 , reg SSR.rege 
 , reg Ant.reg
 , reg EvenParity.reg
 , reg Fly02.reg
 , reg BigCtx.reg
 , reg (BA.reg :: POU) 
 --, reg BigCtx.reg_head
 -- , reg Fly.reg 
 -- ,  reg Ant2.reg
 ]

type Code  = String
type POU   = ProblemOpts ()

job :: OutputBuffer -> String -> IO ()
job buff cmd = case ( decode cmd :: Result JSValue ) of
  Error str  -> do
    putStrLn $ "ERROR in job : " ++ str
    putStrLn $ cmd
  Ok jsvalue -> do
    let code = fromJsStr $ jsProp jsvalue "code" 
        Just runFun = Map.lookup code problemTab 
    putStrLn code
    runFun jsvalue buff

problemList :: JSValue
problemList = jsArr $ snd jobs

jobs :: ( [ (Code , JSValue -> OutputBuffer -> IO()) ] , [JSValue] )
jobs = unzip regs

reg :: Typeable a => ProblemOpts a -> ((String, JSValue -> OutputBuffer -> IO ()), JSValue)
reg po = ( (poCode po , \ jsv buff -> runProblemOpts (json2po po jsv) buff ) , po2json po )


problemTab :: Map Code ( JSValue -> OutputBuffer -> IO () )
problemTab  = Map.fromList $ fst jobs 







