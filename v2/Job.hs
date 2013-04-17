module Job ( job , problemList , problemList_ , job_ ) where

import GP_Core ( nRunsByServer )
import Text.JSON
import JSONUtils
import Problems.Utils ( ProblemOpts , po2json, poCode, json2po , runProblemOpts )
import Data.Typeable ( Typeable )
import Data.Map (Map)
import qualified Data.Map as Map
--import Utils ( unescape )



import qualified Problems.SSR.Problem    as SSR
import qualified Problems.BigCtx.Problem as BigCtx
import qualified Problems.Fly.Problem    as Fly
import qualified Problems.Ant.Problem    as Ant
import qualified Problems.BA.Problem     as BA

regs = 
 [ reg Fly.reg 
 , reg SSR.reg 
 , reg Ant.reg
 , reg BigCtx.reg
 , reg (BA.reg :: POU) 
 ]











type JobID = String
type Code  = String
type POU   = ProblemOpts ()



reg :: Typeable a => ProblemOpts a -> ((String, JSValue -> JobID -> IO ()), JSValue)
reg po = ( (poCode po , \ jsv jobID -> runProblemOpts (json2po po jsv) jobID ) , po2json po )

jobs :: ( [ (Code , JSValue -> JobID -> IO()) ] , [JSValue] )
jobs = unzip regs

problemList_ :: JSValue
problemList_ = jsArr $ snd jobs

problemTab :: Map Code ( JSValue -> JobID -> IO () )
problemTab  = Map.fromList $ fst jobs 

job_ :: JobID -> String -> IO ()
job_ jobID cmd = case ( decode cmd :: Result JSValue ) of
  Error str  -> putStrLn $ "ERROR in job_ : " ++ str
  Ok jsvalue -> do
    let code = fromJsStr $ jsProp jsvalue "code" 
        Just runFun = Map.lookup code problemTab 
    putStrLn code
    runFun jsvalue jobID 







( problemMap , problemList__ ) = registerThem 
 [  SSR.reg 
 -- ,  Fly.reg 
 ,   BA.reg ]

registerThem :: [ProblemOpts a] -> ( [(String,ProblemOpts a)] , JSValue )
registerThem xs = ( map (\po->(poCode po,po)) xs , jsArr $ map po2json xs )

job__ :: JobID -> String -> IO ()
job__ jobID cmd = case ( decode cmd :: Result JSValue ) of
  Error str  -> putStrLn $ "ERROR in job__ : " ++ str
  Ok jsvalue -> do
    let code    = fromJsStr $ jsProp jsvalue "code" 
        Just po = lookup code problemMap 
        poNew   = json2po po jsvalue
    putStrLn code
    putStrLn . show $ po2json poNew
    runProblemOpts poNew jobID




problemList :: [(String,String,JSValue)]
problemList = [ 
  ("fly"    , "Fly eating apples and stuff... "                               , Fly.jsData ) , 
  ("ssr"    , "Simple Symbolic Regression - new IM !"                         , SSR.jsData ) ,
  ("ant"    , "Artifical Ant"                                                 , Ant.jsData ) ,
  ("ba"     , "Bool Alternate"                                                , JSNull     ) ,
  

  ("head"   , "Problem with basic funs for types generating elementary funs." , JSNull     ) ,
  ("tail"   , "..."                                                           , JSNull     ) ,
  ("map"    , "..."                                                           , JSNull     ) ,
  ("filter" , "..."                                                           , JSNull     ) ,
  ("elem"   , "..."                                                           , JSNull     ) ,
  
  ("ssr_old", "Simple Symbolic Regression - old IM !"                         , JSNull     )
  
 ]




job :: String -> String -> IO ()
job jobID cmd = 
  let parts   = wordsWhen (==' ') cmd
      problem = head parts
      numRuns = (read $ parts !! 1)::Int
      numGene = read $ parts !! 2  
      popSize = read $ parts !! 3
      go p    = go' p jobID numRuns numGene popSize  --nRunsByServer jobID numRuns ( p numGene popSize )  
   in do
    putStrLn cmd
    case problem of
      "fly"    -> go Fly.problem1
      "ssr"    -> go SSR.mainProblem
      "ant"    -> go Ant.mainProblem
      "ba"     -> go  BA.mainProblem_ -- BA.mainProblem jobID -- 
      

      -- "ssr2"   -> go SSR.problem2 -- problem_ssr2
      -- "ssr3"   -> go SSR.problem3
      -- "ssr4"   -> go SSR.problem4
      "head"   -> go BigCtx.pr_head  
      "tail"   -> go BigCtx.pr_tail  
      "map"    -> go BigCtx.pr_map   
      "filter" -> go BigCtx.pr_filter
      "elem"   -> go BigCtx.pr_elem  

      "ssr_old"-> go SSR.problem1 -- problem_ssr
      
go' p jobID numRuns numGene popSize = nRunsByServer jobID numRuns ( p numGene popSize )  


wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
