module Job ( job , problemList , problemList_ , job_ ) where


import GP_Core ( nRunsByServer )

import Text.JSON
import JSONUtils

import Problems.Utils ( ProblemOpts , po2json, poCode, json2po , runProblemOpts )

import qualified Problems.SSR.Problem    as SSR
import qualified Problems.BigCtx.Problem as BigCtx
import qualified Problems.Fly.Problem    as Fly
import qualified Problems.Ant.Problem    as Ant
import qualified Problems.BA.Problem     as BA

import Data.Typeable ( Typeable )


( problemMap , problemList__ ) = registerThem 
 [  SSR.problemOpts 
 -- ,  Fly.problemOpts 
 ,   BA.problemOpts ]


type JobID = String
type Code  = String
type POU   = ProblemOpts ()

problemList_ = jsArr $ snd jobs
problemTab_  = fst jobs 

jobs :: ( [ (Code , JSValue -> JobID -> IO()) ] , [JSValue] )
jobs = unzip [ 
  f (BA.problemOpts :: POU)  , 
  f Fly.problemOpts , 
  f SSR.problemOpts ]
 where
  f po = ( (poCode po , \ jsv jobID -> runProblemOpts (json2po po jsv) jobID ) , po2json po )


registerThem :: [ProblemOpts a] -> ( [(String,ProblemOpts a)] , JSValue )
registerThem xs = ( map (\po->(poCode po,po)) xs , jsArr $ map po2json xs )

job_ :: JobID -> String -> IO ()
job_ jobID cmd = case ( decode (unescape cmd) :: Result JSValue ) of
  Error str  -> putStrLn $ "ERROR in job_ : " ++ str
  Ok jsvalue -> do
    let code = fromJsStr $ jsProp jsvalue "code" 
        Just runFun = lookup code problemTab_ 
    putStrLn code
    runFun jsvalue jobID 


job__ :: JobID -> String -> IO ()
job__ jobID cmd = case ( decode (unescape cmd) :: Result JSValue ) of
  Error str  -> putStrLn $ "ERROR in job_ : " ++ str
  Ok jsvalue -> do
    let code    = fromJsStr $ jsProp jsvalue "code" 
        Just po = lookup code problemMap 
        poNew   = json2po po jsvalue
    putStrLn code
    putStrLn . show $ po2json poNew
    runProblemOpts poNew jobID


unescape :: String -> String
unescape []     = []
unescape (x:[]) = [x]
unescape (x:y:rest)
    | x == '\\' = y : unescape rest
    | otherwise = x : unescape (y : rest)







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
