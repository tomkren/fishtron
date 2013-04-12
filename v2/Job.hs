module Job ( job , problemList ) where


import GP_Core ( nRunsByServer )

import Text.JSON
import JSONUtils

import qualified Problems.SSR.Problem    as SSR
import qualified Problems.BigCtx.Problem as BigCtx
import qualified Problems.Fly.Problem    as Fly
import qualified Problems.Ant.Problem    as Ant
import qualified Problems.BA.Problem     as BA







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
      "ba"     -> go  BA.mainProblem -- BA.mainProblem jobID -- 
      

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
