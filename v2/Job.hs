module Job ( job , problemList ) where


import GP_Core ( nRunsByServer )

import GP_Test (problem_ssr,problem_ssr2,problem_ant,problem_ba,problem_head)

import Text.JSON
import JSONUtils

import qualified Problems.SSR.Problem    as SSR
import qualified Problems.BigCtx.Problem as BigCtx
import qualified Problems.Fly.Problem    as Fly





problemList :: [(String,String,JSValue)]
problemList = [ 
  ("fly"    , "Fly eating apples and stuff... "                               , Fly.jsData ) , 
  ("head"   , "Problem with basic funs for types generating elementary funs." , JSNull     ) ,
  ("tail"   , "..."                                                           , JSNull     ) ,
  ("map"    , "..."                                                           , JSNull     ) ,
  ("filter" , "..."                                                           , JSNull     ) ,
  ("elem"   , "..."                                                           , JSNull     ) ,
  ("ssr"    , "Simple Symbolic Regression - new IM !"                         , SSR.jsData ) ,
  ("ssr_old", "Simple Symbolic Regression - old IM !"                         , JSNull     ) ,
  -- ("ssr2"   , "Simple Symbolic Regression - new IM"                           , JSNull     ) ,
  -- ("ssr3"   , "Simple Symbolic Regression - new IM & single-filed & inside-FF", JSNull     ) ,
  -- ("ssr4"   , "Simple Symbolic Regression - new IM & single-filed"            , JSNull     ) ,
  ("ba"     , "Bool Alternate"                                                , JSNull     ) ,
  ("ant"    , "Artifical Ant"                                                 , JSNull     ) ,
  ("head_"  , "Trivial Head problem"                                          , JSNull     )   
 ]




job :: String -> String -> IO ()
job jobID cmd = 
  let parts   = wordsWhen (==' ') cmd
      problem = head parts
      numRuns = (read $ parts !! 1)::Int
      numGene = read $ parts !! 2  
      popSize = read $ parts !! 3
      go p    = nRunsByServer jobID numRuns ( p numGene popSize )  
   in do
    putStrLn cmd
    case problem of
      "fly"    -> go Fly.problem1
      "ant"    -> go problem_ant 
      "ba"     -> go problem_ba  
      "ssr"    -> go SSR.mainProblem
      "ssr_old"-> go SSR.problem1 -- problem_ssr
      -- "ssr2"   -> go SSR.problem2 -- problem_ssr2
      -- "ssr3"   -> go SSR.problem3
      -- "ssr4"   -> go SSR.problem4
      "head_"  -> go problem_head 
      "head"   -> go BigCtx.pr_head  
      "tail"   -> go BigCtx.pr_tail  
      "map"    -> go BigCtx.pr_map   
      "filter" -> go BigCtx.pr_filter
      "elem"   -> go BigCtx.pr_elem  




wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
