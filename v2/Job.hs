module Job ( job , problemList ) where


import GP_Core ( nRunsByServer )




import GP_Test (problem_ssr,problem_ssr2,problem_ant,problem_ba,problem_head)

import qualified Problems.SSR.Problem    as SSR
import qualified Problems.BigCtx.Problem as BigCtx





problemList :: [(String,String)]
problemList = [ 
  ("ssr"    , "Simple Symbolic Regression - old IM") ,
  ("ssr2"   , "Simple Symbolic Regression - new IM") ,
  ("ssr3"   , "Simple Symbolic Regression - new IM & single-filed & inside-FF") ,
  ("ssr4"   , "Simple Symbolic Regression - new IM & single-filed"),
  ("ba"     , "Bool Alternate") ,
  ("ant"    , "Artifical Ant") ,
  ("head"   , "Trivial Head problem" ),
  ("BigCtx" , "Problem with basic funs for types generating elementary funs." )
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
      "ant"    -> go problem_ant 
      "ba"     -> go problem_ba  
      "ssr"    -> go SSR.problem1 -- problem_ssr
      "ssr2"   -> go SSR.problem2 -- problem_ssr2
      "ssr3"   -> go SSR.problem3
      "ssr4"   -> go SSR.problem4
      "head"   -> go problem_head 
      "BigCtx" -> go BigCtx.problem1




wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
