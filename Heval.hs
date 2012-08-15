module Heval 
( heval
, evals
, as
) where

import Data.Typeable
import Data.List
import Control.Monad
import qualified Language.Haskell.Interpreter as Hint

{--
main :: IO ()
main = do 
 f <- heval "\\n->foo n" (as::Int -> Int)
 putStrLn . show $ f 42
--}

evals :: (Typeable a) => [String] -> a -> IO [a]
evals exprs as = do heval str [as]
 where str = "[" ++ (intercalate  "," exprs) ++ "]"

evals_uneff :: (Typeable a) => [String] -> a -> IO [a]
evals_uneff exprs as = do
 forM exprs $ \ expr -> 
  heval expr as

heval :: (Typeable a) => String -> a -> IO a
heval expr as = do 
  r <- Hint.runInterpreter $ do 
   Hint.loadModules ["HevalFuns.hs"]
   Hint.setTopLevelModules ["HevalFuns"]
   Hint.setImportsQ [("Prelude", Nothing)]
   Hint.interpret expr as
  case r of
   Left err -> do 
    printInterpreterError err
    return as
   Right x  -> return x    

as :: Typeable a => a
as = Hint.as

say :: String -> Hint.Interpreter ()
say = Hint.liftIO . putStrLn

printInterpreterError :: Hint.InterpreterError -> IO ()
printInterpreterError e = 
 putStrLn $ "[InterpreterError] : " ++ (show e)

