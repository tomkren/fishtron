module Heval 
( heval
, hevals
) where

import Data.Typeable
import System.IO.Unsafe
import Data.List
import Control.Monad

import qualified Language.Haskell.Interpreter as Hint

hevals :: (Typeable a) => [String] -> a -> IO [a]
hevals exprs as = do heval str [as]
 where str = "[" ++ (intercalate  "," exprs) ++ "]"

hevals_uneff :: (Typeable a) => [String] -> a -> IO [a]
hevals_uneff exprs as = do
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


printInterpreterError :: Hint.InterpreterError -> IO ()
printInterpreterError e = 
 putStrLn $ "[InterpreterError] : " ++ (show e)

