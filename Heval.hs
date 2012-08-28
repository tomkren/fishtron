{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Heval 
( eval
, evals
, as
) where

import Data.Typeable
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import System.Random
import Data.Functor.Identity

import System.IO.Unsafe

import qualified Language.Haskell.Interpreter as Hint

import Util



{--
main :: IO ()
main = do 
 f <- heval "\\n->foo n" (as::Int -> Int)
 putStrLn . show $ f 42

type HEval =  StateT StdGen IO

randToHEval :: Rand a -> HEval a
randToHEval = state . runState
 
liftHE :: (a -> b) -> Rand a -> HEval b
f `liftHE` rand = f `liftM` (randToHEval rand)

instance RunRand HEval where 
 runRand he = do
  gen <- getStdGen
  fst `liftM` runStateT he gen

hEval :: (Typeable a) => String -> a -> HEval a
hEval expr as = liftIO $ eval expr as 

hEvals :: (Typeable a) => [String] -> a -> HEval [a]
hEvals exprs as = liftIO $ evals exprs as 

--}

 
eval :: (Typeable a) => String -> a -> a
eval str as = unsafePerformIO (heval str as) 

evals :: (Typeable a) => [String] -> a -> [a]
evals strs as = unsafePerformIO (hevals strs as) 



hevals :: (Typeable a) => [String] -> a -> IO [a]
hevals exprs as = do heval str [as]
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

