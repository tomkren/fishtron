{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

import System.Random
import Control.Monad.State


class Generable_old from by to where
 generateIt_old :: (RandomGen g) => g -> from -> by -> ( [to] , g )

class Mutable_old a by where
 mutateIt_old :: (RandomGen g) => g -> a -> by -> ( a , g )

class Xoverable_old a where
 xoverIt_old :: (RandomGen g) => g-> a -> a -> ( (a,a) , g )


type Rand a = State StdGen a

randLift :: Random a => (StdGen -> (a,StdGen)) -> Rand a
randLift f = do
 gen <- get
 let (val,gen') = f gen
 put gen'
 return val

getRandom :: Random a => Rand a
getRandom = randLift random

getRandomR :: Random a => (a,a) -> Rand a
getRandomR range = randLift $ randomR range

mkRand :: Int -> Rand ()
mkRand i = put $ mkStdGen i 

class Generable from by to where
  generateIt :: from -> by -> Rand [to]

class Mutable a by where
 mutateIt :: a -> by -> Rand a 

gene :: Int -> Rand a -> a
gene i rand = fst $ runState rand (mkStdGen i)


type BitGenomType = Int
type BitGenom     = [Bool] 
type ProbBitMut   = Double

instance Generable_old BitGenomType () BitGenom where generateIt_old = cGenerate
instance Mutable_old BitGenom ProbBitMut        where mutateIt_old   = cMutate 

instance Generable BitGenomType () BitGenom where generateIt t () = genBitGenoms t
instance Mutable   BitGenom ProbBitMut      where mutateIt = mutBitGenom

genBitGenoms :: BitGenomType -> Rand [BitGenom]
genBitGenoms numBits = do
 genom  <- genBitGenom  numBits
 genoms <- genBitGenoms numBits
 return $ genom : genoms

genBitGenom :: BitGenomType -> Rand BitGenom
genBitGenom numBits 
 | numBits == 0 = return [] 
 | otherwise    = do
  bit  <- getRandom
  rest <- genBitGenom (numBits-1)
  return $ bit : rest

mutBitGenom :: BitGenom -> ProbBitMut -> Rand BitGenom
mutBitGenom genom p = do
 forM genom $ \ bit -> do
  p' <- getRandomR (0.0,1.0)
  return $ if p' < p then not bit else bit


test :: (Generable typ env genom, Mutable genom mutOpt ) => Int -> typ -> env -> mutOpt -> Rand [[genom]]
test generSize typ env mutOpt = do
 first <- generateIt typ env
 let first' = take generSize first
 rest  <- mutGenerations mutOpt first'
 return $ first' : rest

mutGenerations :: (Mutable genom mutOpt ) => mutOpt -> [genom] -> Rand [[genom]]
mutGenerations mutOpt gener = do
 gener' <- mutGeneration  mutOpt gener 
 rest   <- mutGenerations mutOpt gener'
 return $ gener' : rest

mutGeneration :: (Mutable genom mutOpt ) => mutOpt -> [genom] -> Rand [genom]
mutGeneration mutOpt gener = do
 forM gener $ \ genom -> 
  mutateIt genom mutOpt


cGenerate :: (RandomGen g) => g -> BitGenomType -> () -> ( [BitGenom] , g )
cGenerate gen numBits () = 
  let ( genom , gen'  ) = genBitGenom gen  numBits 
      ( rest  , gen'' ) = cGenerate gen' numBits ()
   in ( genom : rest , gen'' )
 where
  genBitGenom :: (RandomGen g) => g -> BitGenomType -> ( [Bool] , g )
  genBitGenom gen 0       = ( [] , gen )
  genBitGenom gen numBits = 
   let ( bit  , gen'  ) = random gen
       ( rest , gen'' ) = genBitGenom gen' (numBits-1) 
    in ( bit : rest , gen'' )

cMutate :: (RandomGen g) => g -> BitGenom -> ProbBitMut -> ( BitGenom , g )
cMutate gen genom p = foldr f ([],gen) genom  
 where 
  f bit (acc,gen) = 
   let (p',gen') = randomR (0.0,1.0) gen
    in ( (if p' < p then not bit else bit):acc , gen' )  


