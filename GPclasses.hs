{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

import System.Random
import Control.Monad.State


class Generable from by to where
 generateIt :: (RandomGen g) => g -> from -> by -> ( [to] , g )

class Mutable a by where
 mutateIt :: (RandomGen g) => g -> a -> by -> ( a , g )

class Xoverable a where
 xoverIt :: (RandomGen g) => g-> a -> a -> ( (a,a) , g )


type Rand a = State StdGen a

getRandom :: Random a => Rand a
getRandom = do
 gen <- get
 let (val,gen') = random gen
 put gen'
 return val

mkRand :: Int -> Rand ()
mkRand i = put $ mkStdGen i 

class Generable2 from by to where
  generateIt2 :: from -> by -> Rand [to]

class Mutable2 a by where
 mutateIt2 :: a -> by -> Rand a 

gene :: Int -> Rand a -> a
gene i rand = fst $ runState rand (mkStdGen i)


type CGenomType = Int
type CGenom     = [Bool] 
type CMutProb   = Double

instance Generable  CGenomType () CGenom where generateIt = cGenerate
instance Generable2 CGenomType () CGenom where generateIt2 t () = genCGenoms t
instance Mutable  CGenom CMutProb        where mutateIt   = cMutate 


genCGenoms :: CGenomType -> Rand [CGenom]
genCGenoms numBits = do
 genom  <- genCGenom  numBits
 genoms <- genCGenoms numBits
 return $ genom : genoms

genCGenom :: CGenomType -> Rand CGenom
genCGenom numBits 
 | numBits == 0 = return [] 
 | otherwise    = do
  bit  <- getRandom
  rest <- genCGenom (numBits-1)
  return $ bit : rest



cGenerate :: (RandomGen g) => g -> CGenomType -> () -> ( [CGenom] , g )
cGenerate gen numBits () = 
  let ( genom , gen'  ) = genCGenom gen  numBits 
      ( rest  , gen'' ) = cGenerate gen' numBits ()
   in ( genom : rest , gen'' )
 where
  genCGenom :: (RandomGen g) => g -> CGenomType -> ( [Bool] , g )
  genCGenom gen 0       = ( [] , gen )
  genCGenom gen numBits = 
   let ( bit  , gen'  ) = random gen
       ( rest , gen'' ) = genCGenom gen' (numBits-1) 
    in ( bit : rest , gen'' )

cMutate :: (RandomGen g) => g -> CGenom -> CMutProb -> ( CGenom , g )
cMutate gen genom p = foldr f ([],gen) genom  
 where 
  f bit (acc,gen) = 
   let (p',gen') = randomR (0.0,1.0) gen
    in ( (if p' < p then not bit else bit):acc , gen' )  


