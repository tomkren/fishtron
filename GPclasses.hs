{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

import System.Random
import Control.Monad.State
import Util
import Dist


class Generable term typ opt where
  generateIt :: typ -> opt -> Rand [term]

class Mutable term opt where
 mutateIt :: opt -> term -> Rand term 

class Crossable term opt where
 crossIt :: opt -> term -> term -> Rand (term,term)  

class ( Generable term typ genOpt  , 
        Mutable   term mutOpt , 
        Crossable term crossOpt ) => 
        Evolvable term typ genOpt mutOpt crossOpt  -- eOpt 
 where
  evolveIt :: GenSize -> genOpt -> mutOpt -> crossOpt -> typ -> (term->FitVal) -> Rand [Dist term]

evolve :: ( Generable term typ genOpt  , Mutable   term mutOpt , Crossable term crossOpt ) => 
          NumGens -> GenSize -> genOpt -> mutOpt -> crossOpt -> typ -> (term->FitVal) -> Rand [Dist term]
evolve numGens gSize genOpt mutOpt crossOpt typ fitFun = do
  genTerms <- take gSize `liftM` generateIt typ genOpt
  steps numGens fitFun crossOpt $ mkDist $ mapFF genTerms
 where
  mapFF = map (\ t ->( t , fitFun t ))


  steps :: (Crossable term crossOpt) => NumGens -> (term->FitVal) -> crossOpt -> Dist term -> Rand [Dist term]
  steps i ff crossOpt g = 
    if i == 0 
      then return []
      else do 
       g'   <- step ff crossOpt g
       rest <- steps (i-1) ff crossOpt g' --g'
       return $ g' : rest --return $ g : g' : rest 
  
   where
  
    step :: (Crossable term crossOpt) => (term->FitVal) -> crossOpt -> Dist term -> Rand (Dist term)
    step ff crossOpt g = do
     winners <- distTake_new gSize g
     childs  <- crossThem crossOpt winners
     return $ mkDist $ map (\ t ->( t , ff t ) ) childs
  



crossThem :: (Crossable term opt) => opt -> [term] -> Rand [term]
crossThem _   []  = return []
crossThem _   [t] = return [t]
crossThem opt (t1:t2:ts) = do
 (t1',t2') <- crossIt opt t1 t2
 ts'       <- crossThem opt ts 
 return $ t1' : t2' : ts'

test :: Rand [Dist BitGenom]
test =  evolve ( 5  :: NumGens )
               ( 10  :: GenSize ) 
               () 
               ( 0.1 :: ProbBitMut )
               ( 4 :: BitXoverOpt )
               ( 4 :: BitGenomType )
               ( fromIntegral . length . filter id )

 
type BitGenomType = Int
type BitGenom     = [Bool] 
type ProbBitMut   = Double
type BitXoverOpt  = Int

type Di a = (a,a) 


-- spiš než tohle to dat do evolveOpt
--type BitMutatOpt  = ( Prob , ProbBitMut   )
--type BitXoverOpt  = ( Prob , BitGenomType )
--type Prob = Double

type GenSize      = Int
type NumGens      = Int
type FitVal       = Double

instance Generable BitGenom BitGenomType () where generateIt t () = genBitGenoms t
instance Mutable   BitGenom ProbBitMut      where mutateIt        = mutBitGenom
instance Crossable BitGenom BitXoverOpt     where crossIt         = xoverBitGenom

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

mutBitGenom :: ProbBitMut -> BitGenom -> Rand BitGenom
mutBitGenom p genom = do
 forM genom $ \ bit -> do
  p' <- getRandomR (0.0,1.0)
  return $ if p' < p then not bit else bit

xoverBitGenom :: BitXoverOpt -> BitGenom -> BitGenom -> Rand (BitGenom,BitGenom)
xoverBitGenom genomSize genom1 genom2 = do
 cutPos <- getRandomR (0,genomSize)
 let ( g1a , g1b ) = splitAt cutPos genom1
 let ( g2a , g2b ) = splitAt cutPos genom2
 return $ ( g1a ++ g2b , g2a ++ g1b )




{--
gp :: (Generable typ env term , Mutable term mutOpt ) => Int -> Int -> typ -> env -> mutOpt -> (term->FitVal) -> Rand [Dist term]
gp numGens genSize typ env mutOpt fitFun = undefined --do
-- gen0 <- take genSize `liftM` generateIt typ env

gpStep :: ( Mutable term mutOpt ) => mutOpt -> (term->FitVal) -> Dist term -> Rand (Dist term) 
gpStep mutOpt fitFun gen = do
  best     <- getBest
  popSize  <- populationSize
  parents  <- getWinners (popSize - 1)
  children <- xovers parents
  updatePopulation ff $ best:children
--}

mutGenerations :: (Mutable genom mutOpt ) => mutOpt -> [genom] -> Rand [[genom]]
mutGenerations mutOpt gener = do
 gener' <- mutGeneration  mutOpt gener 
 rest   <- mutGenerations mutOpt gener'
 return $ gener' : rest

mutGeneration :: (Mutable genom mutOpt ) => mutOpt -> [genom] -> Rand [genom]
mutGeneration mutOpt gener = do
 forM gener $ \ genom -> 
  mutateIt mutOpt genom


sex :: ( Crossable genom opt ) => opt -> Di genom -> Di genom -> Rand (Di genom)
sex opt dad@(deda1,babi1) mum@(deda2,babi2) = do
 (sperm,_) <- crossIt opt deda1 babi1
 (egg  ,_) <- crossIt opt deda2 babi2
 return (sperm,egg) 


sex2 :: ( Crossable genom opt ) => opt -> Di (Dist genom) -> Di (Dist genom) -> Rand (Di [genom])
sex2 opt dad@(deda1,babi1) mum@(deda2,babi2) = do

 fromD1 <- distTake_new (distSize deda1) deda1 
 fromD2 <- distTake_new (distSize deda2) deda2
 fromB1 <- distTake_new (distSize babi1) babi1 
 fromB2 <- distTake_new (distSize babi2) babi2

 sperms <- forM (zip fromD1 fromB1) (\(d1,b1) -> crossIt opt d1 b1)
 eggs   <- forM (zip fromD2 fromB2) (\(d2,b2) -> crossIt opt d2 b2)
 
 return ( map fst sperms , map fst eggs ) 


meiosis :: ( Crossable genom opt ) => opt -> Di genom -> Rand [genom]
meiosis opt (gDad,gMum) = do
 (son1,son2) <- crossIt opt gDad gMum
 (son3,son4) <- crossIt opt gDad gMum
 return [son1,son2,son3,son4]
