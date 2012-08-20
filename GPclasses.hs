{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}

import System.Random
import Control.Monad.State
import Data.Maybe
import Util
import Dist

type Problem term typ gOpt mOpt cOpt = ( PopSize, gOpt, mOpt, cOpt, typ, term->FitVal )

type PopSize    = Int
type NumGens    = Int
type FitVal     = Double
type Prob       = Double

data GenOp term = MonoOp (term->Rand term) | DiOp (term->term->Rand(term,term))


class Generable term typ opt where
  generateIt :: typ -> opt -> Rand [term]

class Mutable term opt where
 mutateIt :: opt -> term -> Rand term 

class Crossable term opt where
 crossIt :: opt -> term -> term -> Rand (term,term)  

class Evolvable term typ gOpt mOpt cOpt where
  evolveIt  :: Problem term typ gOpt mOpt cOpt -> Rand [Dist term]
  
instance (Generable term typ gOpt, Mutable term mOpt, Crossable term cOpt) => Evolvable term typ gOpt mOpt cOpt where
 evolveIt ( popSize, gOpt, mOpt, cOpt, typ, ff ) = 
  evolveBegin popSize gOpt typ ff >>= infChainRand (evolveStep mOpt cOpt ff)

evolveBegin :: ( Generable term typ gOpt ) => PopSize -> gOpt -> typ -> (term->FitVal) -> Rand ( Dist term )
evolveBegin popSize gOpt typ ff = 
 (ffDist ff . take popSize) `liftM` generateIt typ gOpt

evolveStep :: ( Mutable term mOpt , Crossable term cOpt ) => mOpt -> cOpt -> (term->FitVal) -> Dist term -> Rand (Dist term)
evolveStep mOpt cOpt ff pop = 
  ffDist ff `liftM` ( getWinners pop >>= performOps mOpt cOpt )

ffDist :: (term->FitVal) -> [term] -> Dist term
ffDist ff = mkDist . map (\t->(t,ff t)) 

getWinners :: Dist term -> Rand ( term , [term] )
getWinners pop = 
  let Just (best,_) = distMax pop
   in ( best , ) `liftM` distTake_new (distSize pop - 1) pop 

performOps :: (Mutable term mOpt , Crossable term cOpt ) => mOpt -> cOpt -> (term,[term]) -> Rand [term]
performOps mOpt cOpt (best,terms) = (best : ) `liftM` performOps' opDist terms
 where
  opDist = mkDist 
   [ ( MonoOp return          , 33 )
   , ( MonoOp (mutateIt mOpt) , 33 ) 
   , ( DiOp   (crossIt  cOpt) , 33 ) ]
  
performOps' :: Dist (GenOp term) -> [term] -> Rand [term]
performOps' _ [] = return [] 
performOps' opDist terms@(t:ts) = do
  op <- distGet opDist
  case op of
   MonoOp f -> do 
    t'  <- f t
    ts' <- performOps' opDist ts
    return $ t' : ts'
   DiOp f -> case terms of
    [t] -> return [t]
    (t1:t2:tt) -> do
     (t1',t2') <- f t1 t2
     tt'       <- performOps' opDist tt
     return $ t1' : t2' : tt'

putEvolve :: (Show term , Evolvable term typ gOpt mOpt cOpt) => Problem term typ gOpt mOpt cOpt -> IO()
putEvolve problem =
 liftM (map $ fromJust . distMax) (runRand $ evolveIt problem ) >>= putList

putEvolveMaximas :: (Show term , Evolvable term typ gOpt mOpt cOpt) => Problem term typ gOpt mOpt cOpt -> IO()
putEvolveMaximas problem =
   liftM ( maximasBy (\(_,x) (_,y)->x<y) . (map $ fromJust . distMax ) ) (runRand $ evolveIt problem )  >>= putList


-- BitGenom -----------------------------

type BitGenomType = Int
type BitGenom     = [Bool] 
type ProbBitMut   = Double
type BitXoverOpt  = Int

instance Generable BitGenom BitGenomType () where generateIt n () = infiniteRand $ genBitGenom n
instance Mutable   BitGenom ProbBitMut      where mutateIt        = mutBitGenom
instance Crossable BitGenom BitXoverOpt     where crossIt         = xoverBitGenom

bitProblem :: PopSize->BitGenomType->ProbBitMut->(BitGenom->FitVal)->Problem BitGenom BitGenomType () ProbBitMut BitXoverOpt
bitProblem popSize genomSize probBitMut ff = 
  ( popSize , () , probBitMut , genomSize , genomSize , ff  )  

test  = runRand . evolveIt $ bitProblem 20 10 0.1 ff2
test' = putEvolve          $ bitProblem 20 10 0.1 ff2

ff1 :: BitGenom -> FitVal
ff1 bits = let tNum = fromIntegral . length . filter id $ bits in tNum * tNum

ff2 :: BitGenom -> FitVal
ff2 bits = let tNum = fromIntegral . length . filter (\(n,b)->if n `mod` 2 == 0 then b else not b) $ zip [0..] bits in tNum * tNum


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


-- experimental --------------------------

type Di a = (a,a) 

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

{--
crossThem :: (Crossable term opt) => opt -> [term] -> Rand [term]
crossThem _   []  = return []
crossThem _   [t] = return [t]
crossThem opt (t1:t2:ts) = do
 (t1',t2') <- crossIt opt t1 t2
 ts'       <- crossThem opt ts 
 return $ t1' : t2' : ts'
--}