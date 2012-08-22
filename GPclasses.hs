{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}

import System.Random
import Control.Monad.State
import Data.Maybe
import Control.Monad.Writer
import Util
import Dist

type Problem term gOpt mOpt cOpt = ( PopSize, EOpt, gOpt, mOpt, cOpt, term->FitVal )

type PopSize    = Int
type NumGens    = Int
type FitVal     = Double
type Prob       = Double

data GenOp term = MonoOp (term->Rand term) | DiOp (term->term->Rand(term,term))

type EOpt    = ( ProbRep , ProbMut , ProbCro )
type ProbRep = Prob
type ProbMut = Prob
type ProbCro = Prob 



class Generable term opt where
  generateIt :: opt -> Rand [term]

class Mutable term opt where
 mutateIt :: opt -> term -> Rand term 

class Crossable term opt where
 crossIt :: opt -> term -> term -> Rand (term,term)  

class Evolvable term gOpt mOpt cOpt where
  evolveIt  :: Problem term gOpt mOpt cOpt -> Rand [Dist term]
  
instance (Generable term gOpt, Mutable term mOpt, Crossable term cOpt) => Evolvable term gOpt mOpt cOpt where
 evolveIt ( popSize, eOpt, gOpt, mOpt, cOpt, ff ) = 
  evolveBegin popSize gOpt ff >>= infChainRand (evolveStep eOpt mOpt cOpt ff)

evolveBegin :: ( Generable term gOpt ) => PopSize -> gOpt -> (term->FitVal) -> Rand ( Dist term )
evolveBegin popSize gOpt ff = 
 (ffDist ff . take popSize) `liftM` generateIt gOpt

evolveStep :: ( Mutable term mOpt , Crossable term cOpt ) => EOpt -> mOpt -> cOpt -> (term->FitVal) -> Dist term -> Rand (Dist term)
evolveStep eOpt mOpt cOpt ff pop = 
  ffDist ff `liftM` ( getWinners pop >>= performOps eOpt mOpt cOpt )

ffDist :: (term->FitVal) -> [term] -> Dist term
ffDist ff = mkDist . map (\t->(t,ff t)) 

getWinners :: Dist term -> Rand ( term , [term] )
getWinners pop = 
  let Just (best,_) = distMax pop
   in ( best , ) `liftM` distTake_new (distSize pop - 1) pop 

performOps :: (Mutable term mOpt , Crossable term cOpt ) => EOpt -> mOpt -> cOpt -> (term,[term]) -> Rand [term]
performOps (pRep,pMut,pCro) mOpt cOpt (best,terms) = (best : ) `liftM` performOps' opDist terms
 where
  opDist = mkDist 
   [ ( MonoOp return          , pRep )
   , ( MonoOp (mutateIt mOpt) , pMut ) 
   , ( DiOp   (crossIt  cOpt) , pCro ) ]
  
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

putEvolve :: (Show term , Evolvable term gOpt mOpt cOpt) => Problem term gOpt mOpt cOpt -> IO()
putEvolve problem =
 liftM (map $ fromJust . distMax) (runRand $ evolveIt problem ) >>= putList

putEvolveMaximas :: (Show term , Evolvable term gOpt mOpt cOpt) => Problem term gOpt mOpt cOpt -> IO()
putEvolveMaximas problem =
   liftM ( maximasBy (\(_,x) (_,y)->x<y) . (map $ fromJust . distMax ) ) (runRand $ evolveIt problem )  >>= putList


-- instances -----------------

instance Generable Bool ()   where generateIt _  = infRand $ getRandom
instance Mutable   Bool Prob where mutateIt p x  = randCase p (not x) x
instance Crossable Bool ()   where crossIt _ x y = return $ ( x && y , x || y )


instance Generable Double DoubleGenerator where generateIt dg  = doubleGenerator dg 
instance Mutable   Double DoubleMutation  where mutateIt dm x  = doubleMutation dm x 
instance Crossable Double DoubleCross     where crossIt dc x y = doubleCross dc x y

data DoubleGenerator = DGUnifRange (Double,Double)

doubleGenerator :: DoubleGenerator -> Rand [Double]
doubleGenerator dg = case dg of
 DGUnifRange range -> infRand $ getRandomR range

data DoubleMutation = DMDelta (Double,Double)
                    | DMNegate -- dummy example, not supposed to be used

doubleMutation :: DoubleMutation -> Double -> Rand Double
doubleMutation dm x = case dm of
 DMDelta range -> (x+) `liftM` getRandomR range
 DMNegate      -> return (-x)

data DoubleCross = DCAvg
                 | DCGeoAvg

doubleCross :: DoubleCross -> Double -> Double -> Rand (Double,Double)
doubleCross dc x y = case dc of
 DCAvg    -> let arith = (x + y) / 2
                 harmo = if x /= 0 && y /= 0 then 2 / ( 1/x + 1/y ) else arith
              in return (arith,harmo)
 DCGeoAvg -> let gm = sqrt ( (abs x) * (abs y) ) 
              in return ( gm , -gm )



-- BitGenom -----------------------------

type BitGenomType = Int
type BitGenom     = [Bool] 
type ProbBitMut   = Double
type BitXoverOpt  = Int

instance Generable BitGenom BitGenomType where generateIt n = infRand $ genBitGenom n
instance Mutable   BitGenom ProbBitMut   where mutateIt     = mutBitGenom
instance Crossable BitGenom BitXoverOpt  where crossIt      = xoverBitGenom

bitProblem :: PopSize->BitGenomType->ProbBitMut->(BitGenom->FitVal)->Problem BitGenom BitGenomType ProbBitMut BitXoverOpt
bitProblem popSize genomSize probBitMut ff = 
  ( popSize , (33,33,34) , genomSize , probBitMut , genomSize , ff  )  
  
bitProblem1 = bitProblem 20 100 0.1 ff2

test  = runRand . evolveIt $ bitProblem1
test' = putEvolve          $ bitProblem1
test''= putEvolveMaximas   $ bitProblem1

ff1 :: BitGenom -> FitVal
ff1 bits = let tNum = fromIntegral . length . filter id $ bits in tNum * tNum

ff2 :: BitGenom -> FitVal
ff2 bits = let tNum = fromIntegral . length . filter (\(n,b)->if n `mod` 2 == 0 then b else not b) $ zip [0..] bits 
               len  = fromIntegral . length $ bits
            in (tNum * tNum)/(len*len)


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
  randCase p (not bit) bit
--  p' <- getRandomR (0.0,1.0)
--  return $ if p' < p then not bit else bit


xoverBitGenom :: BitXoverOpt -> BitGenom -> BitGenom -> Rand (BitGenom,BitGenom)
xoverBitGenom genomSize genom1 genom2 = do
 cutPos <- getRandomR (0,genomSize)
 let ( g1a , g1b ) = splitAt cutPos genom1
 let ( g2a , g2b ) = splitAt cutPos genom2
 return $ ( g1a ++ g2b , g2a ++ g1b )


-- writer experiments ----------------------

type Mo a = WriterT [String] (State StdGen) a

getRandom_ :: Random a => Mo a
getRandom_ = lift getRandom

getRandomR_ :: Random a => (a,a) -> Mo a
getRandomR_ = lift . getRandomR

infiniteRand_ :: Rand a -> Mo [a]
infiniteRand_ = lift . infiniteRand

log_ :: String -> Mo () 
log_ str = tell [str]

getGen :: Mo StdGen
getGen = lift get

putGen :: StdGen -> Mo ()
putGen = lift . put

 
-- Podle mě bude problém s nekonečnym logovánim....

infChain :: (a -> Mo a) -> a -> Mo [a]
infChain f x = do
   gen <- getGen
   let (gen1,gen2) = split gen
   putGen gen1
   xs <- inf f x 
   putGen gen2
   return $ x:xs
 where
  inf :: (a -> Mo a) -> a -> Mo [a]
  inf f x = do
   x' <- f x
   xs <- inf f x'
   return $ x':xs 

infSame :: Mo a -> Mo [a]
infSame rand = do
   gen <- getGen
   let (gen1,gen2) = split gen
   putGen gen1
   xs <- inf rand
   putGen gen2
   return xs
 where
  inf :: Mo a -> Mo [a]
  inf r = do
   x  <- r
   xs <- inf r
   return $ x:xs

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