
import Control.Monad.Writer
import System.Random
import Control.Monad.State

import GPclasses 
import Util
import Dist

----------- EXPERIMENTS & GRAVEYARD .. --------------


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

sex :: ( Cros genom opt ) => opt -> Di genom -> Di genom -> Rand (Di genom)
sex opt dad@(deda1,babi1) mum@(deda2,babi2) = do
 (sperm,_) <- crossIt opt deda1 babi1
 (egg  ,_) <- crossIt opt deda2 babi2
 return (sperm,egg) 


sex2 :: ( Cros genom opt ) => opt -> Di (Dist genom) -> Di (Dist genom) -> Rand (Di [genom])
sex2 opt dad@(deda1,babi1) mum@(deda2,babi2) = do

 fromD1 <- distTake_new (distSize deda1) deda1 
 fromD2 <- distTake_new (distSize deda2) deda2
 fromB1 <- distTake_new (distSize babi1) babi1 
 fromB2 <- distTake_new (distSize babi2) babi2

 sperms <- forM (zip fromD1 fromB1) (\(d1,b1) -> crossIt opt d1 b1)
 eggs   <- forM (zip fromD2 fromB2) (\(d2,b2) -> crossIt opt d2 b2)
 
 return ( map fst sperms , map fst eggs ) 


meiosis :: ( Cros genom opt ) => opt -> Di genom -> Rand [genom]
meiosis opt (gDad,gMum) = do
 (son1,son2) <- crossIt opt gDad gMum
 (son3,son4) <- crossIt opt gDad gMum
 return [son1,son2,son3,son4]


-- TODO : pochopit proč to tohle rozbylo: 
--distGen :: (Gene t o) => DistGen o -> Rand [Dist t]
--distGen (DiG_Uniform opt len) = 
-- let gOpt = LG_ ( PG_Both opt (DG_Uniform (0,1) ) ) len
--  in generateIt gOpt >>= mapM (return . mkDist) <---------------
 
 
{--
crossThem :: (Cros term opt) => opt -> [term] -> Rand [term]
crossThem _   []  = return []
crossThem _   [t] = return [t]
crossThem opt (t1:t2:ts) = do
 (t1',t2') <- crossIt opt t1 t2
 ts'       <- crossThem opt ts 
 return $ t1' : t2' : ts'
--}	

{--
t4   = runRand $ (  (distGen (DiG_Uniform (DG_Normal (0,1) ) 10)  )::Rand [Dist Double] )


tt4  = runRand $ evolveBegin prob4

bug2 = runState (evolveBegin prob4) (mkStdGen 2)

bug3 = runState (( ffDist prob4 . take (popSize prob4) ) `liftM` generateIt (gOpt prob4)) (mkStdGen 2)

bug4 = runState ( ( take 3 ) `liftM` ((generateIt (gOpt prob4))::Rand [Dist Double] ) )  (mkStdGen 2)

bug1 = flip runState (mkStdGen 42) $ do 
 pop0 <- evolveBegin prob4
 distTake_new 1 pop0

--getWinners pop
ttt4 = runRand $ evolveStep prob4 =<< evolveBegin prob4

prob3 =  
 let popSize = 200
     len     = 100
     eOpt    = mkEOpt 33 33 33
     gOpt    = LG_         (PG_Both BG_    (DG_Normal (0,1)) ) len
     mOpt    = LM_OnePoint (PM_Both BM_Not (DM_Normal (0,1)) ) len
     cOpt    = LC_OnePoint (PC_Both BC_    DC_Avg            ) len -- tady muže bejt klidně () !!!
 in Problem popSize eOpt gOpt mOpt cOpt ff3


prob4 :: Problem (Dist Double) (DistGen DoubleGen) () ()
prob4 = 
 let popSize = 2
     len     = 3
     eOpt    = mkEOpt 33 0 0
     gOpt    = DiG_Uniform (DG_Normal (0,1) ) len
     mOpt    = () --DiM_ ( DM_Normal (0,1) )
     cOpt    = () --DiC_OnePoint ()
     ff :: Dist Double -> FitVal
     ff _ = 1
  in Problem popSize eOpt gOpt mOpt cOpt ff
--}