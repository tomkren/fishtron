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

--type Problem term gOpt mOpt cOpt = ( PopSize, EOpt, gOpt, mOpt, cOpt, term->FitVal )

data Problem term gOpt mOpt cOpt = Problem
 { popSize :: PopSize
 , eOpt    :: EOpt
 , gOpt    :: gOpt
 , mOpt    :: mOpt
 , cOpt    :: cOpt
 , fitFun  :: term->FitVal
 }

type PopSize    = Int
type NumGens    = Int
type FitVal     = Double
type Prob       = Double
type Len        = Int

type Mean   = Double
type StdDev = Double



data GenOpType  = Reproduction | Mutation | Crossover
data GenOp term = MonoOp (term->Rand term) | DiOp (term->term->Rand(term,term))

type EOpt = Dist GenOpType
--data EOpt = EOpt { probRep :: Prob , probMut :: Prob , probCro :: Prob }

mkEOpt :: Prob -> Prob -> Prob -> EOpt
mkEOpt probRep probMut probCro = 
 mkDist [ ( Reproduction , probRep ) 
        , ( Mutation     , probMut )   
        , ( Crossover    , probCro ) ]


class Gene term opt where
  generateIt :: opt -> Rand [term]

class Muta term opt where
 mutateIt :: opt -> term -> Rand term 

class Cros term opt where
 crossIt :: opt -> term -> term -> Rand (term,term)  

class Evolvable term gOpt mOpt cOpt where
  evolveIt  :: Problem term gOpt mOpt cOpt -> Rand [(Int,Dist term)]
  
instance (Gene term gOpt, Muta term mOpt, Cros term cOpt) => Evolvable term gOpt mOpt cOpt where
 evolveIt problem = 
  zip [0..] `liftM` (evolveBegin problem >>= infChainRand (evolveStep problem) )

evolveBegin :: ( Gene term gOpt ) => Problem term gOpt mOpt cOpt -> Rand ( Dist term )
evolveBegin p = 
 ( ffDist p . take (popSize p) ) `liftM` generateIt (gOpt p)

evolveStep :: ( Muta term mOpt , Cros term cOpt ) => Problem term gOpt mOpt cOpt -> Dist term -> Rand (Dist term)
evolveStep p pop = 
  ffDist p `liftM` ( getWinners pop >>= performOps p )

ffDist :: Problem term gOpt mOpt cOpt -> [term] -> Dist term
ffDist p = mkDist . map (\t->(t,ff t)) 
 where ff = fitFun p 

getWinners :: Dist term -> Rand ( term , [term] )
getWinners pop = 
  let Just (best,_) = distMax pop
   in ( best , ) `liftM` distTake_new (distSize pop - 1) pop 

performOps :: (Muta term mOpt , Cros term cOpt ) => Problem term gOpt mOpt cOpt -> (term,[term]) -> Rand [term]
performOps p (best,terms) = (best : ) `liftM` performOps' (mkOpDist p) terms  
 where
  mkOpDist p = fmap (f p) (eOpt p)
  f p opType = case opType of
   Reproduction -> MonoOp return
   Mutation     -> MonoOp (mutateIt $ mOpt p)
   Crossover    -> DiOp   (crossIt  $ cOpt p)
  
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
 liftM (map $ (\(i,d)->(i,fromJust . distMax $ d) ) ) (runRand $ evolveIt problem ) >>= putList

putEvolveMaximas :: (Show term , Evolvable term gOpt mOpt cOpt) => Problem term gOpt mOpt cOpt -> IO()
putEvolveMaximas problem =
   liftM ( maximasBy (\(_,(_,x)) (_,(_,y))->x<y) . (map $ (\(i,d)->(i,fromJust . distMax $ d) ) ) ) (runRand $ evolveIt problem )  
    >>= putList


-- instances -----------------

instance Gene t t  where generateIt x  = return $ repeat x
instance Muta t () where mutateIt _ x  = return x
instance Muta t t  where mutateIt o _  = return o
instance Cros t () where crossIt _ x y = return (x,y)

instance (Gene t1 o1,Gene t2 o2) => Gene (t1,t2) (PairGen o1 o2) where generateIt = pairGen 
instance (Muta t1 o1,Muta t2 o2) => Muta (t1,t2) (PairMut o1 o2) where mutateIt   = pairMut
instance (Cros t1 o1,Cros t2 o2) => Cros (t1,t2) (PairCro o1 o2) where crossIt    = pairCro

instance (Gene t o) => Gene [t]      (ListGen o) where generateIt = listGen 
instance (Muta t o) => Muta [t]      (ListMut o) where mutateIt   = listMut
instance (Cros t o) => Cros [t]      (ListCro o) where crossIt    = listCro 
     
instance (Gene t o) => Gene (Dist t) (DistGen o) where generateIt = distGen
instance (Muta t o) => Muta (Dist t) (DistMut o) where mutateIt   = distMut
instance (Cros t o) => Cros (Dist t) (DistCro o) where crossIt    = distCro


-- TODO : pochopit proč to tohle rozbylo: 
--distGen :: (Gene t o) => DistGen o -> Rand [Dist t]
--distGen (DiG_Uniform opt len) = 
-- let gOpt = LG_ ( PG_Both opt (DG_Uniform (0,1) ) ) len
--  in generateIt gOpt >>= mapM (return . mkDist) <---------------
 


data DistMut o = DiM_ o 
data DistCro o = DiC_OnePoint o
data DistGen o = DiG_Uniform o Len

distGen :: (Gene t o) => DistGen o -> Rand [Dist t]
distGen (DiG_Uniform opt len) = 
 let gOpt = LG_ ( PG_Both opt (DG_Uniform (0,1) ) ) len
  in do 
    xss <- generateIt gOpt 
    return $ map mkDist xss


distMut :: (Muta t o) => DistMut o -> Dist t -> Rand (Dist t)
distMut (DiM_ opt) dist = 
 let mOpt = LM_OnePoint ( PM_Both opt (DM_Normal (0,1) ) ) (distSize dist)
  in mkDist `liftM` mutateIt mOpt (distToList 1 dist)

distCro :: (Cros t o) => DistCro o -> Dist t -> Dist t -> Rand ( Dist t , Dist t )
distCro (DiC_OnePoint _) x y =
 let cOpt = LC_OnePoint () (distSize x)
  in do
   let x' = distToList 1 x
       y' = distToList 1 y 
   (a,b) <- crossIt cOpt x' y'
   return ( mkDist a , mkDist b )   


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

instance Gene Bool   BoolGen   where generateIt = boolGen
instance Muta Bool   BoolMut   where mutateIt   = boolMut 
instance Cros Bool   BoolCro   where crossIt    = boolCro

instance Gene Double DoubleGen where generateIt = doubleGen 
instance Muta Double DoubleMut where mutateIt   = doubleMut 
instance Cros Double DoubleCro where crossIt    = doubleCro

 

data PairGen o1 o2 = PG_Both o1 o2 
data PairMut o1 o2 = PM_Both o1 o2
data PairCro o1 o2 = PC_Both o1 o2

data ListGen opt   = LG_          opt Len
data ListMut opt   = LM_OnePoint  opt Len
                   | LM_Prob      opt Len Prob
data ListCro opt   = LC_OnePoint  opt Len
          --       | LC_OnePoint' opt Len
          --       | LC_NPoint    opt Len
          --       | LC_NPoint'   opt Len
          --       | LC_AllPoint  opt Len
  
data BoolGen       = BG_
data BoolMut       = BM_Not
                   | BM_Prob      Prob
data BoolCro       = BC_
      
data DoubleGen     = DG_Uniform   (Double,Double)
                   | DG_Normal    (Mean,  StdDev)
data DoubleMut     = DM_Uniform   (Double,Double)
                   | DM_Normal    (Mean,  StdDev)
data DoubleCro     = DC_Avg  
                   | DC_GeoAvg  
  

pairGen :: (Gene t1 o1,Gene t2 o2) => PairGen o1 o2 -> Rand [(t1,t2)]
pairGen (PG_Both o1 o2) = liftM2 zip (generateIt o1) (generateIt o2)

pairMut :: (Muta t1 o1,Muta t2 o2) => PairMut o1 o2 -> (t1,t2) -> Rand (t1,t2)
pairMut (PM_Both o1 o2) (x,y) = liftM2 (,) (mutateIt o1 x) (mutateIt o2 y)

pairCro :: (Cros t1 o1,Cros t2 o2) => PairCro o1 o2 -> (t1,t2) -> (t1,t2) -> Rand ((t1,t2),(t1,t2))
pairCro(PC_Both o1 o2) (x1,x2) (y1,y2) = do
 (a1,b1) <- crossIt o1 x1 y1
 (a2,b2) <- crossIt o2 x2 y2
 return $ ( (a1,b2) , (b1,a2) )

listGen :: (Gene term opt) => ListGen opt -> Rand [[term]]
listGen (LG_ opt len) = cutUpInflist len `liftM` generateIt opt 
 where
  cutUpInflist :: Len -> [a] -> [[a]]
  cutUpInflist len terms = 
   let ( listTerm, terms' ) = splitAt len terms
    in listTerm : cutUpInflist len terms'

listMut :: (Muta term opt) => ListMut opt -> [term] -> Rand [term]
listMut lm genom = case lm of
 LM_OnePoint opt len -> do
  pos <- getRandomR (0,len-1)
  let (xs,y:ys) = splitAt pos genom 
  (\y' -> xs ++ ( y' : ys ) ) `liftM` mutateIt opt y
 LM_Prob opt len p -> 
  forM genom $ \ bit -> randIf p (mutateIt opt bit) (return bit)




listCro :: (Cros term opt) => ListCro opt -> [term] -> [term] -> Rand ([term],[term])
listCro lc x y = case lc of
 LC_OnePoint _ len -> do
  cutPos <- getRandomR (0,len)
  let ( x1 , x2 ) = splitAt cutPos x
      ( y1 , y2 ) = splitAt cutPos y
  return $ ( x1 ++ y2 , y1 ++ x2 )


boolGen :: BoolGen -> Rand [Bool]
boolGen _ = infRand $ getRandom

boolMut :: BoolMut -> Bool -> Rand Bool
boolMut bm x = case bm of
 BM_Not    -> return $ not x
 BM_Prob p -> randCase p (not x) x

boolCro :: BoolCro -> Bool -> Bool -> Rand (Bool,Bool)
boolCro _ x y = return $ ( x && y , x || y )

doubleGen :: DoubleGen -> Rand [Double]
doubleGen dg = case dg of
 DG_Uniform range  -> infRand $ getRandomR range
 DG_Normal  params -> infRand $ getNormal  params

doubleMut :: DoubleMut -> Double -> Rand Double
doubleMut dm x = case dm of
 DM_Uniform range  -> (x+) `liftM` getRandomR range
 DM_Normal  params -> (x+) `liftM` getNormal  params

doubleCro :: DoubleCro -> Double -> Double -> Rand (Double,Double)
doubleCro dc x y = case dc of
 DC_Avg    -> let arith = (x + y) / 2
                  harmo = if x /= 0 && y /= 0 then 2 / ( 1/x + 1/y ) else arith
               in return (arith,harmo)
 DC_GeoAvg -> let gm = sqrt ( (abs x) * (abs y) ) 
               in return ( gm , -gm )



-- BitGenom -----------------------------




type BoolListProblem = Problem [Bool] (ListGen BoolGen) (ListMut BoolMut) (ListCro () )

boolListProblem :: PopSize -> Len -> ([Bool]->FitVal) -> BoolListProblem
boolListProblem popSize len ff = 
  Problem popSize (mkEOpt 33 33 33) (LG_ BG_ len) (LM_OnePoint BM_Not len ) (LC_OnePoint () len) ff  
  

test = putEvolveMaximas $ boolListProblem 20 100 ff2


testFF3 = 
 let popSize = 200
     len     = 100
     eOpt    = mkEOpt 33 33 33
     gOpt    = LG_         (PG_Both BG_    (DG_Normal (0,1)) ) len
     mOpt    = LM_OnePoint (PM_Both BM_Not (DM_Normal (0,1)) ) len
     cOpt    = LC_OnePoint (PC_Both BC_    DC_Avg            ) len -- tady muže bejt klidně () !!!
  in putEvolveMaximas $ Problem popSize eOpt gOpt mOpt cOpt ff3

testFF4 = 
 let popSize = 20
     len     = 10
     eOpt    = mkEOpt 33 0 0
     gOpt    = DiG_Uniform (DG_Normal (0,1) ) len
     mOpt    = DiM_ ( DM_Normal (0,1) )
     cOpt    = DiC_OnePoint ()
  in putEvolve $ Problem popSize eOpt gOpt mOpt cOpt ff4

ff1 :: [Bool]-> FitVal
ff1 bits = let tNum = dLen . filter id $ bits in tNum * tNum

ff2 :: [Bool] -> FitVal
ff2 bits = let tNum = dLen . filter (\(n,b)->if n `mod` 2 == 0 then b else not b) $ zip [0..] bits 
               len  = dLen $ bits
            in 100*(tNum * tNum)/(len*len)

ff3 :: [(Bool,Double)] -> FitVal
ff3 xs =
 let len     = dLen xs
     f (b,d) = (b && d > 0) || ( (not b) && d < 0 )
     numOk   = dLen . filter f $ xs
  in 100 * ( numOk * numOk ) / ( len * len )

ff4 :: Dist Double -> FitVal
ff4 d =  
 let xs = fst $ runState (distTake_new (2 * distSize d) d) (mkStdGen 42) 
  in 1 / ( 1 + abs (sum xs) ) 


dLen :: [a] -> Double
dLen = fromIntegral . length 

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

{--
crossThem :: (Cros term opt) => opt -> [term] -> Rand [term]
crossThem _   []  = return []
crossThem _   [t] = return [t]
crossThem opt (t1:t2:ts) = do
 (t1',t2') <- crossIt opt t1 t2
 ts'       <- crossThem opt ts 
 return $ t1' : t2' : ts'
--}