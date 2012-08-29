{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}

module GPclasses where

import System.Random
import Control.Monad.State
import Data.Maybe
import Data.Typeable

import Util
import Dist
import Heval
import KozaTree

--type Problem term gOpt mOpt cOpt = ( PopSize, EOpt, gOpt, mOpt, cOpt, term->FitVal )

data Problem term a gOpt mOpt cOpt = Problem
 { popSize :: PopSize
 , eOpt    :: EOpt
 , gOpt    :: gOpt
 , mOpt    :: mOpt
 , cOpt    :: cOpt
 , fitFun  :: FitFun2 term a
 , ass     :: a
 }

type FitFun term = term -> Rand FitVal

data FitFun2 term a = FF1 (term -> Rand FitVal)
                    | FF2 (term->String) a (a->Rand FitVal)



type Credit = Double

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

mkEOpt :: (Prob,Prob,Prob) -> EOpt
mkEOpt (probRep,probMut,probCro) = 
 mkDist [ ( Reproduction , probRep ) 
        , ( Mutation     , probMut )   
        , ( Crossover    , probCro ) ]


class Gene term opt where
  generateIt :: opt -> Rand [term]

class Muta term opt where
 mutateIt :: opt -> term -> Rand term 

class Cros term opt where
 crossIt :: opt -> term -> term -> Rand (term,term)  

class Evolvable term a gOpt mOpt cOpt where
  evolveIt  :: Problem term a gOpt mOpt cOpt -> Credit -> Rand [Dist term]
  
instance (Gene term gOpt, Muta term mOpt, Cros term cOpt,Typeable a) => Evolvable term a gOpt mOpt cOpt where
 evolveIt problem credit = 
  wrapper $ evolveBegin problem credit >>= infChainRand (evolveStep problem) 

wrapper :: Rand [(Dist term,a)] -> Rand [Dist term]
wrapper rand = do
 xs <- rand
 return . takeWhile (not . distIsEmpty) . map fst $ xs 

evolveBegin :: ( Gene term gOpt, Typeable a ) => Problem term a gOpt mOpt cOpt -> Credit -> Rand (Dist term,Credit)
evolveBegin p credit = do
 let toTake = min (popSize p) (floor credit) 
 pop0 <- (take toTake `liftM` generateIt (gOpt p)) >>= evalFF p
 return (pop0, credit - fromIntegral toTake )

evolveStep ::(Muta term mOpt,Cros term cOpt,Typeable a)=>Problem term a gOpt mOpt cOpt->(Dist term,Credit)->Rand (Dist term,Credit)
evolveStep p x@(_,credit) = do
 pop <- ( getWinners x >>= performOps p ) >>= evalFF p
 return (pop, credit - fromIntegral (distSize pop) )

evalFF :: (Typeable a) => Problem term a gOpt mOpt cOpt -> [term] -> Rand (Dist term)
evalFF p ts = case fitFun p of
 FF1 ff -> mkDist `liftM` mapM (\t->(t,) `liftM` ff t) ts
 FF2 toStr a ff -> 
  let strs = map toStr ts
      as   = evals strs a
   in mkDist `liftM` mapM (\(t,a)->(t,) `liftM` ff a) (zip ts as)


getWinners :: (Dist term,Credit) -> Rand ( [term] , [term] )
getWinners (pop,credit) = 
  let Just (best,_) = distMax pop
      bests  = if credit >= 1 then [best] else []
      toTake = (min (distSize pop) (floor credit) ) - length bests 
   in ( bests , ) `liftM` distTake_new toTake pop 

performOps :: (Muta term mOpt , Cros term cOpt ) => Problem term a gOpt mOpt cOpt -> ([term],[term]) -> Rand [term]
performOps p (best,terms) = (best ++ ) `liftM` performOps' (mkOpDist p) terms  
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


putEvolve :: (Show term , Evolvable term a gOpt mOpt cOpt) => Credit -> Problem term a gOpt mOpt cOpt -> IO()
putEvolve credit problem =
 liftM (map $ (\(i,d)->(i,fromJust . distMax $ d) ) ) (runRand $ (zip [0..]) `liftM` evolveIt problem credit ) >>= putList

putEvolveMaximas :: (Show term , Evolvable term a gOpt mOpt cOpt) => Credit -> Problem term a gOpt mOpt cOpt -> IO ()
putEvolveMaximas credit problem = do
 ds <- runRand $ zip [0..] `liftM` evolveIt problem credit
 let f (i,t) = ( i , fromJust.distMax$t)
     bs = map f ds
     g = snd . snd 
     lt x y = (g x) < (g y)
     ms = maximasBy lt bs
 putList ms

-- instances -----------------

--as :: a
--as = undefined

data TermLog t = LogGenom t | LogLine1 | LogLine2 | LogLine3 

instance Show t => Show (TermLog t) where
 show l = case l of
  LogGenom t -> show t
  LogLine3 -> replicate 80 '#'
  LogLine2 -> replicate 80 '='
  LogLine1 -> replicate 80 '-'

testGene :: (Show t, Gene t o) => t -> o -> IO [t]
testGene _ o = do
 ts <- runRand $ generateIt o
 putList ts
 return ts 

testMuta :: (Show t, Gene t oGen, Muta t oMut) => t -> oGen -> oMut -> IO [TermLog t]
testMuta _ oGen oMut = do
 ts <- runRand $ generateIt oGen
 ms <- runRand $ forM ts (\t-> mutateIt oMut t)  
 let ls = concatMap (\(t,m) -> [LogGenom t , LogLine1 , LogGenom m , LogLine2] ) ( zip ts ms )
 putList ls
 return ls

testCros :: (Show t, Gene t oGen, Cros t oCro) => t -> oGen -> oCro -> IO [TermLog t]
testCros _ oGen oCro = do
 ts <- runRand $ generateIt oGen
 let ps = pairs ts
 cs <- runRand $ forM ps (\(t1,t2)-> crossIt oCro t1 t2)  
 let ls = concatMap (\((t1,t2),(t1',t2')) -> 
                       [LogGenom t1  , LogLine1 ,
                        LogGenom t2  , LogLine2 , 
                        LogGenom t1' , LogLine1 , 
                        LogGenom t2' , LogLine3] ) ( zip ps cs )
 putList ls
 return ls


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

instance Gene KTree  KTreeGen  where generateIt = kTreeGen
instance Muta KTree  KTreeMut  where mutateIt   = kTreeMut
instance Cros KTree  KTreeCro  where crossIt    = kTreeCro

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

data DistMut o     = DiM_ o 
data DistCro o     = DiC_OnePoint o
data DistGen o     = DiG_Uniform o Len
 
data KTreeGen      = KG_Koza KEnv
data KTreeMut      = KM_Koza KEnv
data KTreeCro      = KC_Koza

type KEnv          = (KTerminals,KNonterminals)
type KTerminals    = [String]
type KNonterminals = [(String,Arity)]
type Arity         = Int

data BoolGen       = BG_
data BoolMut       = BM_Not
                   | BM_Prob      Prob
data BoolCro       = BC_
      
data DoubleGen     = DG_Uniform   (Double,Double)
                   | DG_Normal    (Mean,  StdDev)
data DoubleMut     = DM_Uniform   (Double,Double)
                   | DM_Normal    (Mean,  StdDev)
                   | DM_NormalAbs (Mean,  StdDev)
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


distGen :: (Gene t o) => DistGen o -> Rand [Dist t]
distGen (DiG_Uniform opt len) = 
 let gOpt = LG_ ( PG_Both opt (DG_Uniform (0,1) ) ) len
  in do 
    xss <- generateIt gOpt 
    return $ map mkDist xss


distMut :: (Muta t o) => DistMut o -> Dist t -> Rand (Dist t)
distMut (DiM_ opt) dist = 
 let mOpt = LM_OnePoint ( PM_Both opt (DM_NormalAbs (0,1) ) ) (distSize dist)
  in mkDist `liftM` mutateIt mOpt (distToList 1 dist)

distCro :: (Cros t o) => DistCro o -> Dist t -> Dist t -> Rand ( Dist t , Dist t )
distCro (DiC_OnePoint _) x y =
 let cOpt = LC_OnePoint () (distSize x)
  in do
   let x' = distToList 1 x
       y' = distToList 1 y 
   (a,b) <- crossIt cOpt x' y'
   return ( mkDist a , mkDist b )   

kTreeCro :: KTreeCro -> KTree -> KTree -> Rand (KTree,KTree)
kTreeCro KC_Koza tree1 tree2 = do
  cPos1 <- crossPos tree1
  cPos2 <- crossPos tree2
  let sub1          = kSubtree tree1 cPos1
      (tree2',sub2) = kChangeSubtree tree2 cPos2 sub1
      (tree1',_   ) = kChangeSubtree tree1 cPos1 sub2
  return (tree1',tree2')
 where
  crossPos :: KTree -> Rand KPos
  crossPos tree = do
   poses <- let (ts,ns) = kPoses2 tree 
             in if null ns 
                 then return ts 
                 else randCase 0.9 ns ts
   getRandomL poses

kTreeMut :: KTreeMut -> KTree -> Rand KTree
kTreeMut (KM_Koza kEnv) tree = do
 newSub <- kTreeGenOne kEnv
 mutPos <- getRandomL $ kPoses tree
 return . fst $ kChangeSubtree tree mutPos newSub

kTreeGen :: KTreeGen -> Rand [KTree]
kTreeGen (KG_Koza kEnv) = infRand $ kTreeGenOne kEnv 

kTreeGenOne :: KEnv -> Rand KTree
kTreeGenOne (terminals,nonterminals) = do
  isFullMethod <- getRandom
  maximalDepth <- getRandomL [1..6]
  genOne 0 maximalDepth isFullMethod
 where
  ts = map (\t->(t,0)) terminals
  ns = nonterminals
  cs = ts ++ ns
  genOne :: Int -> Int -> Bool -> Rand KTree
  genOne depth maxDepth fullMet
    | depth == 0        = genOne' ns
    | depth == maxDepth = genOne' ts
    | fullMet           = genOne' ns
    | otherwise         = genOne' cs
   where 
    genOne' :: [(String,Arity)] -> Rand KTree
    genOne' xs = do
     (name,arity) <- getRandomL xs
     trees <- mapM (\_->genOne (depth+1) maxDepth fullMet ) [1..arity]
     return $ KNode name trees

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
 DM_Uniform   range  -> (x+)         `liftM` getRandomR range
 DM_Normal    params -> (x+)         `liftM` getNormal  params
 DM_NormalAbs params -> (abs . (x+)) `liftM` getNormal  params

doubleCro :: DoubleCro -> Double -> Double -> Rand (Double,Double)
doubleCro dc x y = case dc of
 DC_Avg    -> let arith = (x + y) / 2
                  harmo = if x /= 0 && y /= 0 then 2 / ( 1/x + 1/y ) else arith
               in return (arith,harmo)
 DC_GeoAvg -> let gm = sqrt ( (abs x) * (abs y) ) 
               in return ( gm , -gm )



-- BitGenom -----------------------------




type BoolListProblem = Problem [Bool] () (ListGen BoolGen) (ListMut BoolMut) (ListCro () )

boolListProblem :: (Double,Double,Double) -> PopSize -> Len -> ([Bool]->FitVal) -> BoolListProblem
boolListProblem eParams popSize len ff = 
  Problem popSize (mkEOpt eParams) (LG_ BG_ len) (LM_OnePoint BM_Not len ) (LC_OnePoint () len) (FF1 $ return . ff)  ()
  


test       = putEvolveMaximas 1000 $ boolListProblem (   33,   33,   33) 20 100 ff2 -- 49    , 56 
testByMeta = putEvolveMaximas 1000 $ boolListProblem ( 1.93,94.85, 3.22) 2  100 ff2 --       , 98.01
testByMeta2= putEvolveMaximas 1000 $ boolListProblem (8.921,87.83,3.245) 2  100 ff2 -- 98.01 , 90
testByMeta3= putEvolveMaximas 1000 $ boolListProblem (5.040,94.56,0.396) 2  100 ff2 -- 
testByMeta4= putEvolveMaximas 1000 $ boolListProblem (20.39,74.26,5.341) 3  100 ff2
testByMeta5= putEvolveMaximas 1000 $ boolListProblem (6.962,88.36,4.680) 2  100 ff2 -- 96.04
testByMeta6= putEvolveMaximas 1000 $ boolListProblem (0.489,98.82,0.692) 2  100 ff2
testExtrapo= putEvolveMaximas 1000 $ boolListProblem (0    ,  100,    0) 2  100 ff2

normalize :: Credit -> [Double] -> ( (Double,Double,Double) , Int )
normalize credit [rep,mut,cro,gSize,gNum] =
 let q = 100.0 / (rep+mut+cro)
     popSize = round . sqrt $ credit * (gSize / gNum)
  in ( (rep*q,mut*q,cro*q) , popSize )


test2_1 = test2 ( (   33,   33,   33) , 20 ) -- 46, 47, 41, 49
test2_2 = test2 ( (5.819,85.73,8.442) ,  2 ) -- 77, 69, 64, 71
test2_3 = test2 ( (7.827,87.79,4.378) ,  3 ) -- 74, 72, 77, 69, 62, 69

test2 ( ePars , popSize ) = 
 let len     = 100
     eOpt    = mkEOpt ePars
     gOpt    = LG_         (PG_Both BG_    (DG_Normal (0,1)) ) len
     mOpt    = LM_OnePoint (PM_Both BM_Not (DM_Normal (0,1)) ) len
     cOpt    = LC_OnePoint ()                                  len 
  in putEvolveMaximas 500 $ Problem popSize eOpt gOpt mOpt cOpt (FF1 $ return . ff3) ()

metaTest2 = 
 let len      = 100
     gOpt     = LG_         (PG_Both BG_    (DG_Normal (0,1)) ) len
     mOpt     = LM_OnePoint (PM_Both BM_Not (DM_Normal (0,1)) ) len  
     cOpt     = LC_OnePoint ()                                  len
     ff       = FF1 $ return . ff3
     inCredit = 500
  in putEvolveMaximas 25000 $ metaProblem (gOpt,mOpt,cOpt,ff,(),inCredit)


testFF4 = 
 let popSize = 20
     len     = 2
     eOpt    = mkEOpt (33,33,33)
     gOpt    = DiG_Uniform (DG_Normal (0,1) ) len
     mOpt    = DiM_ ( DM_Normal (0,1) )
     cOpt    = DiC_OnePoint ()
  in putEvolveMaximas 20000 $ Problem popSize eOpt gOpt mOpt cOpt (FF1 ff4') () -- (return . ff4)

type MetaProblem = Problem [Double] () (ListGen DoubleGen) (ListMut DoubleMut) (ListCro ())
type MetaParams t a g m c = (g,m,c,FitFun2 t a,a,Credit) 


metaTest = 
 let len      = 100
     gOpt     = LG_ BG_ len  
     mOpt     = LM_OnePoint BM_Not len  
     cOpt     = LC_OnePoint () len
     ff       = FF1 $ return . ff2
     inCredit = 500
  in putEvolveMaximas 25000 $ metaProblem (gOpt,mOpt,cOpt,ff,(),inCredit)

metaProblem :: Evolvable t a g m c => MetaParams t a g m c -> MetaProblem
metaProblem params =
 let popSize = 20
     len     = 5 -- == length [rep,mut,cro,gSize,gNum] 
     eOpt    = mkEOpt (33,33,33)
     gOpt    = LG_ ( DG_Uniform (0,1) ) len
     mOpt    = LM_OnePoint ( DM_NormalAbs (0,1) ) len
     cOpt    = LC_OnePoint () len
  in Problem popSize eOpt gOpt mOpt cOpt (metaFF params) () 
 
metaFF :: Evolvable t a g m c => MetaParams t a g m c -> FitFun2 [Double] () -- [Double] -> Rand FitVal
metaFF (gOpt,mOpt,cOpt,ff,a,credit) = FF1 $ \ [rep,mut,cro,gSize,gNum] -> 
 let innerProblem = 
      let ratio   = gSize / gNum
          popSize = round . sqrt $ credit * ratio
          eOpt    = mkEOpt (rep,mut,cro)
       in Problem popSize eOpt gOpt mOpt cOpt ff a 
  in do 
   ds <- evolveIt innerProblem credit
   if null ds
    then return 0
    else let Just (_,ffVal) = distMax . last $ ds
          in return ffVal


testKoza ff env as = 
 let eOpt    = mkEOpt (10,0,90)
     popSize = 500
     gOpt    = KG_Koza env
     mOpt    = KM_Koza env
     cOpt    = KC_Koza 
  in putEvolveMaximas 25000 $ Problem popSize eOpt gOpt mOpt cOpt ff as


koza_ssr = testKoza ff_koza_ssr env_koza_ssr (as::Double->Double)

env_koza_ssr :: KEnv
env_koza_ssr = (["x"],[("plus",2),("minus",2),("krat",2),("rdiv",2),("sin",1),("cos",1),("exp",1),("rlog",1)])

ff_koza_ssr :: FitFun2 KTree (Double->Double)
ff_koza_ssr = FF2 toStr (as::Double->Double) (return . ff)
 where
  toStr :: KTree -> String
  toStr tree = "(\\ x -> " ++ show tree ++ ")"
  ff :: (Double->Double) -> FitVal
  ff f = (1/) . (1+) . sum . map (\x-> let dx = (f x) - ( x*x*x*x+x*x*x+x*x+x ) in dx*dx ) $ [-1,-0.9..1] 



ff_Koza0 :: FitFun2 KTree Double
ff_Koza0 = FF2 show (as::Double) (\i -> return $ 1/ (1+abs (123 - i)) )


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
 let xs = fst $ runState (distTake_new (100 * distSize d) d) (mkStdGen 42) 
  in 1 / ( 1 + abs (sum xs) ) 

ff4' :: Dist Double -> Rand FitVal
ff4' d = do 
 xs <- (distTake_new (5 * distSize d) d) 
 return $ 1 / ( 1 + abs (sum xs) ) 


dLen :: [a] -> Double
dLen = fromIntegral . length 

