{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module GP_Data where

import GP_Core ( Gene, Muta, Cros, generateIt, mutateIt, crossIt , Prob )
import Util    ( Ral, getRandom, getRandomR, getRandomL, getNormal, randIf, randCase )

import KozaTree ( KTree(KNode), KPos, kSubtree, kChangeSubtree, kPoses, kPoses2, kDepth ) 
import Dist     ( Dist, mkDist, distSize, distToList )


import Control.Monad (replicateM, liftM, liftM2, forM)

type Len    = Int
type Mean   = Double
type StdDev = Double

-- KTree --------------------------------------------------------------------------

instance Gene KTree KTreeGen where generateIt = kTreeGen
instance Muta KTree KTreeMut where mutateIt   = kTreeMut
instance Cros KTree KTreeCro where crossIt    = kTreeCro

data KTreeGen = KG_Koza KEnv
data KTreeMut = KM_Koza KEnv
data KTreeCro = KC_Koza

type KEnv          = (KTerminals,KNonterminals)
type KTerminals    = [String]
type KNonterminals = [(String,Arity)]
type Arity         = Int

kTreeGenOne :: KEnv -> Ral KTree
kTreeGenOne (terminals,nonterminals) = do
  isFullMethod <- getRandom
  maximalDepth <- getRandomL [1..6]
  genOne 0 maximalDepth isFullMethod
 where
  ts = map (\t->(t,0)) terminals
  ns = nonterminals
  cs = ts ++ ns
  genOne :: Int -> Int -> Bool -> Ral KTree
  genOne depth maxDepth fullMet
    | depth == 0        = genOne' ns
    | depth == maxDepth = genOne' ts
    | fullMet           = genOne' ns
    | otherwise         = genOne' cs
   where 
    genOne' :: [(String,Arity)] -> Ral KTree
    genOne' xs = do
     (name,arity) <- getRandomL xs
     trees <- mapM (\_->genOne (depth+1) maxDepth fullMet ) [1..arity]
     return $ KNode name trees

kTreeGen :: Int -> KTreeGen -> Ral [KTree]
kTreeGen n (KG_Koza kEnv) = replicateM n $ kTreeGenOne kEnv 

kTreeCro :: KTreeCro -> KTree -> KTree -> Ral (KTree,KTree)
kTreeCro KC_Koza tree1 tree2 = do
  cPos1 <- crossPos tree1
  cPos2 <- crossPos tree2
  let sub1          = kSubtree tree1 cPos1
      (tree2',sub2) = kChangeSubtree tree2 cPos2 sub1
      (tree1',_   ) = kChangeSubtree tree1 cPos1 sub2
      child1        = if kDepth tree1' > maxDepth then tree1 else tree1'
      child2        = if kDepth tree2' > maxDepth then tree2 else tree2'
  return (child1,child2)
 where
  maxDepth = 17
  crossPos :: KTree -> Ral KPos
  crossPos tree = do
   poses <- let (ts,ns) = kPoses2 tree 
             in if null ns 
                 then return ts 
                 else randCase 0.9 ns ts
   getRandomL poses

kTreeMut :: KTreeMut -> KTree -> Ral KTree
kTreeMut (KM_Koza kEnv) tree = do
 newSub <- kTreeGenOne kEnv
 mutPos <- getRandomL $ kPoses tree
 return . fst $ kChangeSubtree tree mutPos newSub

-- trivials ------------------------------------------------------------------------

instance Gene t t  where generateIt n x  = return $ replicate n x
instance Muta t () where mutateIt _ x    = return x
instance Muta t t  where mutateIt o _    = return o
instance Cros t () where crossIt _ x y   = return (x,y)

-- pair ---------------------------------------------------------------------------

instance (Gene t1 o1,Gene t2 o2) => Gene (t1,t2) (PairGen o1 o2) where generateIt = pairGen 
instance (Muta t1 o1,Muta t2 o2) => Muta (t1,t2) (PairMut o1 o2) where mutateIt   = pairMut
instance (Cros t1 o1,Cros t2 o2) => Cros (t1,t2) (PairCro o1 o2) where crossIt    = pairCro

data PairGen o1 o2 = PG_Both o1 o2 
data PairMut o1 o2 = PM_Both o1 o2
data PairCro o1 o2 = PC_Both o1 o2

pairGen :: (Gene t1 o1,Gene t2 o2) => Int -> PairGen o1 o2 -> Ral [(t1,t2)]
pairGen n (PG_Both o1 o2) = liftM2 zip (generateIt n o1) (generateIt n o2)

pairMut :: (Muta t1 o1,Muta t2 o2) => PairMut o1 o2 -> (t1,t2) -> Ral (t1,t2)
pairMut (PM_Both o1 o2) (x,y) = liftM2 (,) (mutateIt o1 x) (mutateIt o2 y)

pairCro :: (Cros t1 o1,Cros t2 o2) => PairCro o1 o2 -> (t1,t2) -> (t1,t2) -> Ral ((t1,t2),(t1,t2))
pairCro(PC_Both o1 o2) (x1,x2) (y1,y2) = do
 (a1,b1) <- crossIt o1 x1 y1
 (a2,b2) <- crossIt o2 x2 y2
 return $ ( (a1,b2) , (b1,a2) )

-- list ------------------------------------------------------------------

instance (Gene t o) => Gene [t] (ListGen o) where generateIt = listGen 
instance (Muta t o) => Muta [t] (ListMut o) where mutateIt   = listMut
instance (Cros t o) => Cros [t] (ListCro o) where crossIt    = listCro 

data ListGen opt   = LG_          opt Len
data ListMut opt   = LM_OnePoint  opt Len
                   | LM_Prob      opt Len Prob
data ListCro opt   = LC_OnePoint  opt Len

listGen :: (Gene term opt) => Int -> ListGen opt -> Ral [[term]]
listGen n (LG_ opt len) = cutUpList `liftM` generateIt (n*len) opt 
 where
  cutUpList :: [a] -> [[a]]
  cutUpList [] = [] 
  cutUpList terms = 
   let ( listTerm, terms' ) = splitAt len terms
    in listTerm : cutUpList terms'

listMut :: (Muta term opt) => ListMut opt -> [term] -> Ral [term]
listMut lm genom = case lm of
 LM_OnePoint opt len -> do
  pos <- getRandomR (0,len-1)
  let (xs,y:ys) = splitAt pos genom 
  (\y' -> xs ++ ( y' : ys ) ) `liftM` mutateIt opt y
 LM_Prob opt len p -> 
  forM genom $ \ bit -> randIf p (mutateIt opt bit) (return bit)

listCro :: (Cros term opt) => ListCro opt -> [term] -> [term] -> Ral ([term],[term])
listCro lc x y = case lc of
 LC_OnePoint _ len -> do
  cutPos <- getRandomR (0,len)
  let ( x1 , x2 ) = splitAt cutPos x
      ( y1 , y2 ) = splitAt cutPos y
  return $ ( x1 ++ y2 , y1 ++ x2 )

-- Bool ---------------------------------------------------------

instance Gene Bool BoolGen where generateIt = boolGen
instance Muta Bool BoolMut where mutateIt   = boolMut 
instance Cros Bool BoolCro where crossIt    = boolCro

data BoolGen       = BG_
data BoolMut       = BM_Not
                   | BM_Prob Prob
data BoolCro       = BC_

boolGen :: Int -> BoolGen -> Ral [Bool]
boolGen n _ = replicateM n getRandom

boolMut :: BoolMut -> Bool -> Ral Bool
boolMut bm x = case bm of
 BM_Not    -> return $ not x
 BM_Prob p -> randCase p (not x) x

boolCro :: BoolCro -> Bool -> Bool -> Ral (Bool,Bool)
boolCro _ x y = return ( x && y , x || y )

-- Double -------------------------------------------------------

instance Gene Double DoubleGen where generateIt = doubleGen 
instance Muta Double DoubleMut where mutateIt   = doubleMut 
instance Cros Double DoubleCro where crossIt    = doubleCro

data DoubleGen     = DG_Uniform   (Double,Double)
                   | DG_Normal    (Mean,  StdDev)
data DoubleMut     = DM_Uniform   (Double,Double)
                   | DM_Normal    (Mean,  StdDev)
                   | DM_NormalAbs (Mean,  StdDev)
data DoubleCro     = DC_Avg  
                   | DC_GeoAvg  

doubleGen :: Int -> DoubleGen -> Ral [Double]
doubleGen n dg = case dg of
 DG_Uniform range  -> replicateM n $ getRandomR range
 DG_Normal  params -> replicateM n $ getNormal  params

doubleMut :: DoubleMut -> Double -> Ral Double
doubleMut dm x = case dm of
 DM_Uniform   range  -> (x+)         `liftM` getRandomR range
 DM_Normal    params -> (x+)         `liftM` getNormal  params
 DM_NormalAbs params -> (abs . (x+)) `liftM` getNormal  params

doubleCro :: DoubleCro -> Double -> Double -> Ral (Double,Double)
doubleCro dc x y = case dc of
 DC_Avg    -> let arith = (x + y) / 2
                  harmo = if x /= 0 && y /= 0 then 2 / ( 1/x + 1/y ) else arith
               in return (arith,harmo)
 DC_GeoAvg -> let gm = sqrt ( (abs x) * (abs y) ) 
               in return ( gm , -gm )

-- Dist ---------------------------------------------------------

instance (Gene t o) => Gene (Dist t) (DistGen o) where generateIt = distGen
instance (Muta t o) => Muta (Dist t) (DistMut o) where mutateIt   = distMut
instance (Cros t o) => Cros (Dist t) (DistCro o) where crossIt    = distCro

data DistMut o     = DiM_ o 
data DistCro o     = DiC_OnePoint o
data DistGen o     = DiG_Uniform o Len

distGen :: (Gene t o) => Int -> DistGen o -> Ral [Dist t]
distGen n (DiG_Uniform opt len) = 
 let gOpt = LG_ ( PG_Both opt (DG_Uniform (0,1) ) ) len
  in do 
    xss <- generateIt n gOpt 
    return $ map mkDist xss

distMut :: (Muta t o) => DistMut o -> Dist t -> Ral (Dist t)
distMut (DiM_ opt) dist = 
 let mOpt = LM_OnePoint ( PM_Both opt (DM_NormalAbs (0,1) ) ) (distSize dist)
  in mkDist `liftM` mutateIt mOpt (distToList 1 dist)

distCro :: (Cros t o) => DistCro o -> Dist t -> Dist t -> Ral ( Dist t , Dist t )
distCro (DiC_OnePoint _) x y =
 let cOpt = LC_OnePoint () (distSize x)
  in do
   let x' = distToList 1 x
       y' = distToList 1 y 
   (a,b) <- crossIt cOpt x' y'
   return ( mkDist a , mkDist b )   
