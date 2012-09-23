{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module GP_Instances where

import GP_Classes ( Gene, Muta, Cros, generateIt, mutateIt, crossIt , Prob )
import KozaTree   ( KTree(KNode), KPos, kSubtree, kChangeSubtree, kPoses, kPoses2 ) 
import Util       ( Rand, getRandom, getRandomL, getRandomR, randIf, randCase, infRand, getNormal )
import Dist       ( Dist, mkDist, distSize, distToList )

import Control.Monad ( liftM, liftM2, forM )

type Len    = Int
type Mean   = Double
type StdDev = Double

-- instances ------------------------------------------------------------------------

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

