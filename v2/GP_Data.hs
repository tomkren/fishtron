{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}

module GP_Data where

import Eva (Eva,evaSplitStdGen)
import GP_Core ( Gene, Muta, Cros, generateIt, mutateIt, crossIt , Prob )
import Utils   ( getRandom, getRandomR, getRandomL, getNormal, randIf, randCase,logIt,boxIt,boxThem )

import TTerm
import TTree
--
import IM ( SearchOptions , prove , kozaSearchOptions , allEdgesSearchOptions , geomSearchOptions )
import System.Random (StdGen)

import InhabitationMachines ( proveN , randProveN, randProveUnique, proveOneWithLimit, Limit, kozaProveN )
--
import KozaTree ( KTree(KNode), KPos, kSubtree, kChangeSubtree, kPoses, kPoses2, kDepth ) 
import Dist     ( Dist, mkDist, distSize, distToList )


import Control.Monad (replicateM, liftM, liftM2, forM)

import Data.List     (nub,groupBy)

type Len    = Int
type Mean   = Double
type StdDev = Double

-- CTT -------------------------------------------------------------------------

instance Gene CTT CTTGen where generateIt = cttGen
instance Cros CTT CTTCro where crossIt    = cttCro2

data CTTGen  
 = CTTG_Koza     Typ Context Limit  -- pomocí starýho IM 
 | CTTG_Koza2    Typ Context        -- a zbytek pomocí novýho IM ..
 | CTTG_AllEdges Typ Context 
 | CTTG_Geom     Typ Context Prob 

data CTTCro = CTTC_Koza

cttGen :: Int -> CTTGen -> Eva [CTT]
cttGen n opt = case opt of
  CTTG_AllEdges typ ctx   -> proveIt  allEdgesSearchOptions n typ ctx 
  CTTG_Koza2    typ ctx   -> proveIt  kozaSearchOptions     n typ ctx 
  CTTG_Geom     typ ctx p -> proveIt (geomSearchOptions p)  n typ ctx
  CTTG_Koza typ ctx limit ->  map mkCTT `liftM` kozaProveN n limit typ ctx
 where
  proveIt :: (Int -> Typ -> Context -> StdGen -> SearchOptions) -> Int -> Typ -> Context -> Eva [CTT]
  proveIt f n typ ctx = do
    gen <- evaSplitStdGen
    return . prove $ f n typ ctx gen



cttCro :: CTTCro -> CTT -> CTT -> Eva (CTT,CTT)
cttCro CTTC_Koza (CTT v1 tree1) (CTT v2 tree2) = do
  cPos1 <- crossPos tree1
  cPos2 <- crossPos tree2
  let sub1          = ttreeSubtree tree1 cPos1
      (tree2',sub2) = ttreeChangeSubtree tree2 cPos2 sub1
      (tree1',_   ) = ttreeChangeSubtree tree1 cPos1 sub2
      child1        = if ttreeDepth tree1' > maxDepth then tree1 else tree1'
      child2        = if ttreeDepth tree2' > maxDepth then tree2 else tree2'
  return ( (CTT v1 child1) , (CTT v2 child2) )
 where
  maxDepth = 17
  crossPos :: TTree -> Eva TTPos
  crossPos tree = do
   poses <- let (ts,ns) = ttreePoses2 tree 
             in if null ns 
                 then return ts 
                 else randCase 0.9 ns ts
   getRandomL poses

cttCro2 :: CTTCro -> CTT -> CTT -> Eva (CTT,CTT)
cttCro2 CTTC_Koza ctt1@(CTT v1 tree1) ctt2@(CTT v2 tree2) = do
  (cPos1,typ) <- crossPos1 tree1
  maybe_cPos2 <- crossPos2 tree2 typ
  case maybe_cPos2 of
   Nothing -> return ( ctt1 , ctt2 )
   Just cPos2 -> do
    let sub1          = ttreeSubtree tree1 cPos1
        (tree2',sub2) = ttreeChangeSubtree tree2 cPos2 sub1
        (tree1',_   ) = ttreeChangeSubtree tree1 cPos1 sub2
        child1        = if ttreeDepth tree1' > maxDepth then tree1 else tree1'
        child2        = if ttreeDepth tree2' > maxDepth then tree2 else tree2'
    return ( (CTT v1 child1) , (CTT v2 child2) )
 where
  maxDepth = 17
  crossPos1 :: TTree -> Eva (TTPos,Typ)
  crossPos1 tree = do
   poses <- let (ts,ns) = ttreePoses2WithTyps tree 
             in if null ns 
                 then return ts 
                 else randCase 0.9 ns ts
   getRandomL poses
  crossPos2 :: TTree -> Typ -> Eva (Maybe TTPos)
  crossPos2 tree typ = do
   poses <- let (ts,ns) = ttreePoses2ByTyp typ tree -- ttreePoses2 tree 
             in if null ns 
                 then return ts 
                 else randCase 0.9 ns ts
   if null poses 
    then return Nothing 
    else Just `liftM` getRandomL poses

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

kTreeGenOne :: KEnv -> Eva KTree
kTreeGenOne (terminals,nonterminals) = do
  isFullMethod <- getRandom
  maximalDepth <- getRandomL [1..6]
  genOne 0 maximalDepth isFullMethod
 where
  ts = map (\t->(t,0)) terminals
  ns = nonterminals
  cs = ts ++ ns
  genOne :: Int -> Int -> Bool -> Eva KTree
  genOne depth maxDepth fullMet
    | depth == 0        = genOne' ns
    | depth == maxDepth = genOne' ts
    | fullMet           = genOne' ns
    | otherwise         = genOne' cs
   where 
    genOne' :: [(String,Arity)] -> Eva KTree
    genOne' xs = do
     (name,arity) <- getRandomL xs
     trees <- mapM (\_->genOne (depth+1) maxDepth fullMet ) [1..arity]
     return $ KNode name trees

kTreeGen :: Int -> KTreeGen -> Eva [KTree]
kTreeGen n (KG_Koza kEnv) = replicateM n $ kTreeGenOne kEnv 

kTreeCro :: KTreeCro -> KTree -> KTree -> Eva (KTree,KTree)
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
  crossPos :: KTree -> Eva KPos
  crossPos tree = do
   poses <- let (ts,ns) = kPoses2 tree 
             in if null ns 
                 then return ts 
                 else randCase 0.9 ns ts
   getRandomL poses

kTreeMut :: KTreeMut -> KTree -> Eva KTree
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
data PairMut o1 o2 = PM_Both o1 o2 |
                     PM_One  o1 o2 |
                     PM_OneP o1 o2 Prob
data PairCro o1 o2 = PC_Both o1 o2

pairGen :: (Gene t1 o1,Gene t2 o2) => Int -> PairGen o1 o2 -> Eva [(t1,t2)]
pairGen n (PG_Both o1 o2) = liftM2 zip (generateIt n o1) (generateIt n o2)

pairMut :: (Muta t1 o1,Muta t2 o2) => PairMut o1 o2 -> (t1,t2) -> Eva (t1,t2)
pairMut opt (x,y) = case opt of 
 PM_Both o1 o2 -> liftM2 (,) (mutateIt o1 x) (mutateIt o2 y)
 PM_One  o1 o2 -> do
  first <- getRandom
  if first then (,y) `liftM` mutateIt o1 x 
           else (x,) `liftM` mutateIt o2 y
 PM_OneP  o1 o2 p ->
  randIf p ( (,y) `liftM` mutateIt o1 x ) 
           ( (x,) `liftM` mutateIt o2 y )           


pairCro :: (Cros t1 o1,Cros t2 o2) => PairCro o1 o2 -> (t1,t2) -> (t1,t2) -> Eva ((t1,t2),(t1,t2))
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
                   | LM_Swap          Len
data ListCro opt   = LC_OnePoint  opt Len

listGen :: (Gene term opt) => Int -> ListGen opt -> Eva [[term]]
listGen n (LG_ opt len) = cutUpList `liftM` generateIt (n*len) opt 
 where
  cutUpList :: [a] -> [[a]]
  cutUpList [] = [] 
  cutUpList terms = 
   let ( listTerm, terms' ) = splitAt len terms
    in listTerm : cutUpList terms'

listMut :: (Muta term opt) => ListMut opt -> [term] -> Eva [term]
listMut lm genom = case lm of
 LM_OnePoint opt len -> do
  pos <- getRandomR (0,len-1)
  let (xs,y:ys) = splitAt pos genom 
  (\y' -> xs ++ ( y' : ys ) ) `liftM` mutateIt opt y
 LM_Prob opt len p -> 
  forM genom $ \ bit -> randIf p (mutateIt opt bit) (return bit)
 LM_Swap len -> do
  let randomPos = getRandomR (0,len-1)
  pos1 <- randomPos
  pos2 <- randomPos
  return $ swap pos1 pos2 genom

swap :: Int -> Int -> [a] -> [a]
swap pos1 pos2 xs
 | pos1 < pos2 = let (as,b:bs) = splitAt pos1 xs
                     (cs,d:ds) = splitAt (pos2-pos1-1) bs
                  in as ++ [d] ++ cs ++ [b] ++ ds
 | pos1 > pos2 = swap pos2 pos1 xs
 | otherwise   = xs
 

listCro :: (Cros term opt) => ListCro opt -> [term] -> [term] -> Eva ([term],[term])
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

boolGen :: Int -> BoolGen -> Eva [Bool]
boolGen n _ = replicateM n getRandom

boolMut :: BoolMut -> Bool -> Eva Bool
boolMut bm x = case bm of
 BM_Not    -> return $ not x
 BM_Prob p -> randCase p (not x) x

boolCro :: BoolCro -> Bool -> Bool -> Eva (Bool,Bool)
boolCro _ x y = return ( x && y , x || y )

-- Int ----------------------------------------------------------

instance Gene Int IntGen where generateIt = intGen
instance Muta Int IntMut where mutateIt   = intMut

data IntGen = IG_Uniform (Int,Int)

intGen :: Int -> IntGen -> Eva [Int]
intGen n ig = case ig of
 IG_Uniform range -> replicateM n $ getRandomR range

data IntMut = IM_Uniform (Int,Int)

intMut :: IntMut -> Int -> Eva Int
intMut im x = case im of
 IM_Uniform range -> getRandomR range


-- Double -------------------------------------------------------

instance Gene Double DoubleGen where generateIt = doubleGen 
instance Muta Double DoubleMut where mutateIt   = doubleMut 
instance Cros Double DoubleCro where crossIt    = doubleCro

data DoubleGen = DG_Uniform   (Double,Double)
               | DG_Normal    (Mean,  StdDev)
data DoubleMut = DM_Uniform   (Double,Double)
               | DM_Normal    (Mean,  StdDev)
               | DM_NormalAbs (Mean,  StdDev)
data DoubleCro = DC_Avg  
               | DC_GeoAvg  

doubleGen :: Int -> DoubleGen -> Eva [Double]
doubleGen n dg = case dg of
 DG_Uniform range  -> replicateM n $ getRandomR range
 DG_Normal  params -> replicateM n $ getNormal  params

doubleMut :: DoubleMut -> Double -> Eva Double
doubleMut dm x = case dm of
 DM_Uniform   range  -> (x+)         `liftM` getRandomR range
 DM_Normal    params -> (x+)         `liftM` getNormal  params
 DM_NormalAbs params -> (abs . (x+)) `liftM` getNormal  params

doubleCro :: DoubleCro -> Double -> Double -> Eva (Double,Double)
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

distGen :: (Gene t o) => Int -> DistGen o -> Eva [Dist t]
distGen n (DiG_Uniform opt len) = 
 let gOpt = LG_ ( PG_Both opt (DG_Uniform (0,1) ) ) len
  in do 
    xss <- generateIt n gOpt 
    return $ map mkDist xss

distMut :: (Muta t o) => DistMut o -> Dist t -> Eva (Dist t)
distMut (DiM_ opt) dist = 
 let mOpt = LM_OnePoint ( PM_Both opt (DM_NormalAbs (0,1) ) ) (distSize dist)
  in mkDist `liftM` mutateIt mOpt (distToList 1 dist)

distCro :: (Cros t o) => DistCro o -> Dist t -> Dist t -> Eva ( Dist t , Dist t )
distCro (DiC_OnePoint _) x y =
 let cOpt = LC_OnePoint () (distSize x)
  in do
   let x' = distToList 1 x
       y' = distToList 1 y 
   (a,b) <- crossIt cOpt x' y'
   return ( mkDist a , mkDist b )   

-- TTerm --------------------------------------------------------------------------

instance Gene TTerm TTermGen where generateIt = ttermGen
instance Cros TTerm TTermCro where crossIt    = ttermCro

data TTermGen = 
 TTG_IM_rand  Typ Context Int  |
 TTG_IM_syst  Typ Context      |
 TTG_IM_rand0 Typ Context Int  |
 TTG_SKI      Typ Context Int

data TTermCro = 
 TTC_my Context |
 TTC_SKI


ttermGen :: Int -> TTermGen -> Eva [TTerm] 
ttermGen n opt = case opt of
 TTG_IM_syst  typ ctx       -> return $ proveN n       typ ctx
 TTG_IM_rand  typ ctx limit -> randProveUnique n limit typ ctx
 TTG_IM_rand0 typ ctx limit -> randProveN      n limit typ ctx
 TTG_SKI      typ ctx limit -> (map toSki) `liftM` (randProveUnique n limit typ ctx)

ttermCro :: TTermCro -> TTerm -> TTerm -> Eva (TTerm,TTerm)
ttermCro (TTC_my ctx) tt1 tt2 = xover ctx tt1 tt2
ttermCro TTC_SKI tt1 tt2 = skiXover tt1 tt2


-- Vyžaduje aby to neobsahovalo proměnný
skiXover :: TTerm -> TTerm -> Eva ( TTerm , TTerm )
skiXover tt1 tt2 = do
 let candidates = compatibleSubterms tt1 tt2
 if null candidates then return (tt1,tt2)
  else do
   i <- getRandomR (0,length candidates -1)
   let ( TTZ t1 ds1 , TTZ t2 ds2 ) = candidates !! i
   return ( tzGoTop (TTZ t2 ds1) , tzGoTop (TTZ t1 ds2) )



xover :: Context -> TTerm -> TTerm -> Eva ( TTerm , TTerm )
xover ctx t1 t2 = do 
 let candidates = compatibleSubterms t1 t2  --  <---------------- tady se voli smart/normal
 if null candidates then return (t1,t2)
  else do
   i <- getRandomR (0, length candidates - 1 )
   let (z1,z2) = candidates !! i 
   -- boxThem $ ["XOVER : what is being swaped",show z1,show z2]
   xover' ctx z1 z2  
   


xover_old :: Context -> TTerm -> TTerm -> Eva ( TTerm , TTerm )
xover_old ctx t1 t2 = 
  let candidates = compatibleSubterms t1 t2  --  <---------------- tady se voli smart/normal
      canSize    = length candidates
   in if null candidates 
      then do return $ (t1,t2) 
      else do
        i <- getRandomR (0, canSize - 1 )
        let (z1,z2) = candidates !! i 
        ret <- xover' ctx z1 z2
        return ret


xover' :: Context -> TTermZipper -> TTermZipper -> Eva ( TTerm , TTerm )
xover' ctx tz1@(TTZ t1 ds1) tz2@(TTZ t2 ds2) = do 
 t1' <- treatFVs ctx (TTZ t1 ds2)
 t2' <- treatFVs ctx (TTZ t2 ds1)
 let pars = ( tzGoTop tz1 , tzGoTop tz2 ) 
 chis <- case (t1',t2') of
  (Just tt1 , Just tt2) -> return ( makeVarsUnique $ tzGoTop (TTZ tt2 ds1) , makeVarsUnique $ tzGoTop (TTZ tt1 ds2) )
  _ -> do
   -- boxIt " >>> Default value fail ! <<< " 
   return pars
 return $ checkXover pars chis

checkXover :: (TTerm,TTerm) -> (TTerm , TTerm) -> (TTerm , TTerm)
checkXover parents chs@(ch1,ch2) = 
 if checkTyp ch1 then
  if checkTyp ch2 then
   chs
  else err 
 else err
 where 
 err = error $ "xover typeCheck ERR: " ++
  show parents ++ " -> " ++ show chs

treatFVs :: Context -> TTermZipper -> Eva (Maybe TTerm)
treatFVs ctx (TTZ t ds) = f (fv' t) t
  where
  boundVarsInDs :: Context  
  boundVarsInDs = foldl (\acc x -> case x of 
    Lamb sym (typ:->_) -> if has acc sym
                          then           acc 
                          else (sym,typ):acc 
    _                  ->                acc ) [] ds
  has :: Context -> Symbol -> Bool
  has xs s = any (\(s',_)->s==s') xs
  f :: [(Symbol,Typ)] -> TTerm  -> Eva (Maybe TTerm)
  f [] t = return (Just t)
  f ((fv,fvTyp):fvs) t = 
    if null withSameTyp
    then case defaultValue fvTyp ctx of
      Nothing     -> return Nothing
      Just defVal -> f fvs (subs fv t defVal)
      -- f fvs $ subs fv t (defaultValue fvTyp ctx) 
    else do
      i <- getRandomR ( 0 , length withSameTyp - 1 )
      f fvs $ subs fv t $ TVar (withSameTyp !! i) fvTyp
    where
    withSameTyp = map fst $ filter (\(_,t) -> t == fvTyp ) boundVarsInDs 
  fv' :: TTerm -> [(Symbol,Typ)]
  fv' (TVar v   typ) = [(v,typ)]
  fv' (TVal v   _  ) = []
  fv' (TApp p q _  ) = nub $ (fv' p) ++ (fv' q) 
  fv' (TLam v p typ) = filter (\(s,_)->s/=v) (fv' p)   -- neefektivni

defaultValue :: Typ -> Context -> Maybe TTerm
defaultValue typ ctx = proveOneWithLimit 10000 typ ctx -- head $ proveN 1 typ ctx

compatibleSubterms :: TTerm -> TTerm ->  [ (TTermZipper,TTermZipper) ]
compatibleSubterms t1 t2 = concatMap (\(as,bs) -> [ (a,b) | a <- as , b <- bs ] ) 
  $ f (groupenSubterms t1) (groupenSubterms t2) 
  where
  f :: [(Typ,[TTermZipper])] -> [(Typ,[TTermZipper])] -> [([TTermZipper],[TTermZipper])]
  f []            _  = []
  f ((typ,ts):xs) ys = case lookup typ ys of
    Nothing  -> f xs ys
    Just ts' -> ( ts , ts' ) : f xs ys

groupenSubterms :: TTerm -> [(Typ,[TTermZipper])]
groupenSubterms t = map (\ts -> ( ttzTyp $ head ts ,ts) ) 
  $ groupBy (\t1 t2 -> ttzTyp t1 == ttzTyp t2 ) $ subterms t 

subterms :: TTerm -> [TTermZipper]
subterms term = subzips (TTZ term []) 
  where
  subzips :: TTermZipper -> [TTermZipper]
  subzips z@(TTZ t _) = case t of
    TVar _   _ -> [z]
    TVal _   _ -> [z]
    TLam _ _ _ -> z : ( subzips $ tzDown z )
    TApp _ _ _ -> z : ( subzips (tzLeft z) ++ subzips (tzRight z) )

