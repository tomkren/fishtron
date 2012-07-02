module Dist 
( Dist( )
, mkDist
, mkDist2
, distMax
, distTake
, distCut
, distObtain
, distSize
) where

import Util

import System.Random
import Text.Printf
import Data.List

d1 = mkDist [('A',1),('B',2),('C',4),('D',8),('E',16)]
d2 = mkDist $ [ (i,(1.113)**i) | i<-[1..64] ]

d3 = mkDist2 [(0,1),(1,2),(2,4),(4,16)] [(interv (2,4),8),(interv (4,6),32)]


interv :: (Double,Double) -> Double -> Double
interv (a,b) x = (b-a)*x + a

type Suma    = Double
type Max     = Double
type Percent = Double
type CutP    = Double
type Size    = Int

data Dist  a = Dist (DTree a) (Suma,Size) | DEmpty  
data DTree a = DLeaf  (a,Double)
             | DLeaf2 (Double->a,Double)
             | DNode  Double (DTree a) (DTree a) 
             --deriving (Show) 

mkDist :: [(a,Double)] -> Dist a
mkDist xs = mkDist2 xs []

mkDist2 :: [(a,Double)] -> [(Double->a,Double)]  -> Dist a
mkDist2 xs ys = case tree3 $ tree2 $ (tree1 xs) ++ (tree1' ys) of 
  Nothing      -> DEmpty
  Just (t,sum) -> Dist t (sum,length xs) 
  where
  tree1 :: [(a,Double)] -> [(DTree a,Double)]
  tree1 = map (\(a,v) -> (DLeaf (a,v) , v) ) 

  tree1' :: [(Double->a,Double)] -> [(DTree a,Double)]
  tree1' = map (\(f,v) -> (DLeaf2 (f,v) , v) ) 

  tree2 ::  [(DTree a,Double)] -> [(DTree a,Double)]
  tree2 []  = []
  tree2 [x] = [x]
  tree2 ((d1,v1):(d2,v2):rest)  
   = let sum = v1+v2 in (DNode (v1/sum) d1 d2 , sum ) : (tree2 rest)

  tree3 :: [(DTree a,Double)] -> Maybe (DTree a,Double)
  tree3 []  = Nothing 
  tree3 [x] = Just x
  tree3 xs  = tree3 $ tree2 xs 

{--
mkDist :: [(a,Double)] -> Dist a
mkDist xs = case tree3 $ tree2 $ tree1 $ sortedXS of 
  Nothing      -> DEmpty
  Just (t,sum) -> Dist t (sum,length xs) 
  where
  sortedXS = sortBy (\(_,x) (_,y)->compare (-x) (-y)) xs 
  tree1 :: [(a,Double)] -> [(DTree a,Double)]
  tree1 = map (\(a,v) -> (DLeaf (a,v) , v) ) 

  tree2 ::  [(DTree a,Double)] -> [(DTree a,Double)]
  tree2 []  = []
  tree2 [x] = [x]
  tree2 ((d1,v1):(d2,v2):rest)  
   = let sum = v1+v2 in (DNode (v1/sum) d1 d2 , sum ) : (tree2 rest)

  tree3 :: [(DTree a,Double)] -> Maybe (DTree a,Double)
  tree3 []  = Nothing 
  tree3 [x] = Just x
  tree3 xs  = tree3 $ tree2 xs 
--}

distGet :: Dist a -> Double -> a
distGet (Dist t _) = get t
  where 
  get :: DTree a -> Double -> a
  get (DLeaf  (a,_) ) _ = a
  get (DLeaf2 (f,_) ) x = f x
  get (DNode mark t1 t2) x 
    = if x < mark 
      then get t1 (  x       /  mark    )
      else get t2 ( (x-mark) / (1-mark) ) 

distPop :: Dist a -> Double -> Maybe ( a , Dist a )
distPop DEmpty _ = Nothing
distPop (Dist (DLeaf  (x,_)) _ ) _    = Just ( x      , DEmpty )
distPop (Dist (DLeaf2 (f,_)) _ ) find = Just ( f find , DEmpty )
distPop (Dist t (sum,size)) find 
  = let ((ret,val),part,t') = pop t find 
     in Just (ret, Dist t' (sum-val,size-1) )
  where
  pop :: DTree a -> Double -> ( (a,Double) , Double , DTree a)
  pop (DNode p t1 t2) find = if find < p 
    then case t1 of
      DLeaf  x       -> ( x            , p , t2 )
      DLeaf2 (f,val) -> ( (f find,val) , p , t2 )
      _       -> let ( ret , pp , t1' ) = pop t1 (find/p)
                     pp' = pp*p
                  in ( ret , pp' , DNode (norm (p-pp') (1-p)) t1' t2 )
    else case t2 of
      DLeaf  x       -> ( x            , 1-p , t1 )
      DLeaf2 (f,val) -> ( (f find,val) , 1-p , t1 )
      _       -> let ( ret , pp , t2' ) = pop t2 ((find-p)/(1-p))
                     pp' = pp*(1-p)
                  in ( ret , pp' , DNode (norm p (1-p-pp') ) t1 t2' ) 
    where
    norm a b = a/(a+b)
      
rand01 :: (RandomGen g) => g -> (Double,g)
rand01 = randomR (0.0,1.0)

randP :: (RandomGen g) => Double -> g -> (Bool,g)
randP p gen = let (r,gen') = rand01 gen
               in ( r < p , gen') 

distGet' :: (RandomGen g) => g -> Dist a -> ( a , g )
distGet' gen dist = ( distGet dist x , gen' ) 
  where (x,gen') = rand01 gen
                            
distPop' :: (RandomGen g) => g -> Dist a -> Maybe ( a , Dist a , g )
distPop' gen dist = do 
   ( ret , dist' ) <- distPop dist x
   return ( ret , dist' , gen' ) 
  where
  (x , gen' ) = rand01 gen

distObt' :: (RandomGen g) => CutP -> g -> Dist a -> Maybe ( a , Dist a , g )
distObt' _    _   DEmpty = Nothing 
distObt' cutP gen dist
  = let ( doCut , gen' ) = randP cutP gen
     in if doCut 
         then distPop' gen' dist 
         else let (ret,gen'') = distGet' gen' dist in Just (ret,dist,gen'')

 
distTake :: (RandomGen g) => g -> Int -> Dist a -> ( [a] , g )
distTake gen 0 _ = ( []   , gen   )
distTake gen n d = ( x:xs , gen'' )
  where
  (x , gen' ) = distGet' gen         d
  (xs, gen'') = distTake gen' (n-1)  d

distCut :: (RandomGen g) => g -> Int -> Dist a -> ( [a] , Dist a , g )
distCut gen 0 d = ( []   , d  , gen  )
distCut gen n d = case distPop' gen d of 
   Nothing -> 
        ( []   , d  , gen  )
   Just ( x    , d' , gen' ) -> 
    let (   xs , d'', gen'') = distCut gen' (n-1) d'
     in ( x:xs , d'', gen'')

distMetaCut :: (RandomGen g) => ( g -> Dist a -> Maybe ( a , Dist a , g )) 
                             -> g -> Int -> Dist a -> ( [a] , Dist a , g )
distMetaCut cutF gen 0 d = ( []   , d  , gen  )
distMetaCut cutF gen n d = case cutF gen d of 
   Nothing -> 
        ( []   , d  , gen  )
   Just ( x    , d' , gen' ) -> 
    let (   xs , d'', gen'') = distMetaCut cutF gen' (n-1) d'
     in ( x:xs , d'', gen'')



distObtain :: (RandomGen g) => g -> (Percent,CutP) -> Dist a -> ( [a] , Dist a , g )
distObtain gen (percent,cutP) dist = distMetaCut (distObt' cutP) gen howMany dist
  where howMany = let part = round $ percent * (fromIntegral $ distSize dist)
                   in if part < 1 then 1 else part

distMax :: Dist a -> Maybe (a,Double)
distMax DEmpty           = Nothing
distMax (Dist t (sum,_)) = Just $ distMax' sum t
  where
  distMax' :: Double -> DTree a -> (a,Double)
  distMax' _ (DLeaf x     ) = x
  distMax' _ (DLeaf2 (f,v)) = (f 0.5,v)                              -- TODO domyslet líp, ne 0.5kou 
  distMax' part (DNode mark t1 t2) = if part1 > part2 then r1 else r2 
    where
    r1@(_ , part1) = distMax' (part*mark    ) t1  
    r2@(_ , part2) = distMax' (part*(1-mark)) t2   
  
distSize :: Dist a -> Int
distSize (Dist _ (_,size)) = size

getDTree :: Dist a -> DTree a
getDTree (Dist t _) = t


prettyRound :: Double -> Double
prettyRound x = if abs (x-x') > 1e-14 
  then x else x'
  where x' = fromInteger $ round x

showDist :: (Show a) => Dist a -> String
showDist DEmpty       = "<EMPTY DIST>"
showDist (Dist t (sum,size)) = "\n" ++
   showDTree sum t ++
   "\n----------------------" ++ 
   --"\n [max]  "  ++ show a ++ " : " ++ (show $ prettyRound max) ++
   --                           " : " ++ showNice (max*100/sum) ++ "%" ++ 
   "\n [sum]  "  ++ (show sum) ++ 
   "\n [size] " ++ (show size) ++
   "\n"
  where 
  showNice = printf "%.2f"
  showDTree :: (Show a) => Double -> DTree a -> String  
  showDTree part (DLeaf x) = show x  -- ++ " : " ++ (show $ prettyRound part) 
                                    ++ " : " ++ showNice (part*100/sum) ++ "%" 
  showDTree part (DLeaf2 (f,v)) = "(<" ++ show (f 0.5) ++ ">," ++ show v ++ ")"
                                    ++ " : " ++ showNice (part*100/sum) ++ "%" 
  showDTree part (DNode mark t1 t2)
   = (showDTree (part*mark) t1) ++ "\n" ++ (showDTree (part*(1-mark)) t2)   

instance (Show a) => Show (Dist a) where show = showDist

