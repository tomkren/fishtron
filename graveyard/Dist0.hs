module Dist 
( Dist( )
, mkDist
, distMax
, distTake
--, distTakeBySize
, distSize
) where

import System.Random

d1::[(Char,Double)]
d1 = [('1',1),('2',2),('3',4),('4',8),('5',16)]


data Dist a  = Dist Double (DTree a)
data DTree a =  DLeaf a | DNode Double (DTree a) (DTree a) deriving (Show)


mkDist :: [(a,Double)] -> Dist a
mkDist xs = let (fxs,sum) = f xs in Dist (sum/100.0) $ tree3 $ tree1 $ fxs
  where
  tree3 :: [(DTree a,Double)] -> DTree a
  tree3 [(d,_)]  = d
  tree3 xs@(_:_) = tree3 $ tree2 xs 
  tree2 ::  [(DTree a,Double)] -> [(DTree a,Double)]
  tree2 xs = case xs of
    []                     -> []
    [x]                    -> [x]
    ((d1,v1):(d2,v2):rest) -> (DNode v1 d1 d2 ,v2) : (tree2 rest)
  tree1 :: [(a,Double)] -> [(DTree a,Double)]
  tree1 = map (\(a,v) -> (DLeaf a , v) ) 
  f :: [(a,Double)] -> ([(a,Double)],Double)
  f xs = let (xs',sum) = f' 0 xs in ( map (\(a,v)->(a,v*(100.0/sum))) xs' , sum )
    where
    f' acc []     = ([],acc)
    f' acc (x@(a,v):xs) = let (xs',acc') = f' (acc + v) xs in ((a,(acc + v)) : xs' , acc' )   

distMax :: Dist a -> (a,Double)
distMax (Dist q tree) = let (a,v) = distMax' 0 100 tree in (a,q*v)
  where
  distMax' min max (DLeaf a)          = (a,max-min)
  distMax' min max (DNode mark t1 t2) = if val1 > val2 then r1 else r2 
    where
    r1@(a1 , val1) = distMax' min mark t1  
    r2@(a2 , val2) = distMax' mark max t2  

distSize :: Dist a -> Int
distSize (Dist _ tree) = size tree
  where
  size (DLeaf _) = 1
  size (DNode _ t1 t2) = size t1 + size t2

distTake :: (RandomGen g) => g -> Int -> Dist a -> ( [a] , g )
distTake gen 0 d = ( []   , gen   )
distTake gen n d = ( x:xs , gen'' )
  where
  (x , gen' ) = distGet  gen         d
  (xs, gen'') = distTake gen' (n-1)  d

distGet :: (RandomGen g) => g -> Dist a -> ( a , g )
distGet gen (Dist q tree) = ( dTreeGet tree x , gen' ) 
  where (x,gen') = randomR (0.0,100.0) gen

distTakeBySize :: (RandomGen g) => g -> Dist a -> ( [a] , g )
distTakeBySize gen dist = distTake gen (distSize dist) dist

getDTree :: Dist a -> DTree a
getDTree (Dist _ tree) = tree

dTreeGet :: DTree a -> Double -> a
dTreeGet tree x = dTreeGet' tree 
  where
  dTreeGet' (DLeaf val) = val
  dTreeGet' (DNode mark d1 d2)
    | x < mark  = dTreeGet' d1
    | otherwise = dTreeGet' d2

showDist :: (Show a) => Dist a -> String
showDist d@(Dist q tree) = (showDTree 0 100 q tree) ++ 
  "\n---------------------------------------------------\n" ++
  ( let (a,v) = distMax d in show a ++ " ... " ++ show (v/q) ++ "%  ="++ show v ) ++
  "\n---------------------------------------------------\n"


showDTree min max q (DLeaf x) 
  = show x ++ " ... " ++ show (max-min) ++ " % = " ++ show (q*(max-min))
showDTree min max q (DNode mark t1 t2) 
  = (showDTree min mark q t1) ++ "\n" ++ (showDTree mark max q t2) 
 
instance (Show a) => Show (Dist a) where
  show = showDist



