module SelfProblem where

import Data.List
import Test.QuickCheck

if' :: Bool -> a -> a -> a
if' p q r = if p then q else r 

listCase :: [a] -> b -> (a->[a]->b) -> b
listCase as b1 b2 = case as of
 []   -> b1
 x:xs -> b2 x xs 

maybeCase :: Maybe a -> b -> (a->b) -> b
maybeCase ma b1 b2 = case ma of
 Nothing -> b1
 Just a  -> b2 a

pair :: a -> b -> (a,b)
pair x y = (x,y) 

pairCase :: (a,b) -> (a->b->c) -> c
pairCase (x,y) f = f x y 

{-- 
  "Axiomy" :
  
  True      :: Bool
  False     :: Bool
  if'       :: Bool -> a -> a -> a 

  Nothing   :: Maybe a
  Just      :: a -> Maybe a
  maybeCase :: Maybe a -> b -> (a->b) -> b

  []        :: [a]
  (:)       :: a -> [a] -> [a]
  listCase  :: [a] -> b -> (a->[a]->b) -> b

  pair      :: a -> b -> (a,b)
  pairCase  :: (a,b) -> (a->b->c) -> c
 
  (<=)      :: Ord a => a -> a -> Bool
  (==)      :: Eq  a => a -> a -> Bool 
   
  foldr     :: (a -> b -> b) -> b -> [a] -> b  
 

--}


{- 

  -- head , tail :
  listCase   :: [Int] -> Maybe Int -> (Int->[Int]->Maybe Int) -> Maybe Int
  Nothing    :: Maybe Int
  Just       :: a -> Maybe Int  

  -- map :
  foldr      :: (Int->[Int]->[Int]) -> [Int] -> [Int] -> [Int]
  (:)        :: Int -> [Int] -> [Int]
  []         :: [Int]

  -- filter:
  if'        :: Bool -> [Int] -> [Int] -> [Int] 
  (foldr,[],(:))

-}


my_head :: [a] -> Maybe a
my_head xs = listCase xs Nothing (\x _->Just x)

my_tail :: [a] -> Maybe [a]
my_tail xs = listCase xs Nothing (\_ t->Just t) 

my_map :: (a->b) -> [a] -> [b]
my_map f xs = foldr (\x acc -> f x : acc) [] xs

my_filter :: (a->Bool) -> [a] -> [a]
my_filter p xs = foldr (\x acc->if'(p x) (x:acc) acc) [] xs 

my_null :: [a] -> Bool
my_null xs = listCase xs True (\_ _->False) 

my_elem ::Eq a => a -> [a] -> Bool 
my_elem x ys = foldr (\y acc-> if' (y==x) True acc ) False ys 

my_min :: Ord a => a -> a -> a
my_min a b = if' ( a <= b ) a b 

my_max :: Ord a => a -> a -> a
my_max a b = if' ( b <= a ) a b 

my_minimum :: Ord a => [a] -> Maybe a
my_minimum xs = foldr (\ x y -> maybeCase y (Just x) (\ z -> if' (x<=z) (Just x) y ) ) Nothing xs



my_insert :: Ord a => a -> [a] -> [a]
my_insert x xs = listCase xs [x] (\y ys -> if'(y<=x) (y : my_insert x ys) (x:y:ys) )


-- ffs : --

ff_head :: ( [Int] -> Maybe Int ) -> Double
ff_head prog = 
  ( if prog []       == Nothing then 1 else 0 ) +
  ( if prog [1]      == Just 1  then 1 else 0 ) +
  ( if prog [42,7,3] == Just 42 then 1 else 0 )

ff_tail :: ( [Int] -> Maybe [Int] ) -> Double
ff_tail prog = 
  ( if prog []       == Nothing     then 1 else 0 ) +
  ( if prog [1]      == Just []     then 1 else 0 ) +
  ( if prog [42,7,3] == Just [7,3]  then 1 else 0 ) +
  ( if prog [1..5]   == Just [2..5] then 1 else 0 ) +




-------------------------------------------------------------------------


--my_insert a xs = 
-- pairCase 
--  ( foldr 
--      (\x acc -> pairCase acc (\ toi ret -> maybeCase toi (toi,x:ret) (\i -> if'(x<=i)(Nothing,x:i:ret)(toi,x:ret)) ) ) 
--      (Just a,[]) 
--      xs
--  ) 
--  ( \ k l -> maybeCase k l (\m->m:l) )

--help1 :: Ord a => a -> [a] -> [a]
--help1 f1 f2 p x xs = listCase xs (f1 x) (\y ys -> if' (p y x) (y : my_insert x ys) (f3 x y ys) )



{--
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = ( qsort $ filter (<=x) xs ) ++ [x] ++ ( qsort $ filter (>x) xs )  


sort2 :: Ord a => [a] -> [a]
sort2 xs = foldr zarad [] xs
  where
    zarad :: Ord a => a -> [a] -> [a]
    zarad x acc = undefined 
--}




--head stuff :

ff_head_qc :: ( [Int] -> Maybe Int ) -> IO Double
ff_head_qc prog = do 
 result <- quickCheckWithResult (stdArgs{chatty=False}) $ prop_isHead prog
 return . fromIntegral . numTests $ result 


prop_isHead prog xs = prog xs == my_head xs

fake_head :: [a] -> Maybe a
fake_head _ = Nothing

false_head [] = Just 1
false_head _  = Nothing
