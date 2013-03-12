module SelfProblem where

import Data.List

my_if :: Bool -> a -> a -> a
my_if p q r = if p then q else r 

my_head :: [a] -> Maybe a
my_head []    = Nothing
my_head (x:_) = Just x 


{-- 
  "Axiomy" :
  
  (<=)     :: Ord a => a -> a -> Bool
  (==)     :: Eq  a => a -> a -> Bool 
  if'      :: Bool -> a -> a -> a
  head'    :: [a] -> Maybe a
  foldr    :: (a -> b -> b) -> b -> [a] -> b  
  maybe    :: b -> (a -> b) -> Maybe a -> b
  Nothing  :: Maybe a
  Just     :: a -> Maybe a
  []       :: [a]
  (:)      :: a -> [a] -> [a]
  True     :: Bool
  False    :: Bool
 

--}

my_null :: [a] -> Bool
my_null xs = listCase xs True (\_ _->False) 

my_elem ::Eq a => a -> [a] -> Bool 
my_elem x ys = foldr (\y acc-> my_if (y==x) True acc ) False ys 

my_min :: Ord a => a -> a -> a
my_min a b = my_if ( a <= b ) a b 

my_max :: Ord a => a -> a -> a
my_max a b = my_if ( b <= a ) a b 


my_minimum :: Ord a => [a] -> Maybe a
my_minimum xs = foldr (\ x y -> maybe (Just x) (\ z -> my_if (x<=z) (Just x) y ) y ) Nothing xs



my_insert :: Ord a => a -> [a] -> [a]
my_insert x []     = [x]
my_insert x (y:ys) = if y <= x then y : ( my_insert x ys ) else x:y:ys 
--lépe by poupravené : my_insert x ys = maybe (x:[]) ( my_if (y<=x) (y:(my_insert x ys)) (x:y:ys) ) (my_head ys)



listCase :: [a] -> b -> (a->[a]->b) -> b
listCase as b1 b2 = case as of
 []   -> b1
 x:xs -> b2 x xs 

maybeCase :: Maybe a -> b -> (a->b) -> b
maybeCase ma b1 b2 = case ma of
 Nothing -> b1
 Just a  -> b2 a

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