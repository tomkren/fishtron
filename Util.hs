-- Util obsahuje obecné funkce funkce nad standardními typy.

module Util
( putList
, insertToListMap , lookupInListMap
, maximas
, newSymbol , newSymbol'
, nthWord
, (+++)
, fill , fillStr
, Queue , emptyQueue , insertQueue , insertsQueue , popQueue , nullQueue , singletonQueue
) where

import Data.List
import qualified Data.Map as Map
import Data.Map (Map)

-- "Zřetězení funkcí"
(+++) :: (a->[b]) -> (a->[b]) -> (a->[b])
(f +++ g) x = f x ++ g x
infixr 7 +++

-- Pro přehledné vykreslení seznamu, záznam na řádek
putList :: (Show a) => [a] -> IO ()
putList [] = putStrLn "[]"
putList xs = foldr1 (>>) (map (putStrLn.show) xs) 

insertToListMap :: (Ord k) => k -> a -> Map k [a] -> Map k [a]
insertToListMap key val listMap 
 = Map.insertWith (\[x] xs -> x:xs) key [val] listMap 

lookupInListMap :: (Ord k) => k -> Map k [a] -> [a]
lookupInListMap key listMap 
 = case Map.lookup key listMap of
  Nothing -> []
  Just xs -> xs

-- Vrací rostoucí seznam dosavadních maxim v seznamu
maximas :: Ord a => [a] -> [a]
maximas (x:xs) = x : f x xs
  where
  f _   []     = []
  f max (x:xs) = if x > max then x : f x xs else f max xs

-- Pro již použitá slova vrací první takové, které je od všech rozdílné
newSymbol :: [String] -> String
newSymbol = newSymbol' ['a'..'z'] 

newSymbol' :: [Char] -> [String] -> String
newSymbol' abeceda vns = snd . head $ dropWhile (\(x,y)->x==y) 
   (zip ((sortBy nameOrdering vns') ++ repeat "") (map (nthWord abeceda) [1..]))
 where
  vns' = filter (all (`elem` abeceda )) vns 

-- Uspořádání primárnì podle délky, sekundárnì podle abecedy.
nameOrdering :: Ord a => [a] -> [a] -> Ordering
nameOrdering x y
    | lo /= EQ  = lo
    | otherwise = compare x y
    where
    lo = lenOrdering x y

-- Uspořádání podle délky.
lenOrdering :: [a] -> [a] -> Ordering
lenOrdering []     []      = EQ
lenOrdering []     _       = LT
lenOrdering _      []      = GT
lenOrdering (_:xs) (_:ys)  = lenOrdering xs ys

-- Vrací n-té slovo (napsané abecedou `abc`) ve výše zmíněném nameOrdering.
nthWord :: [Char] -> Integer -> String
nthWord abc n 
	| n == 0    = ""
	| otherwise = nthWord' abc (n-1)
	where
	nthWord' :: [Char] -> Integer -> String
	nthWord' abc n
		| n < 0       = error "moc maly!"
		| n>=0 && n<d = [pismeno n]
		| otherwise   = nthWord' abc ( div n d - 1 ) ++ 
						[ pismeno (mod n d) ]
		where
		d = toInteger $ length abc 
		pismeno :: Integer -> Char
		pismeno n = abc !! (fromInteger n)

fill :: [(Int,a)] -> (Int,Int) -> a -> [(Int,a)]
fill [] (i,j) val = map (\n->(n,val)) [i..j]
fill xx@((k,x):xs) (i,j) x' 
 | k > i     = (i,x') : fill xx (i+1,j) x'
 | otherwise = (k,x ) : fill xs (i+1,j) x'  

fillStr :: Int -> String -> String
fillStr len str = let l = length str in str ++ [' '|_<-[0..len-l-1]] 

data Queue a = Queue [a] [a] 

instance Show a => Show (Queue a) where
 show (Queue xs ys) = (++) "queue " $ show $ ys ++ (reverse xs)

nullQueue :: Queue a -> Bool
nullQueue (Queue [] []) = True
nullQueue _ = False

emptyQueue :: Queue a
emptyQueue = Queue [] []

singletonQueue :: a -> Queue a
singletonQueue x = Queue [x] []

insertQueue :: a -> Queue a -> Queue a
insertQueue x (Queue xs ys) = Queue (x:xs) ys

insertsQueue :: [a] -> Queue a -> Queue a
insertsQueue xs' (Queue xs ys) = Queue (xs' ++ xs) ys


popQueue :: Queue a -> Maybe (a, Queue a)
popQueue ( Queue [] []     ) = Nothing
popQueue ( Queue xs []     ) = Just (y , Queue [] ys ) where (y:ys) = reverse xs
popQueue ( Queue xs (y:ys) ) = Just (y , Queue xs ys )


