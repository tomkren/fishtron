-- Util obsahuje obecn� funkce funkce nad standardn�mi typy.

module Util
( putList
, maximas
, newSymbol
, (+++)
) where

import Data.List


-- "Z�et�zen� funkc�"
(+++) :: (a->[b]) -> (a->[b]) -> (a->[b])
(f +++ g) x = f x ++ g x
infixr 7 +++

-- Pro p�ehledn� vykreslen� seznamu, z�znam na ��dek
putList :: (Show a) => [a] -> IO ()
putList [] = putStrLn "[]"
putList xs = foldr1 (>>) (map (putStrLn.show) xs) 

-- Vrac� rostouc� seznam dosavadn�ch maxim v seznamu
maximas :: Ord a => [a] -> [a]
maximas (x:xs) = x : f x xs
  where
  f _   []     = []
  f max (x:xs) = if x > max then x : f x xs else f max xs

-- Pro ji� pou�it� slova vrac� prvn� takov�, kter� je od v�ech rozd�ln�
newSymbol :: [String] -> String
newSymbol vns = snd . head $ dropWhile (\(x,y)->x==y) 
  (zip ((sortBy nameOrdering vns') ++ repeat "") (map (nthWord abeceda) [1..]))
  where
  vns' = filter (all (`elem` abeceda )) vns 
  abeceda = ['a'..'z']

-- Uspo��d�n� prim�rn� podle d�lky, sekund�rn� podle abecedy.
nameOrdering :: Ord a => [a] -> [a] -> Ordering
nameOrdering x y
    | lo /= EQ  = lo
    | otherwise = compare x y
    where
    lo = lenOrdering x y

-- Uspo��d�n� podle d�lky.
lenOrdering :: [a] -> [a] -> Ordering
lenOrdering []     []      = EQ
lenOrdering []     _       = LT
lenOrdering _      []      = GT
lenOrdering (_:xs) (_:ys)  = lenOrdering xs ys

-- Vrac� n-t� slovo (napsan� abecedou `abc`) ve v��e zm�n�n�m nameOrdering.
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



