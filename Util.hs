-- Util obsahuje obecné funkce funkce nad standardními typy.

module Util
( putList
, maximas
, newSymbol
, nthWord
, (+++)
) where

import Data.List


-- "Zřetězení funkcí"
(+++) :: (a->[b]) -> (a->[b]) -> (a->[b])
(f +++ g) x = f x ++ g x
infixr 7 +++

-- Pro přehledné vykreslení seznamu, záznam na řádek
putList :: (Show a) => [a] -> IO ()
putList [] = putStrLn "[]"
putList xs = foldr1 (>>) (map (putStrLn.show) xs) 

-- Vrací rostoucí seznam dosavadních maxim v seznamu
maximas :: Ord a => [a] -> [a]
maximas (x:xs) = x : f x xs
  where
  f _   []     = []
  f max (x:xs) = if x > max then x : f x xs else f max xs

-- Pro již použitá slova vrací první takové, které je od všech rozdílné
newSymbol :: [String] -> String
newSymbol vns = snd . head $ dropWhile (\(x,y)->x==y) 
  (zip ((sortBy nameOrdering vns') ++ repeat "") (map (nthWord abeceda) [1..]))
  where
  vns' = filter (all (`elem` abeceda )) vns 
  abeceda = ['a'..'z']

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



