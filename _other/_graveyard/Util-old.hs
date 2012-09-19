module Util
( newSymbol
, (+++)
, putList
, maximas
) where

import Data.List

type Symbol = String


infixr 7 +++
(+++) :: (a->[b]) -> (a->[b]) -> (a->[b])
(f +++ g) x = f x ++ g x


putList :: (Show a) => [a] -> IO ()
putList [] = putStrLn "[]"
putList xs = foldr1 (>>) (map (putStrLn.show) xs) 


maximas :: Ord a => [a] -> [a]
maximas (x:xs) = x : f x xs
  where
  f _   []     = []
  f max (x:xs) = if x > max then x : f x xs else f max xs


newSymbol :: [Symbol] -> Symbol
newSymbol vns = snd . head $ dropWhile (\(x,y)->x==y) 
  (zip ((sortBy nameOrdering vns') ++ repeat "") (map (slovo abeceda) [1..]))
  where
  vns' = filter (all (`elem` abeceda )) vns 
  abeceda = ['a'..'z']

-- Uspoøádání primárnì podle délky, sekundárnì podle abecedy.
nameOrdering :: Ord a => [a] -> [a] -> Ordering
nameOrdering x y
    | lo /= EQ  = lo
    | otherwise = compare x y
    where
    lo = lenOrdering x y

-- Uspoøádání podle délky.
lenOrdering :: [a] -> [a] -> Ordering
lenOrdering []     []      = EQ
lenOrdering []     _       = LT
lenOrdering _      []      = GT
lenOrdering (_:xs) (_:ys)  = lenOrdering xs ys

-- Vrací n-té slovo (napsané abecedou `abc`) ve výše zmínìném
-- nameOrdering.
slovo :: [Char] -> Integer -> String
slovo abc n 
	| n == 0    = ""
	| otherwise = slovo' abc (n-1)
	where
	slovo' :: [Char] -> Integer -> String
	slovo' abc n
		| n <  0      = error "moc maly!"
		| n>=0 && n<d = [pismeno n]
		| otherwise   = slovo' abc ( div n d - 1 ) ++ 
						[ pismeno (mod n d) ]
		where
		d = toInteger $ length abc 
		pismeno :: Integer -> Char
		pismeno n = abc !! (fromInteger n)



