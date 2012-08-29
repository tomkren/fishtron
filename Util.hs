{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- Util obsahuje obecné funkce funkce nad standardními typy.

module Util
( putList
, insertToListMap , lookupInListMap
, maximas , maximasBy
, newSymbol , newSymbol'
, nthWord
, (+++)
, pairs
, fill , fillStr
, Queue , emptyQueue , insertQueue , insertsQueue , popQueue , nullQueue , singletonQueue
, Rand  , randLift , getRandom , getRandomR , runRand 
        , infChainRand , infRand , randCase, randIf ,getNormal, getRandomL
        , RunRand
) where

import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import System.Random
import Data.Random.Normal
import Control.Monad.State
import Data.Functor.Identity

-- "Zřetězení funkcí"
(+++) :: (a->[b]) -> (a->[b]) -> (a->[b])
(f +++ g) x = f x ++ g x
infixr 7 +++

pairs :: [a] -> [(a,a)]
pairs []  = []
pairs [x] = []
pairs (x:y:rest) = (x,y) : pairs rest

-- Pro přehledné vykreslení seznamu, záznam na řádek
putList :: (Show a) => [a] -> IO ()
putList []     = return ()
putList (x:xs) = do
 putStrLn . show $ x
 putList xs 

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
maximas []     = []
maximas (x:xs) = x : f x xs
  where
  f _   []     = []
  f max (x:xs) = if x > max then x : f x xs else f max xs

maximasBy :: (a->a->Bool) -> [a] -> [a]
maximasBy _  []     = []
maximasBy lt (x:xs) = x : f x xs
  where
  f _    []     = []
  f maxi (x:xs) = if maxi `lt` x then x : f x xs else f maxi xs


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

-- queue ----------------------------------------------------------

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

-- rand ----------------------------------------------------------------

type Rand  = StateT StdGen Identity 

class RunRand m where
 runRand :: m a -> IO a

instance RunRand Rand where 
 runRand rand = do
  gen <- getStdGen
  return . fst $ runState rand gen

randLift :: (RandomGen g , MonadState g m ) => (g -> (a,g)) -> m a
randLift f = do
 gen <- get
 let (val,gen') = f gen
 put gen'
 return val

getRandom :: (RandomGen g , MonadState g m , Random a)  => m a
getRandom = randLift random

getRandomR :: (RandomGen g, MonadState g m ,Random a) => (a,a) -> m a
getRandomR range = randLift $ randomR range

getRandomL :: (RandomGen g, MonadState g m ) => [a] -> m a
getRandomL [] = error "Empty list in getRandomL."
getRandomL xs = do
 i <- getRandomR (0,length xs - 1)
 return $ xs !! i

getNormal :: (RandomGen g , MonadState g m ) => (Random a, Floating a) => (a, a) -> m a
getNormal params@( mean , stdDeviation ) = randLift $ normal' params

randCase :: (RandomGen g, MonadState g m ) => Double -> a -> a -> m a
randCase p ok ko = do
 p' <- getRandomR (0.0 , 1.0)
 return $ if p' < p then ok else ko

randIf :: (RandomGen g, MonadState g m) => Double -> m a -> m a -> m a
randIf p ok ko = do
 p' <- getRandomR (0.0 , 1.0)
 if p' < p then ok else ko

infRand :: (RandomGen g, MonadState g m ) => m a -> m [a]
infRand rand = do
   gen <- get
   let (gen1,gen2) = split gen
   put gen1
   xs <- inf rand
   put gen2
   return xs
 where
  inf :: (RandomGen g , MonadState g m ) => m a -> m [a]
  inf r = do
   x  <- r
   xs <- inf r
   return $ x:xs

infChainRand :: (RandomGen g , MonadState g m ) => (a -> m a) -> a -> m [a]
infChainRand f x = do
   gen <- get
   let (gen1,gen2) = split gen
   put gen1
   xs <- inf f x
--       ( xs , _  ) = runState ( inf f x ) gen1
   put gen2
   return $ x:xs
 where
  inf :: (RandomGen g , MonadState g m ) => (a -> m a) -> a -> m [a]
  inf f x = do
   x' <- f x
   xs <- inf f x'
   return $ x':xs 


{--
type Rand a = State StdGen a

randLift :: (StdGen -> (a,StdGen)) -> Rand a
randLift f = do
 gen <- get
 let (val,gen') = f gen
 put gen'
 return val

infRand :: Rand a -> Rand [a]
infRand rand = do
   gen <- get
   let (gen1,gen2) = split gen
   put gen1
   xs <- inf rand
   put gen2
   return xs
 where
  inf :: Rand a -> Rand [a]
  inf r = do
   x  <- r
   xs <- inf r
   return $ x:xs


infChainRand :: (a -> Rand a) -> a -> Rand [a]
infChainRand f x = do
   gen <- get
   let (gen1,gen2) = split gen
   put gen1
   xs <- inf f x
--       ( xs , _  ) = runState ( inf f x ) gen1
   put gen2
   return $ x:xs
 where
  inf :: (a -> Rand a) -> a -> Rand [a]
  inf f x = do
   x' <- f x
   xs <- inf f x'
   return $ x':xs 


test_infChainRand :: Rand [Int]
test_infChainRand =  infChainRand  (\x-> (\r->x+2*r-1) `liftM` getRandomR (0,1)  ) (1000::Int)
  

randCase :: Double -> a -> a -> Rand a
randCase p ok ko = do
 p' <- getRandomR (0.0 , 1.0)
 return $ if p' < p then ok else ko

randIf :: Double -> Rand a -> Rand a -> Rand a
randIf p ok ko = do
 p' <- getRandomR (0.0 , 1.0)
 if p' < p then ok else ko


getRandom :: Random a => Rand a
getRandom = randLift random

getRandomR :: Random a => (a,a) -> Rand a
getRandomR range = randLift $ randomR range

getRandomL :: [a] -> Rand a
getRandomL xs = do
 i <- getRandomR (0,length xs - 1)
 return $ xs !! i

getNormal :: (Random a, Floating a) => (a, a) -> Rand a
getNormal params@( mean , stdDeviation ) = randLift $ normal' params

mkRand :: Int -> Rand ()
mkRand i = put $ mkStdGen i 

runRand' :: Int -> Rand a -> a
runRand' i rand = fst $ runState rand (mkStdGen i)

runRand :: Rand a -> IO a
runRand rand = do
 gen <- getStdGen
 return . fst $ runState rand gen

--}




