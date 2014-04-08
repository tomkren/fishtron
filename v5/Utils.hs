{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
-- , OverlappingInstances 

module Utils where

import Data.Maybe
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import System.Random
import Control.Monad.State
import Data.Random.Normal


import Text.JSON (JSValue(..))

import JSONUtils

-- general ------------------------------------

putList :: (Show a) => [a] -> IO ()
putList []     = return ()
putList (x:xs) = do
 putStrLn . show $ x
 putList xs 

asType :: a
asType = undefined

unescape :: String -> String
unescape []     = []
unescape (x:[]) = [x]
unescape (x:y:rest)
    | x == '\\' = y : unescape rest
    | otherwise = x : unescape (y : rest)

-- Pro již použitá slova vrací první takové, které je od všech rozdílné
newSymbol :: [String] -> String
newSymbol = newSymbol' ['a'..'z'] 

newSymbol' :: [Char] -> [String] -> String
newSymbol' abeceda vns = snd . head $ dropWhile (\(x,y)->x==y) 
   (zip ((sortBy (nameOrderingBy abeceda) vns') ++ repeat "") (map (nthWord abeceda) [1..]))
 where
  vns' = filter (all (`elem` abeceda )) vns 

-- Uspořádání primárne podle délky, sekundárne podle abecedy.
nameOrderingBy :: (Eq a) => [a] -> [a] -> [a] -> Ordering
nameOrderingBy abc x y
  | lo /= EQ  = lo
  | otherwise = compareByAbc abc x y
 where lo = lenOrdering x y

compareByAbc :: (Eq a) => [a] -> [a] -> [a] -> Ordering
compareByAbc _ [] [] = EQ
compareByAbc abc (x:xs) (y:ys) = case cByAbc abc x y of
  EQ -> compareByAbc abc xs ys
  x  -> x
 where 
  cByAbc :: (Eq a) => [a] -> a -> a -> Ordering
  cByAbc abc x y
   | x == y    = EQ
   | otherwise = case dropWhile (\ch-> (ch/=x) && (ch/=y) ) abc  of
      []  -> error "cByAbc : x and y are not in the alphabet"
      z:_ -> if z == x then LT else GT 

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

-- RANDOM STUFF -----------------------------------------------------

class (Monad m) => Randable m where
 randLift :: (StdGen -> (a,StdGen)) -> m a

getRandom :: (Randable m , Random a)  => m a
getRandom = randLift random

getRandomR :: (Randable m , Random a) => (a,a) -> m a
getRandomR range = randLift $ randomR range

getRandomL :: (Randable m ) => [a] -> m a
getRandomL [] = error "Empty list in getRandomL."
getRandomL xs = do
 i <- getRandomR (0,length xs - 1)
 return $ xs !! i

getNormal :: (Randable m , Random a, Floating a) => (a, a) -> m a
getNormal params@( mean , stdDeviation ) = randLift $ normal' params

randCase :: (Randable m ) => Double -> a -> a -> m a
randCase p ok ko = do
 p' <- getRandomR (0.0 , 1.0)
 return $ if p' < p then ok else ko

randIf :: (Randable m ) => Double -> m a -> m a -> m a
randIf p ok ko = do
 p' <- getRandomR (0.0 , 1.0)
 if p' < p then ok else ko


-- LOGGING STUFF -----------------------------------------------------

class (Monad m) => Logable m where
 logIt  :: String -> m ()
 logIt2 :: String -> m ()
 logIt2 x = logIt x

instance Logable IO where
 logIt  = putStrLn 
 logIt2 = putStrLn 

boxIt :: (Logable m) => String -> m ()
boxIt str = 
 let strs = lines str
     len  = min (maximum . map length $ strs) maxMsgLength
  in boxAs len AloneBox strs

data BoxType = AloneBox | TopBox | MiddleBox | BottomBox

maxMsgLength :: Int
maxMsgLength = 74

boxAs :: (Logable m) => Int -> BoxType -> [String] -> m ()
boxAs lineLen boxType strs = 
 case boxType of
  AloneBox -> do
   frame "┌┐"
   boxBody
   frame "└┘"
  TopBox -> do
   frame "┌┐"
   boxBody
   frame "├┤"
  MiddleBox -> do
   boxBody
   frame "├┤"
  BottomBox -> do
   boxBody
   frame "└┘" 
 where
  hLine = replicate (lineLen+2) '─'
  frame [c1,c2] = logIt $ ' ' : c1 : (hLine ++ [c2])
  showLine line = logIt $ " │ "++ line ++ (replicate (lineLen- length line) ' ' ) ++" │"
  boxBody = mapM_ showLine (concatMap (align lineLen) strs) 

boxThem :: (Logable m) => [String] -> m ()
boxThem strs = do
 let strs' = map lines strs
     lineLen = min (maximum . map length . concat $ strs') maxMsgLength
 case strs' of
  [] -> return ()
  [s] -> boxAs lineLen AloneBox s
  _ -> do
   boxAs lineLen TopBox (head strs')
   mapM_ (boxAs lineLen MiddleBox) (tail . init $ strs')
   boxAs lineLen BottomBox (last strs')

align :: Int -> String -> [String]
align _     []  = [] 
align width str = 
 let (line,rest) = splitAt width str
  in line : (align width rest)

-- JSON-show stuff ----

class (Show a) => JShow a where
  jshow_rc :: a -> Int
  jshow_rc _ = (-1)
  jshow_popi :: a -> JSValue
  jshow_popi _ = jsObj []
  jss_size :: a -> Maybe Int
  jss_size _ = Nothing
  jshow_js :: a -> Maybe JSValue
  jshow_js _ = Nothing
  jshow    :: a -> JSValue
  jshow x = jsObj $ l_js ++ [ 
    ("type"     , jsStr "jsonout" ) ,
    ("haskell"  , jsStr $ show x  ) ]
   where
    l_js = case jshow_js x of
     Nothing    -> []
     Just jsval -> [("js",jsval)] 


instance JShow Bool where
  jshow_js x = Just $ JSBool x 

instance JShow a => JShow [a] where
  jshow_js = Just . jsArr . map (fromJust . jshow_js) 


