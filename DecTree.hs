module DecTree
( DecTree (..)
, decide
, paraDec
) where

import Util
import System.Random
import Dist

d1 :: DecTree () Double String
d1 = DecTree paraDec 
       [ (10, DecTree paraDec 
                [ (80 , Answ "Ano!") 
                , (20 , Answs ["Mozna..","Okej.."]) ] ) 
       , (20,Answ2 $ \ x -> "Ne!"++show x  ) ]

test1 = decide d1 ()


data DecTree q c a = Answ a  
                   | Answ2 (Double->a)
                   | Answs [a]
                   | Embryo (q -> DecTree q c a ) 
                   | DecTree ( q -> [c] -> [Double] )
                             [(c , DecTree q c a )]

decide :: DecTree q c a -> q -> Dist a
decide t q = uncurry mkDist2 $ decide' t q 1 
  where
  decide' :: DecTree q c a -> q -> Double -> ([(a,Double)],[(Double->a,Double)]) 
  decide' (Answ  a ) _ feed = ([(a,feed)],[])
  decide' (Answ2 f ) _ feed = ([],[(f,feed)])
  decide' (Answs as) _ feed = ([ (a,f) | a <- as ],[]) where f = feed / (fromIntegral $ length as)
  decide' (Embryo f) q feed = decide' (f q) q feed
  decide' (DecTree decFun sons) q feed 
     = concatMap' (\(t,f)->decide' t q f) $ splitFeed feed $ zip ts $ decFun q cs
    where 
    (cs,ts) = unzip sons
    concatMap' :: (a -> ([b],[c]) ) -> [a] -> ([b],[c])
    concatMap' f =  foldr (\x (acc1,acc2) -> let (ys1,ys2) = f x in (ys1++acc1,ys2++acc2)) ([],[])

             


splitFeed :: Double -> [(t,Double)] -> [(t,Double)]
splitFeed feed xs = map (\(t,part)->(t,feed*part/suma)) xs
  where suma = sum $ map snd xs

paraDec :: (NumableCase c) => q -> [c] -> [Double] 
paraDec _ = map toNum

class NumableCase c where
  toNum :: c -> Double

instance NumableCase Double where
  toNum = id 

instance (Show a,Show c) => Show (DecTree q c a) where
  show (Answ a)       = show a
  show (Answ2 f)      = "<" ++ show (f 0.5) ++ ">"
  show (Answs as)     = show as
  show (Embryo _)     = "<EMBRIO>"
  show (DecTree _ xs) = show xs 
 

