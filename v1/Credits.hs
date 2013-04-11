module Credits where

import Control.Monad.State

type Credit = Double

type Crem = StateT Credit Maybe


runCrem :: Credit -> Crem a -> Maybe (a,Credit)
runCrem c crem = runStateT crem c

getCredit :: Crem Credit
getCredit = get

grab :: Credit -> Crem ()
grab cost = do
 c <- get
 put ( c - cost )
 

failComputation :: Crem a
failComputation = fail ""

mkFun :: Credit -> (a->b) -> a -> Crem b
mkFun cost f x = testCredit cost (f x)

mkFun2 :: Credit -> (a->b->c) -> a -> b -> Crem c
mkFun2 cost f x y = testCredit cost (f x y)


testCredit :: Credit -> a -> Crem a
testCredit cost x = do
 c <- get
 if c >= cost 
  then do
   put ( c - cost )
   return x
  else failComputation



dot_ :: (b->Crem c) -> (a->Crem b) -> (a->Crem c)
dot_ = (<=<) 

ap_ :: (a->Crem b) -> Crem a -> Crem b
ap_ = (=<<) 

map_ :: (a->Crem b) -> [a] -> Crem [b]
map_ = mapM

succ_ :: Enum a => a -> Crem a
succ_ = mkFun 1 succ

plus_ :: Num a => a -> a -> Crem a
plus_ = mkFun2 2 (+)

take_ :: Int -> [a] -> Crem [a]
take_ 0 xs     = grab 1 >> return []
take_ n []     = grab 1 >> return []
take_ n (x:xs) = do 
 grab 2
 ret <- take_ (n-1) xs
 return $ x:ret 






------------------------------------------------------------
{-- tohle se spíš podobá odhadování nákladů, než počítání nákladů

data Cr a b = Cr { crFun :: (a->b) , crCost :: (a->Credit) }

cr_succ :: Cr Int Int
cr_succ = Cr succ (const 1) 

cr_sum :: Num a => Cr [a] a
cr_sum = Cr sum crLen 

cr_map :: Cr ( Cr a b , [a] ) [b]
cr_map = Cr fun cost
 where 
  fun :: ( Cr a b , [a] ) -> [b]
  fun ( cr_f , xs ) = map (crFun cr_f) xs
  cost :: ( Cr a b , [a] ) -> Credit
  cost ( cr_f , xs ) = sum . map (crCost cr_f) $ xs

dot :: Cr b c -> Cr a b -> Cr a c
dot f g = Cr fun cost
 where 
  fun    = (crFun f) . (crFun g) 
  cost x = (crCost g x) + (crCost f (crFun g x) ) 



crLen :: [a] -> Credit
crLen = fromIntegral . length

--}