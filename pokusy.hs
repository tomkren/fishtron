{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveDataTypeable    #-}

import Data.Data

class Gener from by to where
 gen :: from -> by -> to


instance Gener Int Char String where 
 gen n ch = replicate n ch  


--data Mordor = Mordor (Int->Int) deriving (Typeable,Data)


gsize :: Data a => a -> Int
gsize x = 1 + sum ( gmapQ gsize x )





data Term = Var String 
          | Lam String Term
          | App Term Term
          | Hax (Term->Term)
         


instance Show Term where
  show t = case t of
   Var x -> x
   Lam x m -> "(\\ " ++ x ++" . " ++ show m ++ ")"
   App m n -> "( " ++ show m ++ " " ++ show n ++ ")"
   Hax _   -> "HAX"  

unHax (Hax f) = f 

toHask ( Var x )    = ( Var x )
toHask ( Lam x' m ) = Hax ( \ x -> toHask' x x' m )
toHask ( App m n  ) = (unHax $ toHask m) (toHask n)

toHask' x x' (Var y') 
 | x' == y'  = x
 | otherwise = (Var y')

toHask' x x' (App m n) = App (toHask' x x' m) (toHask' x x' n) 

toHask' x x' (Lam y' m) 
 | x' == y'  = (Lam y' m)
 | otherwise = (Lam y' ( toHask' x x' m ) )  
