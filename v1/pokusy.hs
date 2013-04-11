{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveDataTypeable    #-}

import Data.Data
import Data.Dynamic
import Data.Maybe

class Gener from by to where
 gen :: from -> by -> to


instance Gener Int Char String where 
 gen n ch = replicate n ch  


--data Mordor = Mordor (Int->Int) deriving (Typeable,Data)


gsize :: Data a => a -> Int
gsize x = 1 + sum ( gmapQ gsize x )



t1 = App (Lam "x" (ValPlus (ValInt 1) (Var "x"))) (ValInt 5)
t2 = App (Lam "x" (ValPlus (Var "x") (Var "x"))) (ValInt 5)
t3 = Lam "x" (ValPlus (ValPlus (Var "x") (Var "x")) (Var "x"))

data Term = Var String
          | Lam String Term
          | App Term Term
          | ValInt Int
          | ValPlus Term Term
          | Hax (Term->Term) 


toHask :: Term -> Term
toHask t = case t of
 Lam x m  -> Hax (\ x' -> toHask (subVar x x' m) )
 App m n  -> (unHax . toHask $ m) (toHask n) 
 ValInt _ -> t
 ValPlus m n -> case (toHask m , toHask n) of
  ( ValInt a , ValInt b) -> ValInt $ a + b
  _ -> error "ValPlus.."
 Hax _ -> error "Hax.."
 Var _ -> error "Var.."

unHax :: Term -> (Term -> Term)
unHax t = case t of
 Hax f -> f
 _ -> error "unHax.."

subVar :: String -> Term -> Term -> Term
subVar u u' t = case t of
 Var x    -> if x == u then u' else t
 Lam x m  -> if x == u then t  else Lam x (subVar u u' m)
 App m n  -> App (subVar u u' m) (subVar u u' n)
 Hax _    -> t
 ValInt _ -> t
 ValPlus m n -> ValPlus (subVar u u' m) (subVar u u' n) 


instance Show Term where
  show t = case t of
   Var x -> x
   Lam x m -> "(\\ " ++ x ++" . " ++ show m ++ " )"
   App m n -> "(" ++ show m ++ " " ++ show n ++ ")"
   Hax _   -> "HAX"  
   ValInt n -> show n
   ValPlus m n -> "("++ show m ++"+"++show n++")" 




