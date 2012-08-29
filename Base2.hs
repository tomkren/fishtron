module Base2 
( TTerm (..)
) where

import Data.Typeable

import Heval


type Symbol  = String

data Typ  = Typ Symbol
          | Typ :-> Typ
          deriving (Eq,Ord)
infixr 7 :->

data TTerm = Var Symbol        Typ  
           | Val Symbol        Typ   
           | Lam Symbol TTerm  Typ
           | App TTerm  TTerm  Typ
--         | Temp Nete

instance Show TTerm where 
 show t = case t of
  Var x   _ -> x
  Val x   _ -> x
  Lam x m _ -> "( \\ "++ x   ++ " -> " ++ show m ++" )"
  App m n _ -> "( "++ show m ++ " "    ++ show n ++" )" 

int = Typ "Int" 

t1 = App (Lam "x" ( App (Val "(+1)" (int:->int) ) (Var "x" int ) int  ) (int:->int)) (Val "6" int) int

ttEval :: (Typeable a) => TTerm -> a -> IO a
ttEval tterm a = eval (show tterm) a
