module TTerm 
( Typ (..)
, TTerm (..)
, Context
) where

import Data.Typeable
import Text.ParserCombinators.Parsec

import Heval


type Symbol = String

data Typ  = Typ Symbol
          | Typ :-> Typ
          deriving (Eq,Ord)
infixr 7 :->

type Context = [(Symbol,Typ)]

data TTerm = TVar Symbol        Typ  
           | TVal Symbol        Typ   
           | TLam Symbol TTerm  Typ
           | TApp TTerm  TTerm  Typ

data Term  = Var Symbol          
           | Val Symbol           
           | Lam Symbol Term  
           | App Term   Term  


instance Show TTerm where 
 show t = case t of
  TVar x   _ -> x
  TVal x   _ -> x
  TLam x m _ -> "(\\ "++ x   ++ " -> " ++ show m ++")"
  TApp m n _ -> "( "++ show m ++ " "    ++ show n ++")" 

instance Show Term where 
 show t = case t of
  Var x   -> x
  Val x   -> x
  Lam x m -> "(\\ "++ x   ++ " -> " ++ show m ++")"
  App m n -> "("++ show m ++ " "    ++ show n ++")" 


ttEval :: (Typeable a) => TTerm -> a -> a
ttEval tterm a = eval (show tterm) a

-- parser --------------------------------------------

tParse :: String -> Either ParseError Term 
tParse str = parse input "Term" str

tParse' :: String -> Term
tParse' str = let Right t = tParse str in t

input :: Parser Term
input = do
 t <- expr 
 eof
 return t

expr :: Parser Term
expr = var
   <|> do char '('
          t <- lam <|> app
          char ')'
          return t  

sym :: Parser String 
sym = many1 $ noneOf " ()\\->"

sps :: Parser ()
sps = skipMany1 space

var :: Parser Term
var = do
  str <- sym 
  return $ Var str  

app :: Parser Term
app = do 
 e1 <- expr
 sps
 e2 <- expr
 return $ App e1 e2

lam :: Parser Term
lam = do
 char '\\'
 sps
 str <- sym
 sps
 char '-' >> char '>'
 sps
 e <- expr
 return $ Lam str e




