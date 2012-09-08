module TTerm 
( Typ (..)
, TTerm (..)
, Context
, Symbol
, typeArgs
, ttermTyp
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

-- various -----------------------------------------

ttermTyp :: TTerm -> Typ
ttermTyp t = case t of
 TVar _   typ -> typ  
 TVal _   typ -> typ
 TLam _ _ typ -> typ
 TApp _ _ typ -> typ

typeArgs :: Typ -> ([Typ],Symbol)
typeArgs typ = case typ of
 Typ alpha -> ([],alpha)
 (a :-> b) -> let (  as, alpha) = typeArgs b
               in (a:as, alpha)

typeRank :: Typ -> Int
typeRank t = case t of
  Typ _   -> 0
  a :-> b -> max (1 + typeRank a) (typeRank b) 

isTypNum :: Typ -> Bool
isTypNum t = if isTypFromNulls t then f t else False
 where 
  f t = case t of
   Typ _         -> True
   a :-> (Typ _) -> f a
   _             -> False

isTypFromNulls :: Typ -> Bool
isTypFromNulls t = case t of
 Typ "0" -> True
 Typ _   -> False
 a :-> Typ "0" -> isTypFromNulls a
 _ -> False 

-- show --------------------------------------------

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

instance Show Typ where show = showTyp

showTyp :: Typ -> String
showTyp t | isTypNum t = show $ typeRank t
          | otherwise  = showTyp' t 

showTyp' :: Typ -> String
showTyp' (Typ t) = t
showTyp' (a :-> b) = "(" ++ showTyp a ++ "->" ++ showTyp b ++ ")"

-- eval -----------------------------------------------

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




