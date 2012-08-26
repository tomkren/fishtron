module KozaTree 
( KTree (..)
, parseKTree
) where

import Text.ParserCombinators.Parsec
import Data.List
import Data.Typeable
import Data.Tree

import Heval

data KTree = KNode String [KTree]





instance Show KTree where
 show (KNode str ts) = case ts of
  [] -> str
  _  -> "(" ++ str ++ " " ++ (intercalate " " (map show ts)) ++ ")"


toTree :: KTree -> Tree String
toTree (KNode str ts) = Node str $ map toTree ts


evalKTree :: (Typeable a) => KTree -> a -> IO a
evalKTree t as = heval (show t) as  

{- 

 ( (a b c) c d e  ) .. v kozovskejch nejde

 expr   ::= symbol | ( symbol exprs )  
 exprs  ::= expr | expr exprs 
 symbol ::= 'abc'
-}

kparse :: String -> KTree
kparse str = let Right t = parseKTree str in t

parseKTree :: String -> Either ParseError KTree
parseKTree str = parse expr0 "(unknown)" str

expr0 :: GenParser Char st KTree
expr0 = do 
 tree <- expr
 eof
 return tree

expr :: GenParser Char st KTree
expr = expr1 <|> expr2

expr1 :: GenParser Char st KTree
expr1 = do 
 str <- symbol
 return $ KNode str []

expr2 :: GenParser Char st KTree
expr2 = do
 char '('
 str   <- symbol
 white
 trees <- sepBy expr white
 char ')'
 return $ KNode str trees

symbol :: GenParser Char st String
symbol = many1 ( noneOf " ()" )

white :: GenParser Char st Char
white = char ' ' 