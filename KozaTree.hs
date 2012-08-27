module KozaTree 
( KTree (..)
, kParse
, kPoses, kPoses2
, kChangeSubtree
) where

import Text.ParserCombinators.Parsec
import Data.List
import Data.Typeable
import Data.Tree
import Data.Maybe
import Data.Either
import Control.Monad
import Text.Parsec.Prim

import Heval
import Util

data KTree = KNode String [KTree] deriving (Eq)

type KPos = [Int]


t1 = kParse' "(a (b e (f k l) g) (c h i) (d j))"


-- pak udelat quickcheck na 
--  (kSize t1) == (length . kPoses $ t1)
--  kSubtrees t == map (fromJust . kSubtree t) (kPoses t) 
--  když změnim dvakrat na samý pozici vratí se to do puvodního stavu

kSize :: KTree -> Int
kSize (KNode _ ts) = (1+) . sum $ map kSize ts

kSubtrees :: KTree -> [KTree]
kSubtrees t@(KNode _ ts) = t : concatMap kSubtrees ts

kSubtree :: KTree -> KPos -> Maybe KTree
kSubtree t [] = Just t
kSubtree (KNode _ ts) (i:is) = do
 subt <- ts `at` i
 kSubtree subt is

kChangeSubtree :: KTree -> KPos -> KTree -> (KTree,KTree)
kChangeSubtree tree           []     newSub = (newSub,tree) 
kChangeSubtree (KNode str ts) (i:is) newSub = 
 let (ts1,subt:ts2) = splitAt (i-1) ts
     (subt',oldSub) = kChangeSubtree subt is newSub
  in (KNode str (ts1 ++ (subt':ts2) ) ,oldSub)  


at :: [a] -> Int -> Maybe a
xs     `at` n | n < 1 =  Nothing
[]     `at` _         =  Nothing
(x:_)  `at` 1         =  Just x
(_:xs) `at` n         =  xs `at` (n-1)

kPoses :: KTree -> [KPos]
kPoses t = map reverse (kPoses' [] t)
 where
  kPoses' :: [Int] -> KTree -> [[Int]]
  kPoses' pos (KNode _ ts) = 
   pos : (concatMap (\(i,t)-> kPoses' (i:pos) t ) (zip [1..] ts) )

kPoses2 :: KTree -> ([KPos],[KPos])
kPoses2 t = 
  let xs  = kPoses2' [] t 
      rev = map reverse 
   in ( rev . lefts $ xs , rev . rights $ xs )
 where
  kPoses2' :: [Int] -> KTree -> [ Either [Int] [Int] ]
  kPoses2' pos (KNode _ []) = [Left pos]
  kPoses2' pos (KNode _ ts) = 
   (Right pos) : (concatMap (\(i,t)-> kPoses2' (i:pos) t ) (zip [1..] ts) )


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


data KToken = LPar | RPar | Symbol String deriving (Show,Eq)

kPars :: String -> Either ParseError [KToken]
kPars str = do
 toks <- kToksParse str
 kExprParse toks

kToksParse :: String -> Either ParseError [KToken]
kToksParse str = parse toks' "(Token Error)" str 
 where
  sym   = Symbol `liftM` many1 ( noneOf " ()" )
  lpar  = char '(' >> return LPar
  rpar  = char ')' >> return RPar
  tok   = sym <|> lpar <|> rpar
  manyW = many $ char ' '
  toks' = manyW >> toks
  toks  = toks1 <|> toks0
  toks0 = eof >> return []
  toks1 = do
   t <- tok
   manyW
   ts <- toks
   return $ t:ts 

kExprParse :: [KToken] -> Either ParseError [KToken]
kExprParse toks = parse expre "(Syntax Error)" toks

sat :: (KToken ->Bool) -> GenParser KToken st KToken
sat p = do 
 toks <- getInput
 case toks of
  [] -> parserFail "(fail)"
  (tok:rest) -> 
   if p tok then do {setInput rest ; return tok} else parserFail "(fail)"

lpar :: GenParser KToken st KToken
lpar = sat (==LPar)

rpar :: GenParser KToken st KToken
rpar = sat (==RPar)

symb :: GenParser

ex1 :: GenParser KToken st [KToken]
ex1 = do 
  lpar 
  many ex
  rpar 
  return ts
 

-- ---------

kParse' :: String -> KTree
kParse' str = let Right t = kParse str in t

kParse :: String -> Either ParseError KTree
kParse str = parse expr0 "(unknown)" str

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