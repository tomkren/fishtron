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
import Text.Parsec.Pos

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


data Tok = LPar | RPar | Symbol String | End deriving (Show,Eq)

type Token = (SourcePos,Tok)
data Tok2  = Identifier String
          | Reserved String
          | Symbol2 String
          | Price Int
          deriving Show

kPars :: String -> Either ParseError KTree
kPars str = do
  toks <- kToksParse str
  kExprParse (toTokenus $ toks++[End])

kToksParse :: String -> Either ParseError [Tok]
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

type MyParser a = GenParser Token () a

toTokenus :: [Tok] -> [Token]
toTokenus ts = zip (map  (\i->setSourceColumn (initialPos "input") i ) [1..] ) ts

kExprParse :: [Token] -> Either ParseError KTree
kExprParse toks = parse input "(Syntax Error)" toks

input :: MyParser KTree
input = do
  tree <- ex
  end
  return tree


mytoken :: (Tok -> Maybe a) -> MyParser a
mytoken test = token showToken posToken testToken
 where 
  showToken (pos,tok) = show tok
  posToken  (pos,tok) = pos
  testToken (pos,tok) = test tok


symbo :: MyParser String
symbo = mytoken $ \tok -> case tok of
 Symbol name -> Just name
 _ -> Nothing


lpar :: MyParser ()
lpar = mytoken $ \tok -> case tok of
 LPar -> Just ()
 _ -> Nothing

rpar :: MyParser ()
rpar = mytoken $ \tok -> case tok of
 RPar -> Just ()
 _ -> Nothing

end :: MyParser ()
end = mytoken $ \tok -> case tok of
 End -> Just ()
 _ -> Nothing

ex :: MyParser KTree
ex = ex1 <|> ex2

ex1 :: MyParser KTree
ex1 = do
 str <- symbo 
 return $ KNode str [] 

ex2 :: MyParser KTree
ex2 = do
  lpar 
  str <- symbo
  ts <- many ex
  rpar
  return $ KNode str ts


{--

do
  tree <- ex
  end
  return tree

lpar :: GenParser KToken st KToken
lpar = sat (==LPar)

rpar :: GenParser KToken st KToken
rpar = sat (==RPar)

end :: GenParser KToken st KToken
end = sat (==End)

symb :: GenParser KToken st String
symb = do
 Symbol str <- sat (\x->case x of Symbol _ -> True ;_->False)
 return str

ex :: GenParser KToken st KTree
ex = ex1 <|> ex2

ex1 :: GenParser KToken st KTree
ex1 = do 
  str <- symb
  return $ KNode str []


ex2 :: GenParser KToken st KTree
ex2 = do
  lpar 
  str <- symb
  ts <- many ex
  rpar
  return $ KNode str ts

--}





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