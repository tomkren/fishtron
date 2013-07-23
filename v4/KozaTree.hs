module KozaTree 
( KTree (..), KPos
, kParse
, kPoses, kPoses2
, kChangeSubtree
, kSubtree
, kSize
, kDepth
) where

import Data.List (intercalate)
import Data.Either (lefts,rights)
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec (parse,eof,(<|>),char,many1,noneOf,skipMany1,space,sepBy,Parser,ParseError)




data KTree = KNode String [KTree] deriving (Eq)

type KPos = [Int]



t1 = kParse' "(a (b e (f k l) g) (c h i) (d j))"

t2 = kParse' "(plus 1 1)"


-- pak udelat quickcheck na 
--  (kSize t1) == (length . kPoses $ t1)
--  kSubtrees t == map (fromJust . kSubtree t) (kPoses t) 
--  když změnim dvakrat na samý pozici vratí se to do puvodního stavu

kSize :: KTree -> Int
kSize (KNode _ ts) = (1+) . sum $ map kSize ts

kDepth :: KTree -> Int
kDepth (KNode _ ts) = case ts of
 [] -> 0
 _  -> (1+) . maximum $ map kDepth ts

kSubtrees :: KTree -> [KTree]
kSubtrees t@(KNode _ ts) = t : concatMap kSubtrees ts

kSubtree :: KTree -> KPos -> KTree
kSubtree t [] = t
kSubtree (KNode _ ts) (i:is) = 
 kSubtree (ts !! (i-1)) is

kChangeSubtree :: KTree -> KPos -> KTree -> (KTree,KTree)
kChangeSubtree tree           []     newSub = (newSub,tree) 
kChangeSubtree (KNode str ts) (i:is) newSub = 
 let (ts1,subt:ts2) = splitAt (i-1) ts
     (subt',oldSub) = kChangeSubtree subt is newSub
  in (KNode str (ts1 ++ (subt':ts2) ) ,oldSub)  


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


-- toTree :: KTree -> Tree String
-- toTree (KNode str ts) = Node str $ map toTree ts



-- parser --------------------------------------

kParse :: String -> Either ParseError KTree
kParse str = parse input "KTree" str

kParse' :: String -> KTree
kParse' str = let Right t = kParse str in t

input :: Parser KTree
input = do
 t <- expr 
 eof
 return t

expr :: Parser KTree
expr = tre1
   <|> do char '('
          t <- tre2
          char ')'
          return t  

sym :: Parser String 
sym = many1 $ noneOf " ()"

sps :: Parser ()
sps = skipMany1 space

tre1 :: Parser KTree
tre1 = liftM (\str->KNode str []) sym  

tre2 :: Parser KTree
tre2 = do 
 str <- sym
 sps
 ts <- sepBy expr sps
 return $ KNode str ts


