
import Control.Monad.Writer
import System.Random
import Control.Monad.State

import GPclasses 
import Util
import Dist

----------- EXPERIMENTS & GRAVEYARD .. --------------


-- writer experiments ----------------------

type Mo a = WriterT [String] (State StdGen) a

getRandom_ :: Random a => Mo a
getRandom_ = lift getRandom

getRandomR_ :: Random a => (a,a) -> Mo a
getRandomR_ = lift . getRandomR

infiniteRand_ :: Rand a -> Mo [a]
infiniteRand_ = lift . infiniteRand

log_ :: String -> Mo () 
log_ str = tell [str]

getGen :: Mo StdGen
getGen = lift get

putGen :: StdGen -> Mo ()
putGen = lift . put

 
-- Podle mě bude problém s nekonečnym logovánim....

infChain :: (a -> Mo a) -> a -> Mo [a]
infChain f x = do
   gen <- getGen
   let (gen1,gen2) = split gen
   putGen gen1
   xs <- inf f x 
   putGen gen2
   return $ x:xs
 where
  inf :: (a -> Mo a) -> a -> Mo [a]
  inf f x = do
   x' <- f x
   xs <- inf f x'
   return $ x':xs 

infSame :: Mo a -> Mo [a]
infSame rand = do
   gen <- getGen
   let (gen1,gen2) = split gen
   putGen gen1
   xs <- inf rand
   putGen gen2
   return xs
 where
  inf :: Mo a -> Mo [a]
  inf r = do
   x  <- r
   xs <- inf r
   return $ x:xs

-- experimental --------------------------

type Di a = (a,a) 

sex :: ( Cros genom opt ) => opt -> Di genom -> Di genom -> Rand (Di genom)
sex opt dad@(deda1,babi1) mum@(deda2,babi2) = do
 (sperm,_) <- crossIt opt deda1 babi1
 (egg  ,_) <- crossIt opt deda2 babi2
 return (sperm,egg) 


sex2 :: ( Cros genom opt ) => opt -> Di (Dist genom) -> Di (Dist genom) -> Rand (Di [genom])
sex2 opt dad@(deda1,babi1) mum@(deda2,babi2) = do

 fromD1 <- distTake_new (distSize deda1) deda1 
 fromD2 <- distTake_new (distSize deda2) deda2
 fromB1 <- distTake_new (distSize babi1) babi1 
 fromB2 <- distTake_new (distSize babi2) babi2

 sperms <- forM (zip fromD1 fromB1) (\(d1,b1) -> crossIt opt d1 b1)
 eggs   <- forM (zip fromD2 fromB2) (\(d2,b2) -> crossIt opt d2 b2)
 
 return ( map fst sperms , map fst eggs ) 


meiosis :: ( Cros genom opt ) => opt -> Di genom -> Rand [genom]
meiosis opt (gDad,gMum) = do
 (son1,son2) <- crossIt opt gDad gMum
 (son3,son4) <- crossIt opt gDad gMum
 return [son1,son2,son3,son4]


-- TODO : pochopit proč to tohle rozbylo: 
--distGen :: (Gene t o) => DistGen o -> Rand [Dist t]
--distGen (DiG_Uniform opt len) = 
-- let gOpt = LG_ ( PG_Both opt (DG_Uniform (0,1) ) ) len
--  in generateIt gOpt >>= mapM (return . mkDist) <---------------
 
 
{--
crossThem :: (Cros term opt) => opt -> [term] -> Rand [term]
crossThem _   []  = return []
crossThem _   [t] = return [t]
crossThem opt (t1:t2:ts) = do
 (t1',t2') <- crossIt opt t1 t2
 ts'       <- crossThem opt ts 
 return $ t1' : t2' : ts'
--}	

{--
t4   = runRand $ (  (distGen (DiG_Uniform (DG_Normal (0,1) ) 10)  )::Rand [Dist Double] )


tt4  = runRand $ evolveBegin prob4

bug2 = runState (evolveBegin prob4) (mkStdGen 2)

bug3 = runState (( ffDist prob4 . take (popSize prob4) ) `liftM` generateIt (gOpt prob4)) (mkStdGen 2)

bug4 = runState ( ( take 3 ) `liftM` ((generateIt (gOpt prob4))::Rand [Dist Double] ) )  (mkStdGen 2)

bug1 = flip runState (mkStdGen 42) $ do 
 pop0 <- evolveBegin prob4
 distTake_new 1 pop0

--getWinners pop
ttt4 = runRand $ evolveStep prob4 =<< evolveBegin prob4

prob3 =  
 let popSize = 200
     len     = 100
     eOpt    = mkEOpt 33 33 33
     gOpt    = LG_         (PG_Both BG_    (DG_Normal (0,1)) ) len
     mOpt    = LM_OnePoint (PM_Both BM_Not (DM_Normal (0,1)) ) len
     cOpt    = LC_OnePoint (PC_Both BC_    DC_Avg            ) len -- tady muže bejt klidně () !!!
 in Problem popSize eOpt gOpt mOpt cOpt ff3


prob4 :: Problem (Dist Double) (DistGen DoubleGen) () ()
prob4 = 
 let popSize = 2
     len     = 3
     eOpt    = mkEOpt 33 0 0
     gOpt    = DiG_Uniform (DG_Normal (0,1) ) len
     mOpt    = () --DiM_ ( DM_Normal (0,1) )
     cOpt    = () --DiC_OnePoint ()
     ff :: Dist Double -> FitVal
     ff _ = 1
  in Problem popSize eOpt gOpt mOpt cOpt ff
--}

-- PARSERS ::: -----------------------------------------------------------------------------------------



{--

readExpr :: String -> Either ParseError LispVal
readExpr input = parse parseExpr "lisp" input 

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces2 :: Parser ()
spaces2 = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool deriving (Show)

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom


parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit


parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces2

parseDottedList :: Parser LispVal
parseDottedList = do
     head <- endBy parseExpr spaces2
     tail <- char '.' >> spaces2 >> parseExpr
     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
   char '\''
   x <- parseExpr
   return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

-------------------------


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
 lpar
 tree <- ex 
 rpar
 return tree 

ex2 :: MyParser KTree
ex2 = do
  str <- symbo
  ts <- many ex
  return $ KNode str ts
--}

{--

exp -> tree | ( exp )

tree -> s | (s exp exp ...)

start -> exp

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
{--
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

--}