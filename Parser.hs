module Parser 
( parseTTerm
, parseTerm
) where


--TODO - OSTUDA !! :) - předělat do Parsecu


import Base

-- parser ----------------------------------------------------------------------

parseTTerm :: Env -> String -> Maybe TTerm
parseTTerm env str = do
  t <- parseTerm str 
  typeIt env t

data Token   = AtomToken String | ParenToken [Token] deriving (Show)

-- Převádí textovou reprezentaci termu na vnitřní reprezentaci termu.
parseTerm :: String -> Maybe Term
parseTerm code = toTokens code >>= toTerm 

-- Převede textovou repreztentaci na tokeny. Ošetřuje hlavně uzávorkování.
-- Tokenem chápeme buď nějaký symbol, nebo složený token sestávající
-- z více tokenů, odpovídající jedné závorce.
toTokens :: String -> Maybe Token
toTokens = ws2tok' . words 
         . (obal '(') . (obal ')') . (obal '.') . (obal ':') .(obalL '-') .(obalR '>') 
         . (skip '\\') . (skip '/')
    where 
    ws2tok' :: [String] -> Maybe Token
    ws2tok' ws = ws2tok ws >>= return . ParenToken 
    ws2tok :: [String] -> Maybe [Token]
    ws2tok []     = Just []
    ws2tok (x:xs) 
        | x == "("  = do
            ( part1 , part2 ) <- splitByParen 0 [] xs
            ps1 <- ws2tok part1
            ps2 <- ws2tok part2
            return $ (ParenToken ps1 ) : ps2
        | otherwise = do
            ts <- ws2tok xs
            return $ (AtomToken x) : ts
    splitByParen :: Int -> [String] -> [String] -> Maybe ([String],[String])
    splitByParen _   _     []     = Nothing
    splitByParen num part1 (x:xs) 
        | x == ")"    = if num == 0 
                        then Just (reverse part1,xs) 
                        else splitByParen (num-1) (x:part1) xs
        | x == "("    =      splitByParen (num+1) (x:part1) xs
        | otherwise   =      splitByParen num     (x:part1) xs

-- Obalí vískyty `co` v `str` mezerami.
obal :: Char -> String -> String
obal co str = str >>= (\x ->if x == co then [' ',x,' '] else [x])

obalL :: Char -> String -> String
obalL co str = str >>= (\x ->if x == co then [' ',x] else [x])

obalR :: Char -> String -> String
obalR co str = str >>= (\x ->if x == co then [x,' '] else [x])

-- Smaže vískyty `co` v `str`. 
skip :: Char -> String -> String
skip co str = str >>= (\x ->if x == co then [] else [x]) 

-- Převede roztokenovaný vstup na term, pokud to jde.
toTerm :: Token -> Maybe Term
toTerm (AtomToken str ) = Just $ TAtom str
toTerm (ParenToken [] ) = Nothing
toTerm (ParenToken xs ) = case  tokenSplit "." xs of
    ( [], _  ) -> Nothing
    ( a , [] ) -> toApp a
    ( a , b  ) -> toLam2 a b

-- Pomocná funkce starající se o aplikace.
toApp :: [Token] -> Maybe Term
toApp []     = Nothing
toApp [x]    = toTerm x
toApp xs = do 
    ts <- toTerm $ ParenToken (init xs)
    t  <- toTerm $ last xs
    return $ ts :@ t

-- Pomocná funkce starající se o lambda abstrakce.
toLam2 :: [Token] -> [Token] -> Maybe Term
toLam2 head body = case lamHead head of
   Just sss -> toLam' sss
   Nothing  -> Nothing
  where
  toLam' :: [(Symbol,Typ)] -> Maybe Term
  toLam' []         = toTerm $ ParenToken body
  toLam' ((x,typ):xs) = do
    bodyTerm <- toLam' xs
    return $ (x,typ) :. bodyTerm 
  

parseTyp :: [Token] -> Maybe Typ
parseTyp ts = parseTyp1 ts >>= parseTyp3

parseTyp3 :: [Token] -> Maybe Typ
parseTyp3 [] = Nothing
parseTyp3 ts = do 
  typy <- sequence $ map parseTyp2 ts
  return $ foldr1 (:->) typy

parseTyp2 :: Token -> Maybe Typ
parseTyp2 t = case t of
   AtomToken str  -> Just $ Typ str
   ParenToken ts  -> parseTyp1 ts >>= parseTyp3

parseTyp1 :: [Token] -> Maybe [Token]
parseTyp1 tokens  = case tokens of
  []  -> Nothing
  [t] -> Just [t]
  (t1:(AtomToken "->"):ts) -> do 
    ts' <- parseTyp1 ts
    return $ t1 : ts' 
  _ -> Nothing


lamHead :: [Token] -> Maybe [(Symbol,Typ)]
lamHead ts = case lamHead1 ts of
  [varTok]:rest -> do
    ts2 <- lamHead2 varTok rest
    (sequence . lamHead3) ts2
  _             -> Nothing

{--
lamHead4 :: [Maybe a] -> Maybe [a]
lamHead4 as = case as of
  []            -> Just []
  Nothing  : _  -> Nothing
  (Just x) : xs -> case lamHead4 xs of
    Nothing  -> Nothing
    Just xs' -> Just $ x : xs'
--}

lamHead3 :: [(Token,[Token])] -> [Maybe (Symbol,Typ)]
lamHead3 = map $ \(t,ts) -> case t of
  AtomToken str -> case parseTyp ts of 
    Nothing  -> Nothing
    Just typ -> Just (str,typ)
  _             -> Nothing

lamHead2 :: Token -> [[Token]] -> Maybe [(Token,[Token])]
lamHead2 varTok []       = Nothing
lamHead2 varTok [ts]     = Just [(varTok,ts)] 
lamHead2 varTok (r:rest) = case r of 
  [] -> Nothing
  ts -> let varTok' = last ts
         in case lamHead2 varTok' rest of
            Nothing  -> Nothing
            Just ret -> Just $ (varTok,init ts) : ret 

lamHead1 :: [Token] -> [[Token]]
lamHead1 ts = case tokenSplit ":" ts of
 (ts1,[])  -> [ts1]
 (ts1,ts2) -> ts1 : lamHead1 ts2

-- "Zploštění" struktury Tokenů. 
tokenFlatten :: [Token] -> [Token]
tokenFlatten []     = []
tokenFlatten (x@(AtomToken _):xs) = x : tokenFlatten xs
tokenFlatten ((ParenToken ts):xs) = tokenFlatten ts ++ tokenFlatten xs

-- Rozdělí seznamu tokenů na dvě půlky podle prvního vískytu
-- AtomTokenu s odpovídajícím jmenem danému stringu.
tokenSplit :: String -> [Token] -> ([Token],[Token])
tokenSplit _ []       = ([],[])
tokenSplit str (x:xs) = case x of
    (AtomToken str') ->
        if str == str' 
        then ( []  , xs )
        else ( x:a , b  )
    t -> ( t:a , b  )
    where
    (a,b) = tokenSplit str xs


