module TTerm
( Typ (..)
, TTerm (..)
, TTermZipper (..)
, TTermDrobek (..)
, Context
, Symbol
, typeArgs
, ttermTyp , isInCtx
, mkTz, showTTZ, tzDown, tzLeft, tzRight, tzGoTop,ttzTyp
, subs
, makeVarsUnique 
, checkTyp
, toSki, toSki'

,fullEtaReduce
) where

import Data.List ( nub, (\\) , intercalate )
import Data.Typeable ( Typeable )
import Text.ParserCombinators.Parsec ( Parser, ParseError, parse, (<|>), eof, char, many1, noneOf, skipMany1, space )

import Utils ( newSymbol )

--import Util  ( newSymbol )
--import Heval ( eval )

---------------------------------------------------

type Symbol = String

data Typ  = 
 Typ Symbol    |
 TypVar Symbol | 
 TypFun Symbol [Typ] |
 Typ :-> Typ 
 deriving (Eq,Ord)
infixr 7 :->

type Context = [(Symbol,Typ)]

data TTerm = 
 TVar Symbol        Typ | 
 TVal Symbol        Typ | 
 TLam Symbol TTerm  Typ |
 TApp TTerm  TTerm  Typ
 deriving (Ord,Eq)

-----------------------------------------------------------
-- eta-reduction ------------------------------------------

fullEtaReduce :: TTerm -> TTerm
fullEtaReduce t = case oneEtaReduce t of
  Nothing -> t
  Just t' -> fullEtaReduce t'

oneEtaReduce :: TTerm -> Maybe TTerm
oneEtaReduce t = case t of
 TLam x ( TApp m (TVar x' xPrimeTyp ) typApp ) typLam 
  | x == x' && (not (elem x (etaFV m))) -> Just m  
  | otherwise -> do
      m' <- oneEtaReduce m
      return $ TLam x ( TApp m' (TVar x' xPrimeTyp ) typApp ) typLam
 
 TVar _ _     -> Nothing 
 TVal _ _     -> Nothing 
 TLam x m typ -> do
  m' <- oneEtaReduce m
  return $ TLam x m' typ
 TApp m n typ -> case oneEtaReduce m of
  Nothing -> do
    n' <- oneEtaReduce n
    return $ TApp m n' typ
  Just m' -> Just $ TApp m' n typ
 


etaFV :: TTerm -> [Symbol]
etaFV (TVar v   _) = [v]
etaFV (TVal v   _) = [] 
etaFV (TApp p q _) = nub $ (fv p) ++ (fv q) -- neefektivni
etaFV (TLam v p _) = (fv p) \\ [v]



-- substitution ------------------------------------

subs :: Symbol -> TTerm -> TTerm -> TTerm 
subs x m n = fst $ sub (varNames m) m x n   

sub :: [Symbol] -> TTerm -> Symbol -> TTerm -> (TTerm,[Symbol])
sub vns x@(TVal _ _) _ _ = (x,vns)  
sub vns x@(TVar v _) var vloz 
    | v == var  = (vloz,vns)
    | otherwise = (x,vns)  
sub vns (TApp p q typ) var vloz = (TApp t1 t2 typ , vns'')
    where 
    (t1,vns')  = sub vns  p var vloz
    (t2,vns'') = sub vns' q var vloz
sub vns x@(TLam v p typ) var vloz
    | v == var  = (x,vns)
    | not (elem v (fv vloz)) = (TLam v t typ , vns'') 
    | otherwise = sub vns' term' var vloz
    where
    (t,vns'') = (sub vns p var vloz)
    (term',vns') = rename vns x

varNames :: TTerm -> [Symbol]
varNames = nub . varNames'
    where
    varNames' (TVar v   _)  = [v]
    varNames' (TVal v   _)  = [v]
    varNames' (TApp p q _)  = varNames' p ++ varNames' q
    varNames' (TLam v p _)  = v : varNames' p

fv :: TTerm -> [Symbol]
fv (TVar v   _) = [v]
fv (TVal v   _) = [v] -- TODO neni to nejaka blbost davat je do FV, kouknout k cemu se to presne pouziva
fv (TApp p q _) = nub $ (fv p) ++ (fv q) -- neefektivni
fv (TLam v p _) = (fv p) \\ [v]

rename :: [Symbol] -> TTerm -> (TTerm,[Symbol])
rename vns (TLam v m typ@(varTyp :-> _) ) = ((TLam new m' typ),new:vns')
    where 
    new             = newSymbol vns
    (m',vns')       = sub vns m v (TVar new varTyp)
rename vns t = (t,vns)

-- zunikátnění vázaných proměnných ---------------------------------------------

makeVarsUnique :: TTerm -> TTerm
makeVarsUnique tt = (\(a,_,_)->a) $ u (varNames tt) [] tt
  where  
  u :: [Symbol] -> [Symbol] -> TTerm -> ( TTerm , [Symbol] , [Symbol] )
  u allVars occupied t = case t of
    TLam x m typ@(a:->b) -> 
     if x `elem` occupied
     then let x' = newSymbol allVars
              (m',all',occ') = u (x':allVars) (x':occupied) (subs x m (TVar x' a) )
           in ( TLam x' m' typ , all' , occ' )
     else let (m',all',occ') = u allVars (x:occupied) m
           in ( TLam x m' typ , all' , occ' )
    TApp m n typ -> 
     let (m',all' ,occ' ) = u allVars occupied m
         (n',all'',occ'') = u all'    occ'     n
      in ( TApp m' n' typ , all'' , occ'' )
    _           -> ( t , allVars , occupied )

-- checkTyp -------------------------------------------------------------------

checkTyp :: TTerm -> Bool
checkTyp t = checkTyp' t (ttermTyp t) []
  where
  checkTyp' :: TTerm -> Typ -> [(Symbol,Typ)] -> Bool
  checkTyp' t wTyp baze = case t of
   TVar x typ             -> wTyp == typ && (x,typ) `elem` baze
   TVal f typ             -> wTyp == typ
   TLam x m typ@(a :-> b) -> wTyp == typ && checkTyp' m b ((x,a):(filter (\(s,_)->s/=x) baze ) )
   TApp m n typ           -> wTyp == typ && 
                            let mTyp = ttermTyp m
                                nTyp = ttermTyp n
                             in case mTyp of
                               (a :-> b) -> nTyp == a && b == typ &&
                                            checkTyp' m mTyp baze &&
                                            checkTyp' n nTyp baze
                               _         -> False
   _                     -> False

-- various -----------------------------------------

ttermTyp :: TTerm -> Typ
ttermTyp t = case t of
 TVar _   typ -> typ  
 TVal _   typ -> typ
 TLam _ _ typ -> typ
 TApp _ _ typ -> typ

isInCtx :: Symbol -> Context -> Bool
isInCtx _ [] = False 
isInCtx x ((x',_):ctx') = x == x' || isInCtx x ctx' 

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

-- Zipper ------------------------------------------

data TTermZipper = TTZ TTerm  [TTermDrobek] 

data TTermDrobek = 
 AppLeft  TTerm Typ | 
 AppRight TTerm Typ | 
 Lamb Symbol Typ 

mkTz :: TTerm -> TTermZipper
mkTz t = TTZ t []

tzGoTop :: TTermZipper -> TTerm
tzGoTop z = let TTZ tt _ = tzGoTop' z in tt
  where
  tzGoTop' :: TTermZipper -> TTermZipper
  tzGoTop' (TTZ t [] ) = (TTZ t [] )
  tzGoTop' z           = tzGoTop' $ tzUp z


tzUp :: TTermZipper -> TTermZipper
tzUp (TTZ t  (Lamb     x  typ : drobky) ) = (TTZ (TLam x t typ  ) drobky )
tzUp (TTZ t1 (AppLeft  t2 typ : drobky) ) = (TTZ (TApp t1 t2 typ) drobky )
tzUp (TTZ t2 (AppRight t1 typ : drobky) ) = (TTZ (TApp t1 t2 typ) drobky )

tzLeft :: TTermZipper -> TTermZipper
tzLeft (TTZ (TApp m n typ) drobky ) = (TTZ m $ AppLeft n typ : drobky ) 

tzRight :: TTermZipper -> TTermZipper
tzRight (TTZ (TApp m n typ) drobky ) = (TTZ n $ AppRight m typ : drobky ) 

tzDown ::  TTermZipper -> TTermZipper
tzDown (TTZ (TLam x m typ) drobky ) = (TTZ m $ Lamb x typ : drobky ) 


ttzTyp :: TTermZipper -> Typ
ttzTyp (TTZ t _) = ttermTyp t

showTTZ :: TTermZipper -> String
showTTZ (TTZ tt  drobky) = foldl f ("  {  " ++ show tt ++ "  }  ") drobky
 where 
  f :: String -> TTermDrobek -> String
  f str d = case d of
   Lamb     x  _ -> "(\\ "++ x ++ " -> " ++ str ++")"
   AppLeft  t2 _ -> "(" ++ str     ++ " " ++ show t2 ++")"
   AppRight t1 _ -> "(" ++ show t1 ++ " " ++ str     ++")" 

-- show --------------------------------------------

instance Show TTermZipper where show = showTTZ

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

instance Show Typ where show = showTyp'


showTyp :: Typ -> String
showTyp t | isTypNum t = show $ typeRank t
          | otherwise  = showTyp' t 

showTyp' :: Typ -> String
showTyp' (Typ t) = t
showTyp' (a :-> b) = "(" ++ showTyp a ++ "->" ++ showTyp b ++ ")"
showTyp' (TypVar a) = a
showTyp' (TypFun f ts) = "(" ++ f ++ " " ++ (intercalate " " (map show ts) ) ++ ")" 

-- eval -----------------------------------------------

--ttEval :: (Typeable a) => TTerm -> a -> a
--ttEval tterm a = eval (show tterm) a

---------------------------------------------------------------------------





-- ------------- --
-- prevod do SKI --
-- ------------- --

toSki' :: TTerm -> TTerm
toSki' tt = 
 let ret = toSki tt 
  in if checkTyp ret then ret else error "error in SKI conversion!"

toSki :: TTerm -> TTerm
toSki tt = case tt of 
 TVar x   t -> tt
 TVal v   t -> tt
 TApp m n t -> TApp (toSki m) (toSki n) t
 TLam x m t@(tx:->tm) -> case x `elem` (skiFV m) of
  False -> TApp (TVal "k" (tm:->tx:->tm) ) (toSki m) t
  True  -> case m of
   TVar _   _ -> TVal "i" t
   TLam y e _ -> toSki ( TLam x ( toSki m ) t )
   TApp p q _ -> 
    let tp = ttermTyp p
        tq = ttermTyp q
        tlp= tx:->tp 
        tlq= tx:->tq
        lp = TLam x p tlp
        lq = TLam x q tlq
        s  = TVal "s" ( tlp :-> tlq :-> t ) 
     in TApp ( TApp s ( toSki lp ) ( tlq :-> t) ) ( toSki lq ) t 
            


skiFV :: TTerm -> [Symbol]
skiFV tt = case tt of
 TVar x   _ -> [x]
 TVal v   _ -> []
 TApp m n _ -> nub $ (skiFV m) ++ (skiFV n) 
 TLam x m _ -> (skiFV m) \\ [x]


-- toSKI :: SKI -> SKI
-- toSKI S = S
-- toSKI K = K
-- toSKI I = I
-- toSKI x@(SKIvar _)     = x
-- toSKI   (SKIapp e1 e2) = SKIapp (toSKI e1) (toSKI e2) 
-- toSKI   (SKIlam x  e)  
--   | not $ elem x (skifv e) = SKIapp K (toSKI e)
--   | otherwise = case e of
--     (SKIvar _)    -> I
--     (SKIlam y ee) -> toSKI ( SKIlam x ( toSKI ( SKIlam y ee ) ) )
--     (SKIapp e1 e2)-> ( SKIapp (   SKIapp S  (toSKI (SKIlam x e1))   )  (toSKI (SKIlam x e2) ) )
-- 
-- 
-- data SKI = S | K | I | SKIapp SKI SKI | SKIlam VarName SKI | SKIvar VarName  
-- 
-- 
-- toSKI' :: String -> String
-- toSKI' = show . toSKI . termToSKI . parse'
-- 
-- 
-- termToSKI :: Term -> SKI
-- termToSKI (Lazy t _) = termToSKI t
-- termToSKI (Var  x  ) = SKIvar x
-- termToSKI (App m n ) = SKIapp (termToSKI m) (termToSKI n)
-- termToSKI (Lam x m ) = SKIlam x (termToSKI m)
-- 
-- 
-- skifv :: SKI -> [VarName]
-- skifv (SKIvar v)    = [v]
-- skifv (SKIapp p q)  = nub $ (skifv p) ++ (skifv q) 
-- skifv (SKIlam v p)  = (skifv p) \\ [v]
-- skifv _             = []
-- 
-- na :: (a->a) -> Int -> (a->a)
-- na f n x = foldr ($) x $ replicate n f
-- 
-- 
-- instance Show SKI where
--   show S = "S"
--   show K = "K"
--   show I = "I"
--   show (SKIapp m n) = "( " ++ show m ++ " " ++ show n ++ " )" 
--   show (SKIlam x m) = "( \\ " ++ show x ++ "." ++ show m ++ " )"
--   show (SKIvar x )  = show x










-- untyped term - parser --------------------------------------------

data Term  = Var Symbol          
           | Val Symbol           
           | Lam Symbol Term  
           | App Term   Term  


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




