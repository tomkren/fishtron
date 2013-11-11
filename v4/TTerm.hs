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
,CTTerm(..)
,ttermSubtree,ttermChangeSubtree,ttermDepth,TTermPos
,ttermPoses2WithTyps_onlyCompatible,ttermPoses2ByTyp
,optSki
) where

import Data.List ( nub, (\\) , intercalate )
import Data.Typeable ( Typeable )
import Text.ParserCombinators.Parsec ( Parser, ParseError, parse, (<|>), eof, char, many1, noneOf, skipMany1, space )

import Data.Either(Either(..),lefts,rights)

import Utils ( newSymbol , JShow(..) )
import JSONUtils(jsStr)

import qualified Data.Set as Set
import Data.Set (Set)

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



-----------------------------------------------
-- CTTerm --------------------------------------


data CTTerm = CTTerm Context TTerm

type TTermPos = [Int]


instance Show CTTerm where
 show (CTTerm ctx tt) = case ctx of
  [] -> show tt
  _  -> let vars = intercalate " " . map fst $ ctx
         in "\\ "++ vars ++ " -> " ++ show tt 

instance JShow CTTerm where
  jss_size (CTTerm _ tt) = Just $ ttermSize tt
  jshow_js ctt = Just . jsStr . jsShow $ ctt

ttermSize :: TTerm -> Int
ttermSize tterm = case tterm of
 TVar _   _ -> 1
 TVal _   _ -> 1
 TLam _ m _ -> 1 + ttermSize m
 TApp m n _ -> 0 + ttermSize m + ttermSize n



jsShow :: CTTerm -> String
jsShow (CTTerm ctx tt) =
 let vars = intercalate "," . map fst $ ctx
  in "function("++ vars ++ "){return " ++ jsShowBody tt ++ ";}"

jsShowBody :: TTerm -> String
jsShowBody tt = "\'fake\'"

-- let symbol' = transformExceptions symbol 
-- in case ttrees of
--     [] -> symbol'
--     _  ->
--      if isBinop symbol' && length ttrees == 2
--       then let [l,r] = ttrees
--                op    = tail . init $ symbol'
--             in "(" ++ jsShowBody l ++ op ++ jsShowBody r ++ ")"
--       else let inside = intercalate "," . map jsShowBody $ ttrees 
--             in symbol' ++ "(" ++ inside ++ ")"




ttermPoses2WithTyps :: TTerm -> ([(TTermPos,Typ)],[(TTermPos,Typ)])
ttermPoses2WithTyps t = 
  let xs  = poses2xx [] t 
      rev = map (\(pos,typ)->(reverse pos,typ))
   in ( rev . lefts $ xs , rev . rights $ xs )
 where
  poses2xx :: [Int] -> TTerm -> [ Either ([Int],Typ) ([Int],Typ) ]
  poses2xx pos tterm = case tterm of
    TVar x typ   -> [Left (pos,typ)]
    TVal x typ   -> [Left (pos,typ)]
    TLam x m typ -> (Right (pos,typ)) : poses2xx (1:pos) m
    TApp m n typ -> (Right (pos,typ)) : ( ( poses2xx (1:pos) m ) ++ ( poses2xx (2:pos) n ) )


ttermPoses2WithTyps_onlyCompatible :: TTerm -> TTerm -> ([(TTermPos,Typ)],[(TTermPos,Typ)])
ttermPoses2WithTyps_onlyCompatible prvni druhej =
 let (ters,nonters) = ttermPoses2WithTyps prvni
     setTypuVDruhym = ttermTypsSet druhej
     vyhodCoNemajTypVDruhym poziceSTypy 
       = filter ( \(_,typ) -> Set.member typ setTypuVDruhym ) poziceSTypy
  in ( vyhodCoNemajTypVDruhym ters , vyhodCoNemajTypVDruhym nonters  )


ttermTypsSet :: TTerm -> Set Typ
ttermTypsSet tterm = case tterm of
 TVar x   typ -> Set.singleton typ
 TVal x   typ -> Set.singleton typ
 TLam x m typ -> Set.insert typ (ttermTypsSet m)
 TApp m n typ -> Set.union (ttermTypsSet m) (ttermTypsSet n)

--ttermPoses2ByTyp
ttermPoses2ByTyp :: Typ -> TTerm -> ([TTermPos],[TTermPos])
ttermPoses2ByTyp typ t = 
  let xs  = poses2 [] t 
      rev = map reverse 
   in ( rev . lefts $ xs , rev . rights $ xs )
 where
  poses2 :: [Int] -> TTerm -> [ Either [Int] [Int] ]
  poses2 pos tterm = case tterm of
    TVar x typ'   | typ == typ' -> [Left pos]
                  | otherwise   -> []
    TVal x typ'   | typ == typ' -> [Left pos]
                  | otherwise   -> []
    TLam x m typ' | typ == typ' -> (Right pos) : (poses2 (1:pos) m)
                  | otherwise   -> poses2 (1:pos) m
    TApp m n typ' | typ == typ' -> (Right pos) : ( ( poses2 (1:pos) m ) ++ ( poses2 (2:pos) n ) )
                  | otherwise   -> (poses2 (1:pos) m) ++ (poses2 (2:pos) n)



ttermSubtree :: TTerm -> TTermPos -> TTerm
ttermSubtree t []     = t
ttermSubtree t (i:is) = case t of
 TVar x   typ -> error "in ttermSubtree; case TVar"
 TVal x   typ -> error "in ttermSubtree; case TVal"
 TLam x m typ -> ttermSubtree m is
 TApp m n typ | i == 1 -> ttermSubtree m is
              | i == 2 -> ttermSubtree n is


ttermChangeSubtree :: TTerm -> TTermPos -> TTerm -> (TTerm,TTerm)
ttermChangeSubtree tterm []     newSub = (newSub,tterm) 
ttermChangeSubtree tterm (i:is) newSub = case tterm of
 TVar x   typ -> error "in ttermSubtree; case TVar"
 TVal x   typ -> error "in ttermSubtree; case TVal"
 TLam x m typ -> let (m',oldSub) = ttermChangeSubtree m is newSub
                  in ( TLam x m' typ , oldSub )
 TApp m n typ | i == 1 -> let (m',oldSub) = ttermChangeSubtree m is newSub
                           in ( TApp m' n typ , oldSub )
              | i == 2 -> let (n',oldSub) = ttermChangeSubtree n is newSub
                           in ( TApp m n' typ , oldSub ) 


ttermDepth :: TTerm -> Int
ttermDepth tt = case tt of
 TVar _   _ -> 0
 TVal _   _ -> 0
 TLam _ m _ -> 1 + (ttermDepth m)
 TApp m n _ -> 1 + (max (ttermDepth m) (ttermDepth n))  





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


-- ---------------------------- --
-- optimalizovany prevod do SKI --
-- ---------------------------- --


optSki :: TTerm -> TTerm
optSki tt =  
 let ret = compile tt 
  in if checkTyp ret then ret else error "error in optSKI conversion!"


compile :: TTerm -> TTerm
compile tt = case tt of
 TApp m n typ -> TApp (compile m) (compile n) typ
 TLam x m typ -> abstr x (compile m) typ   
 TVar _ _     -> tt
 TVal _ _     -> tt

abstr :: Symbol -> TTerm -> Typ -> TTerm
abstr x m' typ@(a:->b) = case m' of
  TApp f1 f2 _ -> let f1t  = ttermTyp f1
                      f2t  = ttermTyp f2
                      lf1t = a :-> f1t
                      lf2t = a :-> f2t
                      lf1  = abstr x f1 lf1t
                      lf2  = abstr x f2 lf2t
                      expr = TApp
                              (TApp (TVal "s" (lf1t:->lf2t:->a:->b) ) lf1 (lf2t:->a:->b))
                              lf2 (a:->b)
                   in optim expr
  TVar y _ | x == y    -> TVal "i" typ
           | otherwise -> TApp (TVal "k" (b:->a:->b) ) m' typ
  TVal _ _             -> TApp (TVal "k" (b:->a:->b) ) m' typ


optim :: TTerm -> TTerm
optim tt = case tt of
  ( TApp (TApp (TVal "s" _) (TApp (TVal "k" _) p _ ) _ ) 
         ( TApp (TVal "k" _) q (_:->c) ) (a:->b)  ) 
   -> TApp (TVal "k" (b:->a:->b)) ( TApp p q b ) (a:->b) 
  ( TApp (TApp (TVal "s" _) (TApp (TVal "k" _) p _ ) _ ) 
         (TVal "i" _) a ) 
   -> p
  ( TApp (TApp (TVal "s" _) (TApp (TVal "k" _) p _ ) _ ) 
         (TApp (TApp (TVal "b" _) q _) r (_:->c) ) (a:->d) )
   -> let (_:->b) = ttermTyp r
       in (TApp 
          (TApp 
          (TApp (TVal "b_" ((c:->d):->(b:->c):->(a:->b):->(a:->d)) ) p ((b:->c):->(a:->b):->(a:->d)) )
          q ((a:->b):->(a:->d)) )
          r (a:->d) )
  ( TApp ( TApp (TVal "s" _) (TApp (TVal "k" _) p _) ((_:->b):->(a:->c)) ) q _) 
   -> TApp (TApp (TVal "b"  ((b:->c):->(a:->b):->(a:->c)) ) p ((a:->b):->(a:->c))  ) q (a:->c)
  (TApp (TApp (TVal "s" _) (TApp (TApp (TVal "b" _) p _) q _) _) (TApp (TVal "k" _) r _) typ)
   -> let pt = ttermTyp p
          qt = ttermTyp q
          rt = ttermTyp r
       in TApp (TApp (TApp (TVal "c_"  (pt:->qt:->rt:->typ) ) p (qt:->rt:->typ)) q (rt:->typ)) r typ
  (TApp (TApp (TVal "s" _) p _) (TApp (TVal "k" _) q _) typ) 
    -> let pt = ttermTyp p
           qt = ttermTyp q
        in TApp (TApp (TVal "c" (pt:->qt:->typ) ) p (qt:->typ)) q typ
  (TApp (TApp (TVal "s" _) (TApp (TApp (TVal "b" _) p _) q _) _ ) r typ)
    -> let pt = ttermTyp p
           qt = ttermTyp q
           rt = ttermTyp r
        in TApp (TApp (TApp (TVal "s_"  (pt:->qt:->rt:->typ) ) p (qt:->rt:->typ)) q (rt:->typ)) r typ
  x -> x

i :: a -> a
i x = x

k :: a -> b -> a
k x y = x

s :: (a->b->c) -> (a->b) -> a -> c
s f g x = f x (g x)

s_ :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
s_ c f g x = c (f x) (g x)

b::(b -> c) -> (a -> b) -> a -> c
b    f g x = f (g x)

b_ :: (c -> d) -> (b -> c) -> (a -> b) -> a -> d
b_ c f g x = c (f (g x))

c :: (a->b->c)->b->a->c
c f g x = f x g 

c_ :: (b->c->d)->(a->b)->c->a->d
c_ c f g x = c (f x) g

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




