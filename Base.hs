--   Účelem modulu Base je definice typu TTerm a s ním spolupracujících funkcí.
-- TTerm neboli "oTypovaný TERM" můžeme nazvat "základním typem celého programu",
-- protože reprezentuje termy/programy, které "šlechtíme". Požíváme ho jak při generování
-- termů, tak při evaluaci (alespoň zatím).
-- Přesnější charakterizací než "otypovaný term" je "rozpracovaný otypovaný term",
-- v tom smyslu, jako je pro lidského programátora rozpracovaný zdrojový kód nehotového programu.
-- Ve zkratce se jedná o lambda term rozšířený o "položku typ" a o "možnost být rozdělaný".
--
--   Dříve než se dostaneme k formálnějšímu rozboru typu TTerm je snad vhodné zmínit, jakým
-- způsobem se nyní TTermy evaluují a jakým způsobem to chci změnit v budoucnu.
-- Oproti klasickému lambda kalkulu rozlišujeme mezi "proměnnou" a "hodnotou". 
-- Hodnoty odpovídají vestavěným kombinátorům - tyto kombinátory jsou (když to trochu zjedodušíme) 
-- Hasskellovské hodnoty "zabalené" pomocí knihovny Data.Dynamic umožňující obejít typový systém Haskellu
-- (o zjednodušení zde mluvíme protože existují i jiné typy hodnot/kombinátorů, které ale pro nás teď
-- nemá smysl uvažovat). TTermy vyhodnocujeme líně. Kýžené líné vyhodnocování je však podstatně 
-- komplikováno právě zmíněnými vestavěnými kombinátory: do Haskellovské funkce musíme dosadit 
-- haskellovskou hodnotu, čili ne TTerm. Tento problém řešíme tak, že slevíme z úplné lenosti
-- a tím vzniká ne uplně pěkný hybrid. U tohoto hybrydu jsem ale prozatím zůstal, protože jsem
-- považoval za lepší nejdříve udělat "jakž-takž" fungující prototyp celého systému a až po této fázy
-- začít novou iteraci vývoje a celou věc přepsat "bytelně". (To má za následek nepěkné vedlejší 
-- efekty, jako např. neohrabané podchycení kombinátoru "if" atd.)
--   Čím tedy nahradit toho současného hybrida? V zásadě mám dva kandidáty přičemž první z nich je 
-- můj jasný oblíbenec, ale je pracnější:
--    (A) Opustit snahu "expoitovat" haskellovské hodnoty a místo toho sáhnout k napsaní si
--        pořádného interpretu podle [Jones, S. P. (1991). Implementing Functional Languages] 
--    (B) Použít knihovnu Hint (což je Runtime Haskell interpreter (GHC API wrapper)).
--  Pro favorizaci možnosti (A) mám několik důvodů: Při evaluaci považuji za velice užitečné
-- mít k dispozici možnost omezit maximální počet beta redukcí případně jiné podobné možnosti.
-- To by mělo jít (snad) dobře, díky tomu že zmíněná kniha je podrobný tutoriál, čili budu mít
-- v ruce kód, kterému budu rozumět a tím padem ho budu moci hezky rozšířovat. Další důvod
-- je chuť proniknout do tajů funkcionállních překladačů.
--   Na obhajobu (B) lze říct, že by kýžené funkcionality šlo dosáhnout nějakými časovými omezeními
-- na maximální dobu běhu. Ale přijde mi, že z toho jde takový ten pocit "na komára s kanónem".
-- Nejobjektivnějším rozhřešením by nejspíš bylo otestovat, co je rychlejší (což neumím odhadnout). 
--   Ješte se nabízí možnost (C) slevit z lenosti a pak by to možná šlo udělat pomocí součásné metody
-- dostatečně konzistentní, tuhle metodu ale zatím moc nezvažuji jako finální řešení, spíš jako mezifázi.
--
-- TODO
-- * Z toho dlouhýho udělat poznámku pod čarou (obohatit o uvahu že by šlo balit dvojitě..)
-- * Zavýst ten kalkul, podobne jako se zavádí lambda kalkul
-- * Jaký uvažujem typy zatim
-- * Rozebrat Nete, Env, ContextCar, Context, Dictionary 
-- * v kratkosti odbýt TTermZipper
-- * Term jakožto možnost zadat typ jen v lambdach a dopočíst
-- * v rychlosti o funkcích

module Base 
( TTerm      (..) , Typ        (..) , Comb(..)
, TTermZipper(..) , TTermDrobek(..) , Term(..)

, Nete , Symbol , Env , ContextCar

, typeIt     , checkTyp   , toTTerm  , ttermTyp
, breakType  , findNete   , numNetes , symbolToTTerm 
, matchSufix , tzGoTop    , tzDown   , tzLeft        
, tzRight    , ttzTyp     , hasNete  , mkContextCar

,Context
) where

--module Types () where

import Data.List
import Data.Dynamic
import System.Random
import qualified Data.Map as Map

import Dist

-- Typy ------------------------------------------------------------------------

 -- Všechny použité typy (až na Dist) jsou zde.
 -- TTerm je klíčovým typem představujícím otypovaný lambda term.
 --       Za zmínku stojí konstruktory Val a Temp :
 --       Val v sobě drží nějakou (haskellovskou) hodnotu (např. funkci
 --       jako "+" nebo konstantu jako "23"), na rozdíl od
 --       Var, která drží symbolickou proměnnou termu. 
 --       Temp je zatím nedodělaná část rozdělaného TTermu.  
 --   Typ zatím neřeším produkt a koprodukt, ale časem doufám budu.

type Symbol  = String
data Comb    = HaskComb Dynamic | Comb TTerm | IfComb deriving (Show)

data TTerm = Var Symbol        Typ  
           | Val Symbol Comb   Typ   
           | Lam Symbol TTerm  Typ
           | App TTerm  TTerm  Typ
           | Temp Nete -- deriving (Show)

data Typ  = Typ Symbol
          | Typ :-> Typ
          deriving (Eq,Ord)
infixr 7 :->

 --       Nete znamená Neterminál (ve smyslu rozdělaného slova gramatiky). 
 --            Krom Typu má k sobě ještě Bazi (aby se vědelo, které proměnné 
 --            můžeme použít,až bude nahrazen za něco jiného).
 --        Env neboli enviroment představuje paletu stavebních kamenů, 
 --            ze kterých můžeme stavět termy.
 -- ContextCar představuje bázi (kontext) známou z typových systémů (první složka),
 --            obohacenou o dvě pomocné struktury pro efektivnější práci.
 -- Dictionary umožňuje rychle se dostat ke konkrétní interpretaci nejakého 
 --            funkčního symbolu.  

type Nete       = (Typ,ContextCar)
type Env        = [(Symbol,Typ,Comb)]
type ContextCar = ( Context , [Typ] , Dictionary )   -- TODO ? oddělit fun. symboly od vars ?
type Context    = [(Symbol,Typ)]
type Dictionary = Map.Map Symbol Comb

 -- TTermZipper je struktura pro pohodlnou prací s termem a jeho podtermy.
 --             První složka je podterm , druhá složka je seznam, ze kterého
 --             hezky rekonstruujeme půodní term, případně "něco mezi".

type TTermZipper = (TTerm , [TTermDrobek] ) 
data TTermDrobek = AppLeft TTerm Typ | AppRight TTerm Typ | Lamb Symbol Typ deriving (Show)       

data Term = TAtom Symbol     
          | (Symbol,Typ) :. Term
          | Term :@ Term 
          deriving (Show)

-- class instances -------------------------------------------------------------
{----}
instance Show TTerm where
    show x = case x of
        (Lam v m typ)  -> lam v m    --  ++ " : " ++ show typ
        (Temp (typ,_)) -> show typ
        _              -> show' x    --  ++ " : " ++ (show $ ttermTyp x)
        where  
        show' (Temp (typ,_)) = show typ
        show' (Var v _)   = v        
        show' (Val v _ _) = v
        show' (App m n _) = 
            show' m ++ " " ++ show'' n 
            where
            show'' (App a b _) = "(" ++ show' a ++ " " ++ show'' b ++ ")"
            show'' x         = show' x 
        show' (Lam v m _)    = "(" ++ lam v m ++ ")"    
        lam  v m             = "\\" ++ lam' v m
        lam' v (Lam v' m' _) = v ++ " " ++ lam' v' m'
        lam' v m             = v ++ "." ++ show' m

instance Show Typ where
  show (Typ t)   = t
  show (a :-> b) = "(" ++ show a ++ "->" ++ show b ++ ")"

instance Eq TTerm where
  ( Var x _  ) == ( Var y _  ) = x == y
  ( Val x _ _) == ( Val y _ _) = x == y
  ( Lam a b _) == ( Lam c d _) = a == c && b == d
  ( App a b _) == ( App c d _) = a == c && b == d
  ( Temp (t1,_) ) == ( Temp (t2,_) ) = t1 == t2 
  _ == _  = False

instance Ord TTerm where 
  compare ( Var x _  ) ( Var y _  ) = compare x y
  compare ( Val x _ _) ( Val y _ _) = compare x y
  compare ( Lam a b _) ( Lam c d _) = let ac = compare a c in if ac /= EQ then ac else compare b d
  compare ( App a b _) ( App c d _) = let ac = compare a c in if ac /= EQ then ac else compare b d
  compare ( Temp (t1,_) ) ( Temp (t2,_) ) = compare t1 t2 
  compare (Var _ _) _ = LT
  compare (Val _ _ _) _ = LT
  compare (Lam _ _ _) _ = LT
  compare (App _ _ _ ) _ = LT
  compare (Temp _) _= LT

-- context stuff ---------------------------------------------------------------

mkContextCar :: Env -> ContextCar
mkContextCar env = ( b' , typy2 ++ typy1 , Map.fromList b'' )
  where 
  typy1 = nub $ map snd b'
  typy2 = nub $ concatMap breakType typy1
  (b', b'') = foldr (\(name,typ,code) (acc',acc'') -> 
              ((name,typ):acc' , (name,code):acc'' )) ([],[]) env

breakType :: Typ -> [Typ]
breakType t@(Typ _) = [t]
breakType (a :-> b) = breakType a ++ breakType b


-- utils -----------------------------------------------------------------------

toTTerm :: (Typeable a, Show a) => a -> Typ -> TTerm
toTTerm x typ = Val (show x) (HaskComb $ toDyn x) typ 

toTyp :: (a,b,Typ) -> Typ
toTyp (_,_,typ) = typ

ttermTyp :: TTerm -> Typ
ttermTyp t = case t of
  Var _   typ -> typ  
  Val _ _ typ -> typ
  Lam _ _ typ -> typ
  App _ _ typ -> typ
  Temp (typ,_)-> typ

numNetes :: TTerm -> Int
numNetes t = case t of
  ( Var _ _  ) -> 0
  ( Val _ _ _) -> 0 
  ( Lam _ m _) -> numNetes m
  ( App m n _) -> numNetes m + numNetes n
  ( Temp _ )   -> 1

hasNete :: TTerm -> Bool
hasNete t = case findNete t of
  Nothing -> False
  _       -> True

matchSufix :: Typ -> Typ -> Maybe [Typ]
matchSufix neteTyp fTyp = 
  if neteTyp == fTyp then Just []
  else case fTyp of
    Typ _   -> Nothing
    a :-> b -> do 
      ts <- matchSufix neteTyp b
      return (a:ts) 

symbolToTTerm :: Dictionary -> Symbol -> Typ -> TTerm
symbolToTTerm dict sym typ = case Map.lookup sym dict of 
  Nothing   -> Var sym      typ
  Just comb -> Val sym comb typ


-- zipper ----------------------------------------------------------------------

mkTz :: TTerm -> TTermZipper
mkTz t = (t,[])

ttzTyp :: TTermZipper -> Typ
ttzTyp (t,_) = ttermTyp t

tzUp :: TTermZipper -> TTermZipper
tzUp ( t  , Lamb     x  typ : drobky ) = ( Lam x t typ   , drobky )
tzUp ( t1 , AppLeft  t2 typ : drobky ) = ( App t1 t2 typ , drobky )
tzUp ( t2 , AppRight t1 typ : drobky ) = ( App t1 t2 typ , drobky )

tzGoTop :: TTermZipper -> TTerm
tzGoTop z = fst $ tzGoTop' z
  where
  tzGoTop' :: TTermZipper -> TTermZipper
  tzGoTop' ( t , [] ) = ( t , [] )
  tzGoTop' z          = tzGoTop' $ tzUp z

tzLeft :: TTermZipper -> TTermZipper
tzLeft ( App m n typ , drobky ) = ( m , AppLeft n typ : drobky ) 

tzRight :: TTermZipper -> TTermZipper
tzRight ( App m n typ , drobky ) = ( n , AppRight m typ : drobky ) 

tzDown ::  TTermZipper -> TTermZipper
tzDown ( Lam x m typ , drobky ) = ( m , Lamb x typ : drobky ) 

isTzNete :: TTermZipper -> Bool
isTzNete (Temp _,_) = True
isTzNete _          = False

findNete :: TTerm -> Maybe TTermZipper
findNete t = findNete' (t,[])
  where
  findNete' :: TTermZipper -> Maybe TTermZipper
  findNete' z@(Var _ _   ,_) = Nothing
  findNete' z@(Val _ _ _ ,_) = Nothing
  findNete' z@(Temp _    ,_) = Just z
  findNete' z@(Lam _ _ _ ,_) = findNete' $ tzDown z
  findNete' z@(App _ _ _ ,_) = case findNete' $ tzLeft z of
    ok@(Just _) -> ok
    Nothing     -> findNete' $ tzRight z

-- checkType -------------------------------------------------------------------

checkTyp :: TTerm -> Bool
checkTyp t = checkTyp' t (ttermTyp t) []
  where
  checkTyp' :: TTerm -> Typ -> [(Symbol,Typ)] -> Bool
  checkTyp' t wTyp baze = case t of
   Var x typ             -> wTyp == typ && (x,typ) `elem` baze
   Val f _ typ           -> wTyp == typ
   Lam x m typ@(a :-> b) -> wTyp == typ && checkTyp' m b ((x,a):(filter (\(s,_)->s/=x) baze ) )
   App m n typ           -> wTyp == typ && 
                            let mTyp = ttermTyp m
                                nTyp = ttermTyp n
                             in case mTyp of
                               (a :-> b) -> nTyp == a && b == typ &&
                                            checkTyp' m mTyp baze &&
                                            checkTyp' n nTyp baze
                               _         -> False
   _                     -> False

-- dedukce typu ----------------------------------------------------------------

typeIt ::  Env -> Term -> Maybe TTerm
typeIt env term = case f [] term of
  Just tterm -> if checkTyp tterm then Just tterm else Nothing
  Nothing    -> Nothing
  where
  f ::  [(Symbol,Typ)] -> Term -> Maybe TTerm
  f gama t = case t of
    TAtom x -> case lookup x gama of
      Just typ -> Just $ Var x typ
      Nothing  -> case envFind env x of
        Just (typ,dyn) -> Just $ Val x dyn typ 
        Nothing        -> Nothing
    xt@(x,xTyp):.m -> case f (xt:gama) m of
      Just m' -> let mTyp = ttermTyp m' 
                  in Just $ Lam x m' (xTyp :-> mTyp)  
      Nothing -> Nothing
    m :@ n -> case f gama m of
      Just m' -> case f gama n of 
        Just n' -> case ttermTyp m' of
          a:->b -> if a == ttermTyp n'
                   then Just $ App m' n' b
                   else Nothing
          _     -> Nothing
        Nothing -> Nothing
      Nothing -> Nothing

envFind :: Env -> Symbol -> Maybe (Typ,Comb)
envFind []      _                = Nothing
envFind ((sym',typ,comb):env) sym 
 | sym' == sym = Just (typ,comb) 
 | otherwise   = envFind env sym


