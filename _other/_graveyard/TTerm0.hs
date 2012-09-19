-- TYPED FUNCTIONAL GENETIC PROGRAMMING V0.27 -- Tomáš Křen ----------- UTF-8 --

-- INFO ------------------------------------------------------------------------
 -- [ Info a ostatní komentáře jsou aktuální pro V0.25. ] 
 -- 
 -- Soubory projektu :

 -- TTerm.hs   - Vše podstatné. (Až to moc nabobtná, tak to rozdělím. 
 --              Zatím preferuji jeden soubor, abych to předčasně nerozdělil 
 --              nestrategicky.)
 -- Dist.hs    - Iplementace "pravděpodobnostního rozložení".
 -- Util.hs    - Některé všeobecné funkce.
 -- README     - Tento soubor.
 -- archiv.txt - Některé již nepoužívané kusy kódu. (Pro pohodlnější 
 --              vyhledaní než v zálohách. )
 --
 -- Tento soubor :
 --
 -- Kód je členěn do víceméně ucelených sekcí, každá sekce má své vlastní 
 -- vysvětlení. Jmenovitě to jsou (na ty nejpodstatnější ukazuje šipka):
 --
 --  * typy                                  <-
 --  * test cases                            <-
 --    * enviroment
 --    * tools for making fitness functions
 --  * GP core                               <-
 --    * crossover
 --  * prover                                <-
 --    * makers
 --    * inference rules
 --  * evaluation                            <-
 --  * utils
 --    * zipper
 --    * class instances

module TTerm ( ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Dynamic
import System.Random
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.PSQueue as Q -- Prioritní fronta

--import Text.ParserCombinators.Parsec hiding (State)
--import Text.ParserCombinators.Parsec (Parser,parse,letter,parse,parseTest,(<|>),(<?>),string,char,digit,many1)
--import Text.Parsec.Expr


import Util
import Dist2

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
 --       Base představuje bázi (kontext) známou z typových systémů (první složka),
 --            obohacenou o dvě pomocné struktury pro efektivnější práci.
 -- Dictionary umožňuje rychle se dostat ke konkrétní interpretaci nejakého 
 --            funkčního symbolu.  



type Nete       = (Typ,Base)
type Env        = [(Symbol,Typ,Comb)]
type Base       = ( Baze , [Typ] , Dictionary )   -- TODO ? oddělit fun. symboly od vars ?
type Baze       = [(Symbol,Typ)]
type Dictionary = Map.Map Symbol Comb

 -- NumSteps je počet kroků v důkazu nějakého termu.
 --        H je heuristický dolní odhad počtu zbývajících kroků k dokončení důkazu.  
 --   DeltaH je změna heuristického odhadu.

type NumSteps  = Int
type H         = Int
type DeltaH    = Int

 --      Law je z pohledu důkazů odvozovací pravidlo, z pohledu gramatiky termů přepisovací
 --          pravidlo. Pro neterminál vrací všechny termy, na které se vzhledem k tomuto
 --          pravidlu může tento neterminál přepsat. Z pohledu důkazů představuje jeden krok
 --          důkazu.
 --     LawH je Law obohacené o informaci o změně dolního odhadu pro potřebný počet kroků
 --          důkazu způsobený právě použitým krokem.
 -- TTermLaw je ekvivalent LawH nad celými termy. Vrací Nothing pro dokončený důkaz.

type Law       = Nete -> [TTerm]
type LawH      = Nete -> [(TTerm,DeltaH)]
type TTermLaw  = TTerm -> Maybe [(TTerm,DeltaH)]

 -- TTermCar je "obálka" pro TTerm využívaná během důkazu. Obsahuje pomocné informace
 --          o již uplynulém počtu kroků důkazu a dolním odhadu kroků potřebných
 --          pro dokončení důkazu.
 --   CarLaw je ekvivalent TTermLaw pro TTermCar.
 
type TTermCar = (TTerm , NumSteps , H )
type CarLaw   = TTermCar -> Maybe [TTermCar]

 --  FitVal představuje jednotky v kterých měříme fitness u řešení.
 --  FitFun je typ našich fitness funkcí.
 -- GPState reprezentuje stav ve výpočtu Genetického Programování.
 --         Postupně obsahuje následující složky: 
 --         * generátor náhodných čísel,
 --         * pravděpodobnostní rozložení jedinců v populaci,
 --         * "líheň" pro mutování,
 --         * enviroment daného problému.
 
type FitVal  = Double
type FitFun  = TTerm -> FitVal
type GPState = ( StdGen , Dist TTerm , Map.Map Typ [TTerm] , Env  )

 -- TTermZipper je struktura pro pohodlnou prací s termem a jeho podtermy.
 --             První složka je podterm , druhá složka je seznam, ze kterého
 --             hezky rekonstruujeme půodní term, případně "něco mezi".

type TTermZipper = (TTerm , [TTermDrobek] ) 
data TTermDrobek = AppLeft TTerm Typ | AppRight TTerm Typ | Lamb Symbol Typ deriving (Show)       

-- test cases ------------------------------------------------------------------



 --       ff1  Zatím zde máme jen triviální příklad, šlechtící funkce typu Int -> Int, 
 --            že pro vstup 42 dávají výstup s co největší absolutní hodnotou, s omezením
 --            že se tato funkce musí spočítat za maximálně 100 "betaredukcí".
 -- testCase1  Používáme enviroment 'env1' definovaný v následující sekci, velikost populace 50
 --            a 5 generací (tzn. s nultou 6 generací).

ff1 :: TTerm -> FitVal
ff1 f =  case (compute 100 $ f @@ (42::Int) )::Either TTerm Int of
  Left  _ -> 0
  Right i -> fromIntegral $ abs i

testCase1 = putList $ map distMax $ gp (mkStdGen 42424242) env1 (int:->int) ff1 50 10


ff2 :: TTerm -> FitVal
ff2 f =  case (compute 100 $ f @@ (42::Int) @@ ((84::Int)) )::Either TTerm Int of
  Left  _ -> 0
  Right i -> fromIntegral $ abs i

testCase2 = putList $ map distMax $ gp (mkStdGen 42424242) env1 (int:->int:->int) ff2 200 10


testCase3 = putList $ map distMax $ gp (mkStdGen 422) 
  env2 (real:->real) (ffInterp (\x->x*x+x+1) [0..10] ) 500 10

ffInterp :: (Double->Double) -> [Double] -> TTerm -> FitVal
ffInterp q xs f = 1 / ( 1 + err )
  where 
  err = sum $ map (interpErr f) $ map (\ x -> ( x , q x ) ) xs 
  interpErr :: TTerm -> (Double,Double) -> Double
  interpErr f (x,y) = case (compute 10 $ f @@ x )::Either TTerm Double of
    Left  _  -> 9999999
    Right y' -> let dy = y - y' in dy*dy



testCase4 = putList $ map distMax $ gp (mkStdGen 424248) 
  env3 (listInt:->int) (ffSum [0..10] ) 20 10

ffSum :: [Int] -> TTerm -> FitVal
ffSum xs f = 1 / ( 1 + fromIntegral ( ffSumErr xs f ) )

ffSumErr :: [Int] -> TTerm -> Int
ffSumErr xs f = case (compute 300 $ f @@ xs )::Either TTerm Int of
  Left  _ -> 9999999
  Right i -> abs $ (sum xs) - i

-- (\(Right (Val _ dyn _))->fromDynamic dyn :: Maybe Int)$ nf beta 4 $ (fromJust 
-- $ parseTTerm env3 $ "foldr + 0") @@ ([1,2,3]::[Int])



-- enviroment ------------------------------------------------------------------


int  = Typ "Int"
real = Typ "Real"
bool = Typ "Bool"
listInt = Typ "[Int]"

toDyn' :: (Typeable a) => a -> Comb
toDyn' = HaskComb . toDyn

mkComb env str = Comb $ fromJust $ parseTTerm env str

env4 = [
         ( "[]"    , listInt                                 , toDyn' ([]::[Int]) ) 
         -- když tam chybí [] tak se TC4 rozbije, zjistit proč
         ,(":"     , int :-> listInt :-> listInt             , ddotInt           )
         ,("+"     , int :-> int :-> int                     , plusFun           )
         ,("0"     , int                                     , toDyn' (0::Int)    )
         ,("foldr", (int:->int:->int):->int:->listInt:->int , foldrComb )    
       ]


env3 = [
         ( "[]"    , listInt                                 , toDyn' ([]::[Int]) )
         ,(":"     , int :-> listInt :-> listInt             , ddotInt           )
         ,("inc"   , int :-> int                             , incComb        )
        -- ,("foldr" , (int:->int:->int):->int:->listInt:->int , foldrInt          )
         ,("+"     , int :-> int :-> int                     , plusFun           )
         ,("0"     , int                                     , toDyn' (0::Int)    )
         ,("1"     , int                                     , toDyn' (1::Int)    )
         ,("3"     , int                                     , toDyn' (3::Int)    )
         ,("if" , bool :-> int :->  int :-> int  , IfComb          )
         ,("*"  , int :-> int :-> int            , kratFun        )   
         ,("-"  , int :-> int :-> int            , minusFun       )
         ,("=="  , int :-> int :-> bool           , eqFun          ) 
         ,("fac" , int:-> int                     , facComb       )
         ,("null", listInt :-> bool               , nullFun       )
         ,("foldr", (int:->int:->int):->int:->listInt:->int , foldrComb )    
         ,("head", listInt:->int     ,headFun)
         ,("tail", listInt:->listInt ,tailFun)
       ]


com :: (Typeable a) => TTerm -> a
com tt = case (compute 100000 tt )::((Typeable a) =>Either TTerm a) of
  Right i -> i

par = fromJust . parseTTerm env3

ttt1 = ( com $ par "foldr + 0" @@ ([1..100]::[Int])  ) :: Int

tt1 = par "+ 1 1"--"x:Int y:Int . + (+ x y) (* x y)"
tt2 = par "if (== 1 1)" --"x:Int y:Int . (z:Int .* x (+ z y)) (+ y y)"

incComb  = mkComb env1 "+ 1"
facComb = mkComb env3 "x : Int . if (== x 0) 1 (* x (fac (- x 1)) )"

foldrComb = mkComb env3 "f:Int->Int->Int z:Int xs:[Int] . if (null xs) z ( f (head xs) (foldr f z (tail xs)) )"

lol = parseTTerm env3 "x : Int . if (== x 0) 1 (* x (- x 1) )"
lol2 = parseTerm "x:Int .  (- x x )"
 
ddotInt  = toDyn' ( (:) :: Int -> [Int] -> [Int] )
foldrInt = toDyn' ( foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int )
plusFun  = toDyn' ((+) :: Int -> Int -> Int)
nullFun  = toDyn' (null:: [Int] -> Bool )
headFun  = toDyn' ( ( \ xs -> if null xs then 0  else head xs ) :: [Int] -> Int   ) 
tailFun  = toDyn' ( ( \ xs -> if null xs then [] else tail xs ) :: [Int] -> [Int] )


env1 =  [
          ( "abs", int :-> int                    , absFun         )
          ,("+"  , int :-> int :-> int            , plusFun        )
          ,("-"  , int :-> int :-> int            , minusFun       )
          ,("*"  , int :-> int :-> int            , kratFun        )            
          ,("%"  , int :-> int :-> int            , divvFun        )
          ,("1"  , int                            , toDyn' (1::Int) )
          ,("<"  , int :-> int :-> bool           , ltFun          )
          ,("if" , bool :-> int :->  int :-> int  , ifFun          )
        ]

env2 =  [
          ( "abs", real :-> real                    , absFunR          )
          ,("+"  , real :-> real :-> real           , plusFunR         )
          ,("-"  , real :-> real :-> real           , minusFunR        )
          ,("*"  , real :-> real :-> real           , kratFunR         )            
       --   ,("%"  , real :-> real :-> real           , divvFunR         )
          ,("1"  , real                             , toDyn' (1::Double))
       --   ,("<"  , real :-> real :-> bool           , ltFunR           )
       --   ,("if" , bool :-> real :->  real :-> real , ifFunR           )
        ]


absFun   = toDyn' (abs :: Int -> Int)
minusFun = toDyn' ((-) :: Int -> Int -> Int)
kratFun  = toDyn' ((*) :: Int -> Int -> Int)
divvFun  = toDyn' ((\a b->if b==0 then 1 else a `div` b) :: Int -> Int -> Int)
ltFun    = toDyn' ((<) :: Int -> Int -> Bool)
eqFun    = toDyn' ((==) :: Int -> Int -> Bool)
ifFun    = toDyn' ((\p a b ->if p then a else b) :: Bool -> Int -> Int -> Int)

absFunR   = toDyn' (abs :: Double -> Double)
plusFunR  = toDyn' ((+) :: Double -> Double -> Double)
minusFunR = toDyn' ((-) :: Double -> Double -> Double)
kratFunR  = toDyn' ((*) :: Double -> Double -> Double)
divvFunR  = toDyn' ((\a b->if b==0 then 1 else a / b) :: Double -> Double -> Double)
ltFunR    = toDyn' ((<) :: Double -> Double -> Bool)
ifFunR    = toDyn' ((\p a b ->if p then a else b) :: Bool -> Double -> Double -> Double)


 -- pro případné testovaní :

t2 = Lam "a" ( App  t1 
     (Var "a" int) (int) ) (int :-> int)


t1 = Lam "a" ( App(App(Val "*" kratFun (int :-> int :-> int )) 
     (Var "a" int) (int:->int)) (Var "a" int) (int) ) (int :-> int)
t0 = Val "5" (toDyn' (5::Int)) (int)




-- tools for making fitness functions ------------------------------------------

 -- @@ je ekvivalent aplikace funkce pro TTerm a nějakou haskellovskou hodnotu
 -- compute se používá pro evaluaci termu a "přetypování" na haskellovskou hodnotu 

(@@) :: (Typeable a, Show a) => TTerm -> a -> TTerm
f @@ x = let (a:->b) = ttermTyp f in App f (toTTerm x a) b 

compute ::  (Typeable a) => Int -> TTerm -> Either TTerm a
compute steps t = case nf beta steps t of
  Left  t'                       -> Left t'
  Right (Val _ (HaskComb dyn) _) -> Right $ fromJust $ fromDynamic dyn
  Right t'                       -> error $ "\n\nERR: " ++ show t ++ " ---> " ++ show t' 

-- GP core ---------------------------------------------------------------------

 -- 

gp :: StdGen -> Env -> Typ -> (TTerm->FitVal) -> Int -> Int -> [Dist TTerm]
gp gen env typ ff popSize numGen = evalState (gpLoop numGen) gpState0
  where
  gpState0 :: GPState
  gpState0 = mkGPState gen (mkDist $ mapFitness ff $ take popSize $ dk env typ ) env     

  gpLoop :: Int -> State GPState [Dist TTerm]
  gpLoop i = do 
    pop <- getPopulation
    if i > 0
    then do 
      gpStep ff
      pops <- gpLoop (i-1)
      return (pop:pops)
    else do 
      return [pop]
  
mkGPState :: StdGen -> Dist TTerm -> Env -> GPState
mkGPState gen pop env = ( gen , pop , Map.empty , env )

gpStep :: (TTerm->FitVal) -> State GPState ()
gpStep ff = do
  best     <- getBest
  popSize  <- populationSize
  parents  <- getWinners (popSize - 1)
  children <- xovers parents
  updatePopulation ff $ best:children

updatePopulation :: (TTerm->FitVal) -> [TTerm] -> State GPState ()
updatePopulation ff ts = state $ \ ( gen , _ , db , env ) -> 
  ( () , ( gen , mkDist $ mapFitness ff ts , db , env ) )

populationSize :: State GPState Int
populationSize = state $ \ s@( _ , pop , _ , _ ) -> ( distSize pop , s)

getPopulation :: State GPState (Dist TTerm)
getPopulation = state $ \ s@( _ , pop , _ , _ ) -> (pop , s)

getWinners :: Int -> State GPState [TTerm]
getWinners howMany = state $ \ ( gen , pop , db , env ) -> 
  let ( ws , gen' ) = distTake gen howMany pop in ( ws ,( gen' , pop , db , env ) )

getBest :: State GPState TTerm
getBest = state $ \ s@( _ , pop , _ , _ ) ->  ( fst $ fromJust $ distMax pop , s) --( fst $ distMax pop , s) 

getMutant :: Typ -> State GPState TTerm
getMutant typ = state $ \ ( gen , pop , db , env ) -> 
  case Map.lookup typ db of
    Nothing     -> let (x:xs) = dk env typ in 
                   ( x , ( gen , pop , Map.insert typ xs db , env) )
    Just (x:xs) -> ( x , ( gen , pop , Map.insert typ xs db , env) )

getRandomR :: (Random a) => (a,a) -> State GPState a
getRandomR range = state $ \ ( gen , pop , db , env ) -> 
  let (ret,gen') = randomR range gen in (ret,(gen',pop,db,env))

mapFitness :: (TTerm->FitVal) -> [TTerm] -> [(TTerm,FitVal)]
mapFitness fitness = map (\t-> (t,fitness t) )

-- crossover -------------------------------------------------------------------

xovers :: [TTerm] -> State GPState [TTerm]
xovers []         = return []
xovers [t]        = return [t]
xovers (t1:t2:ts) = do 
  (t1',t2') <- xover t1 t2
  ts'       <- xovers ts
  return $ t1':t2':ts'

xover :: TTerm -> TTerm -> State GPState ( TTerm , TTerm )
xover t1 t2 = 
  let candidates = smartCompatibleSubterms t1 t2  --  <------------------- tady se voli smart/normal
      canSize    = length candidates
   in if null candidates 
      then do return $ (t1,t2) 
      else do
        i <- getRandomR (0, canSize - 1 )
        let (z1,z2) = candidates !! i in crossover z1 z2

crossover :: TTermZipper -> TTermZipper -> State GPState ( TTerm , TTerm )
crossover (t1,ds1) (t2,ds2) = do 
 t1' <- treatFVs (t1,ds2)
 t2' <- treatFVs (t2,ds1)
 let  chis = ( makeVarsUnique $ tzGoTop (t2',ds1) , makeVarsUnique $ tzGoTop (t1',ds2) ) 
      pars = ( tzGoTop (t1 ,ds1) , tzGoTop (t2 ,ds2) )
  in  return $ checkXover pars chis

checkXover :: (TTerm,TTerm) -> (TTerm , TTerm) -> (TTerm , TTerm)
checkXover parents chs@(ch1,ch2) = 
 if checkTyp ch1 then
  if checkTyp ch2 then
   chs
  else err 
 else err
 where 
 err = error $ "xover typeCheck ERR: " ++
  show parents ++ " -> " ++ show chs

treatFVs :: TTermZipper -> State GPState TTerm
treatFVs (t,ds) = f (fv' t) t
  where
  boundVarsInDs = foldl (\acc x -> case x of 
    Lamb sym (typ:->_) -> if has acc sym
                          then           acc 
                          else (sym,typ):acc 
    _                  ->                acc ) [] ds
  has :: [(Symbol,Typ)] -> Symbol -> Bool
  has xs s = any (\(s',_)->s==s') xs
  f :: [(Symbol,Typ)] -> TTerm  -> State GPState TTerm
  f [] t = return t
  f ((fv,fvTyp):fvs) t = 
    if null withSameTyp
    then do
      mutant <- getMutant fvTyp
      f fvs $ subs fv t mutant 
    else do
      i <- getRandomR ( 0 , length withSameTyp - 1 )
      f fvs $ subs fv t $ Var (withSameTyp !! i) fvTyp
    where
    withSameTyp = map fst $ filter (\(_,t) -> t == fvTyp ) boundVarsInDs 
  fv' :: TTerm -> [(Symbol,Typ)]
  fv' (Var v   typ) = [(v,typ)]
  fv' (Val v _ _  ) = []
  fv' (App p q _  ) = nub $ (fv' p) ++ (fv' q) 
  fv' (Lam v p typ) = filter (\(s,_)->s/=v) (fv' p)   -- neefektivni

-- mozny vylepseni - použít toto, ALE dost spomaluje to testování na stejnost 
--  - zahodit prohozeni celych stromu (domenka : vzdy to prvni v seznamu)
--  - zahodit prohozeni stejnych veci
smartCompatibleSubterms :: TTerm -> TTerm ->  [ (TTermZipper,TTermZipper) ]
smartCompatibleSubterms t1 t2 
 = case filter ((\((sub1,_),(sub2,_))-> sub1 /= sub2 )) $ compatibleSubterms t1 t2 of -- nechcem prohozeni stejných subt.
  []         -> []
  xs@(_:xs') -> if ttermTyp t1 == ttermTyp t2 -- zahodit prohozeni celych stromu : vzdy to prvni ve vysledku
                then xs'                      -- funguje jen při křížení stejně otypovaných stromů,
                else xs                       -- což je tichý předpoklad, ale radši to ostestujem explicitně

compatibleSubterms :: TTerm -> TTerm ->  [ (TTermZipper,TTermZipper) ]
compatibleSubterms t1 t2 = concatMap (\(as,bs) -> [ (a,b) | a <- as , b <- bs ] ) 
  $ f (groupenSubterms t1) (groupenSubterms t2) 
  where
  f :: [(Typ,[TTermZipper])] -> [(Typ,[TTermZipper])] -> [([TTermZipper],[TTermZipper])]
  f []            _  = []
  f ((typ,ts):xs) ys = case lookup typ ys of
    Nothing  -> f xs ys
    Just ts' -> ( ts , ts' ) : f xs ys

groupenSubterms :: TTerm -> [(Typ,[TTermZipper])]
groupenSubterms t = map (\ts -> ( ttzTyp $ head ts ,ts) ) 
  $ groupBy (\t1 t2 -> ttzTyp t1 == ttzTyp t2 ) $ subterms t 

subterms :: TTerm -> [TTermZipper]
subterms term = subzips (term,[]) 
  where
  subzips :: TTermZipper -> [TTermZipper]
  subzips z@(t,_) = case t of
    Var _   _ -> [z]
    Val _ _ _ -> [z]
    Lam _ _ _ -> z : ( subzips $ tzDown z )
    App _ _ _ -> z : ( subzips (tzLeft z) ++ subzips (tzRight z) )

-- prover ----------------------------------------------------------------------

dk :: Env -> Typ -> [TTerm]
dk env goal = dk' $ Q.singleton car numNete
  where
  law  = mkCarLaw . mkTTermLaw $ axiomH +++ mpH +++ vodH  +++ (envAxiomH env)
  car@(_,_,numNete) = mkCar $ Temp (goal,mkBase env)

  dk' :: Q.PSQ TTermCar Int -> [TTerm]
  dk' q = 
    if (Q.null q)  then []
    else case Q.minView q of
      Nothing -> [] 
      Just ( car@(t,_,_) Q.:-> _ , q' ) -> case law car of
        Nothing   -> let t' = makeVarsUnique t 
                      in if checkTyp t' then t' : dk' q' else error $ "checkType ERR : " ++ show t' 
        Just cars -> dk' $ insertToQ q' cars



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

data Term = TAtom Symbol     
          | (Symbol,Typ) :. Term
          | Term :@ Term 
          deriving (Show)

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
  

-- makers ----------------------------------------------------------------------

mkBase :: Env -> Base
mkBase env = ( b' , typy2 ++ typy1 , Map.fromList b'' )
  where 
  typy1 = nub $ map snd b'
  typy2 = nub $ concatMap breakType typy1
  (b', b'') = foldr (\(name,typ,code) (acc',acc'') -> 
              ((name,typ):acc' , (name,code):acc'' )) ([],[]) env

mkLawH :: Law -> DeltaH -> LawH
mkLawH law i nete = map (\t->( t , i )) $ law nete

mkTTermLaw :: LawH -> TTerm -> Maybe [(TTerm,DeltaH)]
mkTTermLaw law term = do
  (Temp nete,ds) <- findNete term
  return [ (tzGoTop (term , ds), deltaH ) | (term,deltaH) <- law nete ]

mkCar :: TTerm -> TTermCar
mkCar t = (t , 0 , numNetes t)

mkCarLaw :: TTermLaw -> TTermCar -> Maybe [TTermCar]
mkCarLaw law (term,steps,h) = do
  xs <- law term
  return [ (term',steps+1,h+delta) |  (term',delta) <- xs ]

-- inference rules -------------------------------------------------------------

axiomH = mkLawH axiom (-1)
mpH    = mkLawH mp      1
vodH   = mkLawH vod     0

axiom :: Nete -> [TTerm]
axiom (typ,(b,_,dict) ) = [ term | (x,typ') <- b , typ == typ' ,
                            let term = symbolToTTerm dict x typ , 
                            (\t->case t of Var _ _ -> True ; _ -> False) term ]

mp :: Nete -> [TTerm]
mp (typ,base@(_,types,_) ) 
 = [ App (Temp ( typ' :-> typ , base )) (Temp ( typ' , base )) typ | typ' <- types ]
 
vod :: Nete -> [TTerm]
vod (Typ _         , _              ) = []
vod (typ@(a :-> b) , (ba,types,dict) ) = [ Lam x (Temp (b, base' ) ) typ ]
  where 
  x = newSymbol $ map fst ba
  base' = ( (x,a) : ba , nub $ a : types , dict ) -- TODO ? nemelo by bejt este něco v druhý části base' ?

envAxiomH :: Env -> Nete -> [(TTerm,DeltaH)]
envAxiomH env = foldr (+++) (\_->[]) (map bbLawH env) 

bbLawH :: (Symbol,Typ,Comb) -> Nete -> [(TTerm,DeltaH)]
bbLawH (fName , fTyp, _) (neteTyp,base@(_,_,dict) ) = case matchSufix neteTyp fTyp of
  Nothing -> []
  Just ts -> [ (foldl (\acc typ -> App acc (Temp (typ,base) ) (typFromAcc acc) ) 
               (symbolToTTerm dict fName fTyp) ts  , length ts - 1 )  ]
  where
  typFromAcc acc = let (t1:->t2) = ttermTyp acc in t2

-- zunikátnění vázaných proměnných ---------------------------------------------

makeVarsUnique :: TTerm -> TTerm
makeVarsUnique tt = (\(a,_,_)->a) $ u (varNames tt) [] tt
  where  
  u :: [Symbol] -> [Symbol] -> TTerm -> ( TTerm , [Symbol] , [Symbol] )
  u allVars occupied t = case t of
    Lam x m typ@(a:->b) -> 
     if x `elem` occupied
     then let x' = newSymbol allVars
              (m',all',occ') = u (x':allVars) (x':occupied) (subs x m (Var x' a) )
           in ( Lam x' m' typ , all' , occ' )
     else let (m',all',occ') = u allVars (x:occupied) m
           in ( Lam x m' typ , all' , occ' )
    App m n typ -> 
     let (m',all' ,occ' ) = u allVars occupied m
         (n',all'',occ'') = u all'    occ'     n
      in ( App m' n' typ , all'' , occ'' )
    _           -> ( t , allVars , occupied )

-- evaluation ------------------------------------------------------------------

nf :: (TTerm -> Maybe TTerm) -> Int -> TTerm -> Either TTerm TTerm
nf strat = nf'
    where
    nf' 0 t = Left t 
    nf' n t = case strat t of 
      Nothing -> Right t
      Just t' -> nf' (n-1) t'

beta ::  TTerm -> Maybe TTerm
beta ( App (Lam x m    _) n              _  ) = Just $ subs x m n
beta ( App (Val s1 (HaskComb dyn1) _) 
           (Val s2 (HaskComb dyn2) _)     typ) = Just $ Val ("["++s1++" "++s2++"]") (HaskComb $ dynApp dyn1 dyn2) typ 
beta ( App m@(Val _ (HaskComb _) _)  n   typ) = do n' <- beta n ; return $ App m n' typ 
beta ( App m@(Val _ (Comb comb) _)  n    typ) = Just $ App comb n typ 

beta ( App ( App ( App (Val _ IfComb _) (Val _ (HaskComb dynBool) _) _)  b _ ) c _ )  = case fromDynamic dynBool of
  Just True  -> Just b
  Just False -> Just c
  Nothing    -> error "ERR: in if in beta"

beta ( App ifC@(Val _ IfComb _) m typ) = do m' <- beta m ; return $ App ifC m' typ


beta ( App m              n              typ) = do m' <- beta m ; return $ App m' n typ
beta t                                        = Nothing

  
betaErr ::  TTerm -> Either TTerm (Maybe TTerm)
betaErr t = if checkTyp t then Right $ beta t else Left t 

nfErr :: (TTerm -> Either TTerm (Maybe TTerm)) -> Int -> TTerm -> Either TTerm TTerm
nfErr strat i tt = nf' i tt
    where
    nf' 0 t = Left t 
    nf' n t = case strat t of 
      Left errT       -> error $ "beta r. err: " ++ show tt ++ " ---> " ++ show errT   
      Right Nothing   -> Right t
      Right (Just t') -> nf' (n-1) t'

subs :: Symbol -> TTerm -> TTerm -> TTerm 
subs x m n = fst $ sub (varNames m) m x n   

sub :: [Symbol] -> TTerm -> Symbol -> TTerm -> (TTerm,[Symbol])
sub vns x@(Val _ _ _) _ _ = (x,vns)  
sub vns x@(Var v _) var vloz 
    | v == var  = (vloz,vns)
    | otherwise = (x,vns)  
sub vns (App p q typ) var vloz = (App t1 t2 typ , vns'')
    where 
    (t1,vns')  = sub vns  p var vloz
    (t2,vns'') = sub vns' q var vloz
sub vns x@(Lam v p typ) var vloz
    | v == var  = (x,vns)
    | not (elem v (fv vloz)) = (Lam v t typ , vns'') 
    | otherwise = sub vns' term' var vloz
    where
    (t,vns'') = (sub vns p var vloz)
    (term',vns') = rename vns x

varNames :: TTerm -> [Symbol]
varNames = nub . varNames'
    where
    varNames' (Var v   _)  = [v]
    varNames' (Val v _ _)  = [v]
    varNames' (App p q _)  = varNames' p ++ varNames' q
    varNames' (Lam v p _)  = v : varNames' p

fv :: TTerm -> [Symbol]
fv (Var v   _) = [v]
fv (Val v _ _) = [v] -- TODO neni to nejaka blbost davat je do FV, kouknout k cemu se to presne pouziva
fv (App p q _) = nub $ (fv p) ++ (fv q) -- neefektivni
fv (Lam v p _) = (fv p) \\ [v]

rename :: [Symbol] -> TTerm -> (TTerm,[Symbol])
rename vns (Lam v m typ@(varTyp :-> _) ) = ((Lam new m' typ),new:vns')
    where 
    new             = newSymbol vns
    (m',vns')       = sub vns m v (Var new varTyp)
rename vns t = (t,vns)

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

breakType :: Typ -> [Typ]
breakType t@(Typ _) = [t]
breakType (a :-> b) = breakType a ++ breakType b

numNetes :: TTerm -> Int
numNetes t = case t of
  ( Var _ _  ) -> 0
  ( Val _ _ _) -> 0 
  ( Lam _ m _) -> numNetes m
  ( App m n _) -> numNetes m + numNetes n
  ( Temp _ )   -> 1

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

insertToQ :: Q.PSQ TTermCar Int -> [TTermCar] -> Q.PSQ TTermCar Int
insertToQ q [] = q 
insertToQ q ((car@(_,numSteps,numNete)):cars) = insertToQ (Q.insert car (numSteps+numNete) q) cars 

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

-- parser ----------------------------------------------------------------------

parseTTerm :: Env -> String -> Maybe TTerm
parseTTerm env str = do
  t <- parseTerm str 
  typeIt env t


-- Tuto sekci by bylo asi hezké přepracovat pomocí nějakého pěkného
-- funkcionálního parseru, ještě je ale moc neumim, tak sem to napsal takhle.

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

-- TODO ------------------------------------------------------------------------

 --   - VYŘEŠIT TEN BUG S ff2

 --   - Udelat chytrou lihen : v kazdym stelarku prihodit vzdy dvojnasobek predchoziho prihozu
 --     a tento zamychat
 --   - Elitizmus (zatim pro nejlepsiho, pozdeji zobecnenej)
 --   - Chytry krizeni (zahodit moznost prohodit stromy a vymenit stejny podtermy )
 --   - Promyslet napad s kompletnim prozkoumanim moznejch krizeni / nejakej baldwinismus
 --     argument pro: pri typovanym programu je mnohem min moznosti nez pri netypovanym  

--------------------------------------------------------------------------------

