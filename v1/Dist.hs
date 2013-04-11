-- Dist představuje implementaci  distribuce (pravděpodobnostního rozložení).
-- Distribucí máme na mysli datovou strukturu,
-- obsahující prvky a k nim asociované pravděpodobnosti.
-- Typ této datové struktury je parametrizován typem prvků distribuce.
-- Nad touto strukturou máme tři základní operace: 
--  * Vytvoření distribuce                                   .... O(n)
--  * Náhodný výběr z distribuce                             .... O(log n) 
--  * Náhodný výběr z distribuce s odebráním vybraného prvku .... O(log n)
-- Vytvořit distribuci můžeme dvěma způsoby: 
--   Jednodušší způsob je pomocí seznamu hodnot a k nim přiřazených "pravděpodobností".
-- Přesněji řečeno, se nejdená o pravděpodobnosti ve formálním smyslu a to v tom,
-- že jejich součet nemusí být 1, Dist už se sám postará o normalizaci.
-- Navíc se informace o původních hodnotách nezapomíná, což je mnohdy příjemná vlastnost. 
-- Např. když chceme populaci jedinců reprezentovat jako distribuci jedinců, kde
-- pravděpodobnosti odpovídá jejich fitness. Tímto přístupem můžeme uchovat obě informace
-- (tzn. fitness i pravděpodobnost) úsporně a na jediném místě.
-- Příklad: [('a',5),('b',35),('c',10)] bude odpovídat pravděpodobnostnímu rozložení
-- písmen, přičemž pravděpodobnost 'a' je 0.1, 'b' je 0.7, 'c' je 0.2 a všech ostatních písmen 0.
--   Složitější způsob zadání distribuce umožňuje mimo seznamu s prvky (hodnota,pravděpodobnost)
-- i prvky ("funkce distribuce",pravděpodobnost). Funkcí distribuce zde máme na mysli funkci
-- z intervalu <0,1> do "hodnot". [TODO zjistit jak se "funkce distribuce" jmenuje oficiálně]
-- Takto zadaná distribuce má přímočarou interpretaci: z rovnoměrného rozdělení na intervalu <0,1> 
-- vybereme náhodné číslo, na nějž aplykujeme danou funkci, čímž dostaneme vyberaný prvek.
-- (Poznamenejme, že první způsob zadaní by šlo úplně vypustit, a simulovat ho konstantní
-- "funkcí distribuce" - což ovšem z praktických důvodů nebudeme dělat.)
-- Datový typ Dist nám tímto způsobem umožňuje pohodlně skládat jednodušší distribuce do složitějších.
-- Příklad: [ ( (*2) , 40 ) , ( (+10) , 60 ) ] odpovídá rozložení, kde s pravděpodobností 0.4
-- bude vybráno číslo z intervalu <0,2> (každé z nich se stejnou pravděpodobností) a s pravděpodobností 
-- 0.6 bude vybráno číslo z <10,11> (každé z nich se stejnou pravděpodobností).
--
--   Dist je vnitřně reprezentován binárním stromem. Z důvodu efektivnějších operací si dále uchovává informaci 
-- o počtu listů a součtu (nenormalizovaných) pravděpodobností. 
-- Listy bin. stromu jsou dvou typů, odpovídající dvěma typům konstrukčních dvojic (hodnota,pst) respektive
-- (distr. fce, pst). Nelistové uzly bin. stromu obsahují "dělící" hodnotu z intervalu <0,1>. Tato
-- dělící hodnota je rovna součtu normalizovaných pravděpodobností prvků v levém podstromu. 
-- Neformálně funguje výběr z distribuce následovně: Náhodně vybereme číslo z intervalu <0,1>, kteréžto
-- pošleme koření stromu. Pokud je toto číslo menší než dělící hodnota tohoto uzlu, vybíraný prvek se 
-- nachází v levém podstromu; jinak se nachází v pravém podstromu. Toto číslo vhodným způsobem přepočítáme
-- a odešleme zvolenému podstromu, kde se postup rekurzivně opakuje. Pokud narazíme na list prvního typu, 
-- vracíme v něm obsazenou hodnotu; pokud na list druhého typu, vracíme hodnotu vzniklou dosazením onoho 
-- čísla z <0,1> do "funkce distribuce".
--   Při výběru s odebráním vybraného prvku se situace komplikuje o to, že musíme vhodným
-- způsobem přepočítat některé dělící hodnoty. Tyto hodnoty jsou však jen na cestě k danému prvku
-- a tak je časová složitost stále O(log n).
--
--   Nutno poznamenat, že struktura je navržena hlavně pro praci s listy prvního typu a tak některé
-- operace, jako výběr s odebráním nebo nalezení prvku s maximální pravděpodobností, se chovají pro
-- distribuce obsahující listy druhého typu jinak, než by bylo formálně korektní. 
-- Konrétně: Odebíráme vždy celý list stromu; a při nalezení prvku s maximální pravděpodobností
-- hledáme ve skutečnosti list s maximální pravděpodobností, přičemž v případě listu typu dva 
-- vracíme reprezentanta pro 0.5.
-- (Činíme tak z toho důvodu, že zde mícháme dohromady diskrétní a spojité rozdělení, přičemž nám jde 
-- ale spíše o ty diskrétní vlastnosti a ty spojité se nám hodí jen specifickým způsobem.)

module Dist 
( Dist( )
, mkDist  
, mkDist2
,  distToList 
,  distIsEmpty
, distMax
, distMin
, distTake
, distTake_new , distGet
, distCut
, distObtain
, distSize
, distAvg
) where

import System.Random
import Text.Printf
import Control.Monad.State

import Util

data Dist  a = Dist (DTree a) (Suma,Size) | DEmpty  
data DTree a = DLeaf  (a,Double)
             | DLeaf2 (Double->a,Double)
             | DNode  Double (DTree a) (DTree a) 

type Suma    = Double
type Size    = Int
type Percent = Double
type CutP    = Double

instance (Show a) => Show (Dist a) where show = showDist

instance Functor Dist where
  fmap f d = case d of
   DEmpty -> DEmpty
   Dist tree ss -> Dist (fmap f tree) ss

instance Functor DTree where
  fmap f t = case t of 
   DLeaf  (x,val)   -> DLeaf  ( f x   , val )
   DLeaf2 (g,val)   -> DLeaf2 ( f . g , val )
   DNode mark t1 t2 -> DNode mark (fmap f t1) (fmap f t2) 

distIsEmpty :: Dist a -> Bool
distIsEmpty DEmpty = True
distIsEmpty _ = False

mkDist :: [(a,Double)] -> Dist a
mkDist xs = mkDist2 xs []

mkDist2 :: [(a,Double)] -> [(Double->a,Double)]  -> Dist a
mkDist2 xs ys = case tree3 $ tree2 $ (tree1 (check xs)) ++ (tree1' (check ys)) of 
  Nothing      -> DEmpty
  Just (t,sum) -> Dist t (sum,length xs) 
  where
  
  check :: [(a,Double)] -> [(a,Double)]
  check = map (\x@(_,v)->if isNaN v || isInfinite v 
                        then error "Fatal Error in mkDist : value is NaN or Infinite!" 
                        else x )

  tree1 :: [(a,Double)] -> [(DTree a,Double)]
  tree1 = map (\(a,v) -> (DLeaf (a,v) , v) ) 

  tree1' :: [(Double->a,Double)] -> [(DTree a,Double)]
  tree1' = map (\(f,v) -> (DLeaf2 (f,v) , v) ) 

  tree2 ::  [(DTree a,Double)] -> [(DTree a,Double)]
  tree2 []  = []
  tree2 [x] = [x]
  tree2 ((d1,v1):(d2,v2):rest)  
   = let sum = v1+v2 in (DNode (v1/sum) d1 d2 , sum ) : (tree2 rest)

  tree3 :: [(DTree a,Double)] -> Maybe (DTree a,Double)
  tree3 []  = Nothing 
  tree3 [x] = Just x
  tree3 xs  = tree3 $ tree2 xs 

interv :: (Double,Double) -> Double -> Double
interv (a,b) x = (b-a)*x + a

distGet' :: Dist a -> Double -> a
distGet' (Dist t _) = get t
  where 
  get :: DTree a -> Double -> a
  get (DLeaf  (a,_) ) _ = a
  get (DLeaf2 (f,_) ) x = f x
  get (DNode mark t1 t2) x 
    = if x < mark 
      then get t1 (  x       /  mark    )
      else get t2 ( (x-mark) / (1-mark) ) 

distPop :: Dist a -> Double -> Maybe ( a , Dist a )
distPop DEmpty _ = Nothing
distPop (Dist (DLeaf  (x,_)) _ ) _    = Just ( x      , DEmpty )
distPop (Dist (DLeaf2 (f,_)) _ ) find = Just ( f find , DEmpty )
distPop (Dist t (sum,size)) find 
  = let ((ret,val),part,t') = pop t find 
     in Just (ret, Dist t' (sum-val,size-1) )
  where
  pop :: DTree a -> Double -> ( (a,Double) , Double , DTree a)
  pop (DNode p t1 t2) find = if find < p 
    then case t1 of
      DLeaf  x       -> ( x            , p , t2 )
      DLeaf2 (f,val) -> ( (f find,val) , p , t2 )
      _       -> let ( ret , pp , t1' ) = pop t1 (find/p)
                     pp' = pp*p
                  in ( ret , pp' , DNode (norm (p-pp') (1-p)) t1' t2 )
    else case t2 of
      DLeaf  x       -> ( x            , 1-p , t1 )
      DLeaf2 (f,val) -> ( (f find,val) , 1-p , t1 )
      _       -> let ( ret , pp , t2' ) = pop t2 ((find-p)/(1-p))
                     pp' = pp*(1-p)
                  in ( ret , pp' , DNode (norm p (1-p-pp') ) t1 t2' ) 
    where
    norm a b = a/(a+b)
      
rand01 :: (RandomGen g) => g -> (Double,g)
rand01 = randomR (0.0,1.0)

randP :: (RandomGen g) => Double -> g -> (Bool,g)
randP p gen = let (r,gen') = rand01 gen
               in ( r < p , gen') 

distGet ::  (RandomGen g , MonadState g m ) => Dist a -> m a
distGet d = randLift $ flip distGet_ d

distGet_ :: (RandomGen g) => g -> Dist a -> ( a , g )
distGet_ gen dist = ( distGet' dist x , gen' ) 
  where (x,gen') = rand01 gen
                            
distPop' :: (RandomGen g) => g -> Dist a -> Maybe ( a , Dist a , g )
distPop' gen dist = do 
   ( ret , dist' ) <- distPop dist x
   return ( ret , dist' , gen' ) 
  where
  (x , gen' ) = rand01 gen

distObt' :: (RandomGen g) => CutP -> g -> Dist a -> Maybe ( a , Dist a , g )
distObt' _    _   DEmpty = Nothing 
distObt' cutP gen dist
  = let ( doCut , gen' ) = randP cutP gen
     in if doCut 
         then distPop' gen' dist 
         else let (ret,gen'') = distGet_ gen' dist in Just (ret,dist,gen'')

distTake_new :: (RandomGen g , MonadState g m ) => Int -> Dist a -> m [a]
distTake_new n dist = randLift (\gen -> distTake gen n dist )

distTake :: (RandomGen g) => g -> Int -> Dist a -> ( [a] , g )
distTake gen 0 _ = ( []   , gen   )
distTake gen n d = ( x:xs , gen'' )
  where
  (x , gen' ) = distGet_ gen         d
  (xs, gen'') = distTake gen' (n-1)  d

distCut :: (RandomGen g) => g -> Int -> Dist a -> ( [a] , Dist a , g )
distCut gen 0 d = ( []   , d  , gen  )
distCut gen n d = case distPop' gen d of 
   Nothing -> 
        ( []   , d  , gen  )
   Just ( x    , d' , gen' ) -> 
    let (   xs , d'', gen'') = distCut gen' (n-1) d'
     in ( x:xs , d'', gen'')

distMetaCut :: (RandomGen g) => ( g -> Dist a -> Maybe ( a , Dist a , g )) 
                             -> g -> Int -> Dist a -> ( [a] , Dist a , g )
distMetaCut cutF gen 0 d = ( []   , d  , gen  )
distMetaCut cutF gen n d = case cutF gen d of 
   Nothing -> 
        ( []   , d  , gen  )
   Just ( x    , d' , gen' ) -> 
    let (   xs , d'', gen'') = distMetaCut cutF gen' (n-1) d'
     in ( x:xs , d'', gen'')



distObtain :: (RandomGen g) => g -> (Percent,CutP) -> Dist a -> ( [a] , Dist a , g )
distObtain gen (percent,cutP) dist = distMetaCut (distObt' cutP) gen howMany dist
  where howMany = let part = round $ percent * (fromIntegral $ distSize dist)
                   in if part < 1 then 1 else part

distMax :: Dist a -> Maybe (a,Double)
distMax DEmpty           = Nothing
distMax (Dist t (suma,_)) = Just $ distMax' suma t
  where
  distMax' :: Double -> DTree a -> (a,Double)
  distMax' _ (DLeaf x     ) = x
  distMax' _ (DLeaf2 (f,v)) = (f 0.5,v)                              -- TODO domyslet líp, ne 0.5kou 
  distMax' part (DNode mark t1 t2) = if part1 > part2 then r1 else r2 
    where
    r1@(_ , part1) = distMax' (part*mark    ) t1  
    r2@(_ , part2) = distMax' (part*(1-mark)) t2   

distMin :: Dist a -> Maybe (a,Double)
distMin DEmpty           = Nothing
distMin (Dist t (suma,_)) = Just $ distMin' suma t
  where
  distMin' :: Double -> DTree a -> (a,Double)
  distMin' _ (DLeaf x     ) = x
  distMin' _ (DLeaf2 (f,v)) = (f 0.5,v)                              -- TODO domyslet líp, ne 0.5kou 
  distMin' part (DNode mark t1 t2) = if part1 < part2 then r1 else r2 
    where
    r1@(_ , part1) = distMin' (part*mark    ) t1  
    r2@(_ , part2) = distMin' (part*(1-mark)) t2   

distAvg :: Dist a -> Double
distAvg DEmpty = error "Avg of empty Dist!"
distAvg (Dist t (suma,size)) = suma / (fromIntegral size)


distSize :: Dist a -> Int
distSize (Dist _ (_,size)) = size
distSize DEmpty = 0

getDTree :: Dist a -> DTree a
getDTree (Dist t _) = t


prettyRound :: Double -> Double
prettyRound x = if abs (x-x') > 1e-14 
  then x else x'
  where x' = fromInteger $ round x

showDist :: (Show a) => Dist a -> String
showDist DEmpty       = "<EMPTY DIST>"
showDist (Dist t (sum,size)) = "\n" ++
   showDTree sum t ++
   "\n----------------------" ++ 
   --"\n [max]  "  ++ show a ++ " : " ++ (show $ prettyRound max) ++
   --                           " : " ++ showNice (max*100/sum) ++ "%" ++ 
   "\n [sum]  "  ++ (show sum) ++ 
   "\n [size] " ++ (show size) ++
   "\n"
  where 
  showNice = printf "%.2f"
  showDTree :: (Show a) => Double -> DTree a -> String  
  showDTree part (DLeaf x) = show x  -- ++ " : " ++ (show $ prettyRound part) 
                                    ++ " : " ++ showNice (part*100/sum) ++ "%" 
  showDTree part (DLeaf2 (f,v)) = "(<" ++ show (f 0.5) ++ ">," ++ show v ++ ")"
                                    ++ " : " ++ showNice (part*100/sum) ++ "%" 
  showDTree part (DNode mark t1 t2)
   = (showDTree (part*mark) t1) ++ "\n" ++ (showDTree (part*(1-mark)) t2)   


distToList :: Int -> Dist a -> [(a,Double)]
distToList samplingNum d = case d of
  DEmpty -> []
  Dist t _ -> dTreeToList t
 where
  --sampling = 32.0
  dTreeToList :: DTree a -> [(a,Double)]
  dTreeToList t = case t of
   DLeaf  x      -> [x]
   DLeaf2 x      -> sampling samplingNum x
   DNode _ t1 t2 -> dTreeToList t1 ++ dTreeToList t2   

sampling :: Int -> (Double->a,Double) -> [(a,Double)]
sampling num (f,val) = 
 let delta = 1.0 / fromIntegral num
     dVal  = val * delta
     xs    = [ 0.0 , delta .. 1.0 ]
  in [ ( f avg , dVal ) | (a,b) <- zip (init xs) (tail xs) , let avg = (a+b)/2 ]


-- testovací data
d1 = mkDist [('A',1),('B',2),('C',4),('D',8),('E',16)]
d2 = mkDist $ [ (i,(1.113)**i) | i<-[1..64] ]
d3 = mkDist2 [(0,1),(1,2),(2,4),(4,16)] [(interv (2,4),8),(interv (4,6),32)]

