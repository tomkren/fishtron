module GP 
( gp
, FitVal
) where 

import Data.List
import Data.Maybe
import System.Random
import Control.Monad.State
import qualified Data.Map as Map

import Base
import Dist
import Prover
import Evaluation

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


-- GP core ---------------------------------------------------------------------

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

