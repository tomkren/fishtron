module GP_old
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

import Debug.Trace

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

type GPState g = ( g , Dist TTerm , Map.Map Typ [TTerm] , g -> Typ -> ([TTerm],g) )

-- GP core ---------------------------------------------------------------------

gp :: (RandomGen g) => g -> (g -> Typ -> ([TTerm],g)) -> Typ -> (TTerm->FitVal) -> Int -> Int -> [Dist TTerm]
gp gen dkFun typ ff popSize numGen = evalState (gpLoop numGen) gpState0
  where
  gpState0 = let (ts,gen') = dkFun gen typ
              in mkGPState gen' (mkDist $ mapFitness ff $ take popSize $ ts) dkFun     

  gpLoop :: (RandomGen g) => Int -> State (GPState g) [Dist TTerm]
  gpLoop i = do 
    pop <- getPopulation
    if i > 0
    then do 
      logg $ "generace #" ++ show (numGen - i +1) ++" ---------------------------"
      gpStep ff
      pops <- gpLoop (i-1)
      return (pop:pops)
    else do 
      return [pop]

mkGPState :: (RandomGen g) => g -> Dist TTerm -> (g -> Typ -> ([TTerm],g)) -> GPState g
mkGPState gen pop dkFun = ( gen , pop , Map.empty , dkFun )

gpStep :: (RandomGen g) => (TTerm->FitVal) -> State (GPState g) ()
gpStep ff =  do
  best     <- getBest
  popSize  <- populationSize
  parents  <- getWinners (popSize - 1)
  children <- xovers parents
  updatePopulation ff $ best:children

updatePopulation :: (TTerm->FitVal) -> [TTerm] -> State (GPState g) ()
updatePopulation ff ts = state $ \ ( gen , _ , db , dkFun ) -> 
  ( () , ( gen , mkDist $ mapFitness ff ts , db , dkFun ) )

populationSize :: State (GPState g) Int
populationSize = state $ \ s@( _ , pop , _ , _ ) -> ( distSize pop , s)

getPopulation :: State (GPState g) (Dist TTerm)
getPopulation = state $ \ s@( _ , pop , _ , _ ) -> (pop , s)

getWinners :: (RandomGen g) => Int -> State (GPState g) [TTerm]
getWinners howMany = state $ \ ( gen , pop , db , dkFun ) -> 
  let ( ws , gen' ) = distTake gen howMany pop in ( ws ,( gen' , pop , db , dkFun ) )

getBest :: State (GPState g) TTerm
getBest = state $ \ s@( _ , pop , _ , _ ) ->  ( fst $ fromJust $ distMax pop , s) --( fst $ distMax pop , s) 


logg :: String -> State (GPState g) ()
logg str = trace str (state $ \ s -> ((),s) )

getMutant :: (RandomGen g) => Typ -> State (GPState g) TTerm
getMutant typ = state $ \ ( gen , pop , db , dkFun ) -> 
  case Map.lookup typ db of
    Nothing     -> let ( (x:xs) , gen' ) = dkFun gen typ in 
                   ( x , ( gen' , pop , Map.insert typ xs db , dkFun ) )
    Just (x:xs) -> ( x , ( gen  , pop , Map.insert typ xs db , dkFun ) )

getRandomR :: (Random a,RandomGen g) => (a,a) -> State (GPState g) a
getRandomR range = state $ \ ( gen , pop , db , dkFun ) -> 
  let (ret,gen') = randomR range gen in (ret,(gen',pop,db, dkFun ))

mapFitness :: (TTerm->FitVal) -> [TTerm] -> [(TTerm,FitVal)]
mapFitness fitness = map (\t-> (t,fitness t) )

-- crossover -------------------------------------------------------------------

xovers :: (RandomGen g) => [TTerm] -> State (GPState g) [TTerm]
xovers []         = return []
xovers [t]        = return [t]
xovers (t1:t2:ts) = do 
  (t1',t2') <- xover t1 t2
  ts'       <- xovers ts
  return $ t1':t2':ts'

xover :: (RandomGen g) => TTerm -> TTerm -> State (GPState g) ( TTerm , TTerm )
xover t1 t2 = 
  let candidates = smartCompatibleSubterms t1 t2  --  <---------------- tady se voli smart/normal
      canSize    = length candidates
   in if null candidates 
      then do return $ (t1,t2) 
      else do
        i <- getRandomR (0, canSize - 1 )
        let (z1,z2) = candidates !! i in crossover z1 z2

crossover :: (RandomGen g) => TTermZipper -> TTermZipper -> State (GPState g) ( TTerm , TTerm )
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

treatFVs :: (RandomGen g) => TTermZipper -> State (GPState g) TTerm
treatFVs (t,ds) = f (fv' t) t
  where
  boundVarsInDs = foldl (\acc x -> case x of 
    Lamb sym (typ:->_) -> if has acc sym
                          then           acc 
                          else (sym,typ):acc 
    _                  ->                acc ) [] ds
  has :: [(Symbol,Typ)] -> Symbol -> Bool
  has xs s = any (\(s',_)->s==s') xs
  f :: (RandomGen g) => [(Symbol,Typ)] -> TTerm  -> State (GPState g) TTerm
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


