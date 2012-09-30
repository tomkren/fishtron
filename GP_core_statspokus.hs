{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}

module GP_Core 
( Evolvable, evolveIt 
, Gene,      generateIt 
, Muta,      mutateIt 
, Cros,      crossIt  

, Problem(Problem), FitFun(FF2)
, Prob, FitVal, GenOpProbs, PopSize, NumGene
, mkGenOps, mkFF1

, run, runWith
) where

import Control.Monad ( liftM )
import Data.Typeable ( Typeable )
import Data.Maybe    ( fromJust )

import Util  ( Ral, chainM, runRal, runRalWith, maximasBy, putList, logIt )
import Dist  ( Dist, mkDist, distGet, distMax, distSize, distTake_new )
import Heval ( evals )

type PopSize = Int
type NumGene = Int
type FitVal  = Double
type Prob    = Double

data Problem term a gOpt mOpt cOpt = Problem
 { popSize :: PopSize
 , numGene :: NumGene
 , genOps  :: Dist (GenOp term)
 , gOpt    :: gOpt
 , mOpt    :: mOpt
 , cOpt    :: cOpt
 , fitFun  :: FitFun term a
 }

data FitFun term a = 
  FF1 (term -> Ral FitVal) a | 
  FF2 (term->String) a (a->Ral FitVal)

type GenOpProbs = (Prob,Prob,Prob)
data GenOpType = Reproduction | Mutation | Crossover
data GenOp term = 
  MonoOp (term->Ral term) | 
  DiOp   (term->term->Ral (term,term))

data Stats term = Stats
 { bestTerm :: Maybe (term,FitVal)  
 } deriving (Show)

st_setBest :: Stats term -> (term,FitVal) -> Stats term
st_setBest st best = Stats (Just best)

class Gene term opt where
  generateIt :: Int -> opt -> Ral [term]

class Muta term opt where
 mutateIt :: opt -> term -> Ral term 

class Cros term opt where
 crossIt :: opt -> term -> term -> Ral (term,term)  

class Evolvable term a gOpt mOpt cOpt where
  evolveIt :: Problem term a gOpt mOpt cOpt -> Ral (Stats term)


instance (Gene term gOpt, Muta term mOpt, Cros term cOpt,Typeable a,Show term) => Evolvable term a gOpt mOpt cOpt where
 evolveIt p = do
   (pop0,st)     <- evolveBegin (Stats Nothing) p 
   (lastPop,st') <- chain numGens (evolveStep p) pop0 st
   --return . Stats . fromJust. distMax $ lastPop
   return st'
  where 
   numGens = numGene p
   chain :: Int -> (s -> a -> Ral (a,s) ) -> a -> s -> Ral (a,s)
   chain 0 _ x st = return (x,st)
   chain n f x st = do 
    logIt . headline $ "Genration " ++ show (numGens - n + 1)  
    (x',st') <- f st x 
    chain (n-1) f x' st'
 
headline :: String -> String
headline str = "-- " ++ str ++ " "++ replicate (76-length str) '-'

evolveBegin :: ( Gene term gOpt, Typeable a,Show term) => 
  Stats term -> Problem term a gOpt mOpt cOpt -> Ral (Dist term,Stats term)
evolveBegin st p = do
 logIt . headline $ "Genration 0"  
 let n = popSize p 
 terms <- generateIt n (gOpt p)
 pop0 <- evalFF (fitFun p) terms
 b <- logBest pop0
 return (pop0,st_setBest st b )

evolveStep ::(Muta term mOpt,Cros term cOpt,Typeable a,Show term)=> 
  Problem term a gOpt mOpt cOpt-> Stats term -> Dist term ->Ral (Dist term,Stats term)
evolveStep p st pop = do
 (tsNoOps,tsForOps,st') <- getWinners st pop
 tsAfterOps <- performOps (genOps p) tsForOps 
 pop' <- evalFF (fitFun p) ( tsNoOps ++ tsAfterOps )
 return (pop',st')

logBest :: (Show term) => Dist term -> Ral (term,FitVal)
logBest pop = do 
 let Just (best,ffVal) = distMax pop  
 logIt $ "\nBest: " ++ show ffVal ++ "\n" ++ show best ++ "\n"
 return (best,ffVal)

getWinners :: (Show term) => Stats term -> Dist term -> Ral ( [term] , [term] , Stats term )
getWinners st pop = do
  b@(best,_) <- logBest pop
  let toTake = distSize pop - 1 
  ( [best] , , st_setBest st b ) `liftM` distTake_new toTake pop 

evalFF :: (Typeable a,Show term) => FitFun term a -> [term] -> Ral (Dist term)
evalFF ff ts = case ff of
 FF1 ff _ -> mkDist `liftM` mapM (\t->(t,) `liftM` ff t) ts
 FF2 toStr a ff -> 
  let strs = map toStr ts
      xs   = evals strs a
   in mkDist `liftM` mapM (\(t,a)->(t,) `liftM` (ff a >>= checkNaN) ) (zip ts xs)

checkNaN :: FitVal -> Ral FitVal
checkNaN x = 
 if isNaN x 
 then do
  logIt "Warning : Fitness Value is NaN ; changed to 0."
  return 0 
 else return x

performOps :: Dist (GenOp term) -> [term] -> Ral [term]
performOps _ [] = return [] 
performOps opDist terms@(t:ts) = do
  op <- distGet opDist
  case op of
   MonoOp f -> do 
    t'  <- f t
    ts' <- performOps opDist ts
    return $ t' : ts'
   DiOp f -> case terms of
    [t] -> return [t]
    (t1:t2:tt) -> do
     (t1',t2') <- f t1 t2
     tt'       <- performOps opDist tt
     return $ t1' : t2' : tt'

-- Creating problem structure --------------------------------------------------

mkGenOps :: (Muta term mOpt , Cros term cOpt ) => (mOpt,cOpt) -> GenOpProbs -> Dist (GenOp term)
mkGenOps opt (probRep,probMut,probCro) = fmap (mkGenOp opt) d
 where d = mkDist [ ( Reproduction , probRep ) 
                  , ( Mutation     , probMut )   
                  , ( Crossover    , probCro ) ]
 
mkGenOp :: (Muta term mOpt , Cros term cOpt ) => (mOpt,cOpt) -> GenOpType -> GenOp term
mkGenOp (mo,co) opType = case opType of
 Reproduction -> MonoOp return
 Mutation     -> MonoOp (mutateIt mo)
 Crossover    -> DiOp   (crossIt  co)

mkFF1 :: (term -> Ral FitVal) -> FitFun term ()
mkFF1 ff = FF1 ff () 

-- printing & testing -----------------------------------------------------------

run :: (Show term , Evolvable term a gOpt mOpt cOpt) => Problem term a gOpt mOpt cOpt -> IO ()
run problem = do 
  ret <- runRal [] $ evolveIt problem
  putStrLn . show $ ret

runWith :: (Show term , Evolvable term a gOpt mOpt cOpt) => String -> Problem term a gOpt mOpt cOpt -> IO ()
runWith seedStr problem = do 
  ret <- runRalWith seedStr [] $ evolveIt problem
  putStrLn . show $ ret


--putEvolveMaxs :: (Show term , Evolvable term a gOpt mOpt cOpt) => Problem term a gOpt mOpt cOpt -> IO ()
--putEvolveMaxs problem = do
-- ds <- runRal [] $ zip [0..] `liftM` evolveIt problem
-- let f (i,pop) = ( i , fromJust . distMax $ pop )
--     bs = map f ds
--     g = snd . snd 
--     lt x y = (g x) < (g y)
--     ms = maximasBy lt bs
-- putList ms
--