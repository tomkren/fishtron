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

import Util  ( Ral, chainM, runRal, runRalWith, maximasBy, putList, logIt, boxIt, statIt , StatRecord(..) )
import Dist  ( Dist, mkDist, distGet, distMax,distMin,distAvg, distSize, distTake_new )
import Heval ( evals )

import Text.Printf

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


class Gene term opt where
  generateIt :: Int -> opt -> Ral [term]

class Muta term opt where
 mutateIt :: opt -> term -> Ral term 

class Cros term opt where
 crossIt :: opt -> term -> term -> Ral (term,term)  

class Evolvable term a gOpt mOpt cOpt where
  evolveIt :: Problem term a gOpt mOpt cOpt -> Ral (term,FitVal)


instance (Gene term gOpt, Muta term mOpt, Cros term cOpt,Typeable a,Show term) => Evolvable term a gOpt mOpt cOpt where
 evolveIt p = do
   pop0 <- evolveBegin p  
   lastPop <- chain numGens (evolveStep p) pop0
   return . fromJust. distMax $ lastPop 
  where 
   numGens = numGene p
   chain :: Int -> (Int -> a -> Ral a) -> a -> Ral a
   chain 0 _ x = return x
   chain n f x = f (numGens - n + 1) x >>= chain (n-1) f
 
evolveBegin :: ( Gene term gOpt, Typeable a,Show term) => Problem term a gOpt mOpt cOpt -> Ral (Dist term)
evolveBegin p = do
 logGeneration 0  
 let n = popSize p 
 terms <- generateIt n (gOpt p)
 pop0 <- evalFF (fitFun p) terms
 logBest 0 pop0
 return pop0

evolveStep ::(Muta term mOpt,Cros term cOpt,Typeable a,Show term)=> Problem term a gOpt mOpt cOpt -> Int -> Dist term -> Ral (Dist term)
evolveStep p i pop = do
 logGeneration i 
 best   <- logBest i pop 
 terms  <- distTake_new (popSize p - 1) pop   
 terms' <- performOps (genOps p) terms 
 evalFF (fitFun p) ( best : terms' )


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
  boxIt ">>> Warning : Fitness Value is NaN ; changed to 0 <<<"
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


logGeneration :: Int -> Ral ()
logGeneration i = do
 let sh = printf "%*d" (12::Int)
 logIt  $ " ┌────────────────────────┐"
 logIt  $ " │ Genration "++ sh i ++" │"  

logBest :: (Show term) => Int -> Dist term -> Ral term
logBest i pop = do  
 let Just (best,b) = distMax pop
 let            a  = distAvg pop
 let Just (_   ,w) = distMin pop 
 let sh = printf "%*.5f" (14::Int)
 logIt  $ " ├────────────────────────┤"
 logIt  $ " │ Best    " ++ sh b ++ " │"
 logIt  $ " │ Average " ++ sh a ++ " │"
 logIt  $ " │ Worst   " ++ sh w ++ " │"
 logIt  $ " └────────────────────────┘" 
 boxIt  $ show best 
 statIt $ SR_Best  i b
 statIt $ SR_Avg   i a 
 statIt $ SR_Worst i w 
 return best


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