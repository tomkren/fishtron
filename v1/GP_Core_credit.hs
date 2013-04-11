{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}

module GP_Core_credit () where

import Control.Monad ( liftM )
import Data.Typeable ( Typeable )

import Util (Ralog)
import Dist (Dist,mkDist,distGet,distMax,distSize,distTake_new)
import Heval ( evals )



type Credit  = Double
type PopSize = Int
type FitVal  = Double


data Problem term a gOpt mOpt cOpt = Problem
 { popSize :: PopSize
 , eOpt    :: EOpt
 , gOpt    :: gOpt
 , mOpt    :: mOpt
 , cOpt    :: cOpt
 , fitFun  :: FitFun term a
 }

data FitFun term a = 
  FF1 (term -> Ralog FitVal) a | 
  FF2 (term->String) a (a->Ralog FitVal)


type EOpt = Dist GenOpType

data GenOpType = Reproduction | Mutation | Crossover
data GenOp term = 
  MonoOp (term->Ralog term) | 
  DiOp   (term->term->Ralog (term,term))



class Gene term opt where
  generateIt :: Int -> opt -> Ralog [term]

class Muta term opt where
 mutateIt :: opt -> term -> Ralog term 

class Cros term opt where
 crossIt :: opt -> term -> term -> Ralog (term,term)  

class Evolvable term a gOpt mOpt cOpt where
  evolveIt :: Problem term a gOpt mOpt cOpt -> Credit -> Ralog [Dist term]



instance (Gene term gOpt, Muta term mOpt, Cros term cOpt,Typeable a) => Evolvable term a gOpt mOpt cOpt where
 evolveIt problem credit = do
 x <- evolveBegin problem credit  
 evolveSteps problem x

evolveBegin :: ( Gene term gOpt, Typeable a ) => Problem term a gOpt mOpt cOpt -> Credit -> Ralog (Dist term,Credit)
evolveBegin p credit = do
 let n = min (popSize p) (floor credit) 
 terms <- generateIt n (gOpt p)
 pop0 <- evalFF p terms
 return (pop0, credit - fromIntegral n )

evolveSteps :: (Muta term mOpt,Cros term cOpt,Typeable a)=> Problem term a gOpt mOpt cOpt -> (Dist term,Credit) -> Ralog [Dist term]
evolveSteps p x@(_,credit) = 
 if credit <= 0 
  then return []
  else do
   x'@(pop,_) <- evolveStep p x
   rest <- evolveSteps p x'
   return (pop:rest) 

evolveStep ::(Muta term mOpt,Cros term cOpt,Typeable a)=>Problem term a gOpt mOpt cOpt->(Dist term,Credit)->Ralog (Dist term,Credit)
evolveStep p x@(_,credit) = do
 (tsNoOps,tsForOps) <- getWinners x
 tsAfterOps <- performOps (mkGenOpDist p) tsForOps
 pop <- evalFF p ( tsNoOps ++ tsAfterOps )
 return (pop, credit - fromIntegral (distSize pop) )

getWinners :: (Dist term,Credit) -> Ralog ( [term] , [term] )
getWinners (pop,credit) = 
  let Just (best,_) = distMax pop
      bests  = if credit >= 1 then [best] else []
      toTake = (min (distSize pop) (floor credit) ) - length bests 
   in ( bests , ) `liftM` distTake_new toTake pop 

evalFF :: (Typeable a) => Problem term a gOpt mOpt cOpt -> [term] -> Ralog (Dist term)
evalFF p ts = case fitFun p of
 FF1 ff _ -> mkDist `liftM` mapM (\t->(t,) `liftM` ff t) ts
 FF2 toStr a ff -> 
  let strs = map toStr ts
      xs   = evals strs a
   in mkDist `liftM` mapM (\(t,a)->(t,) `liftM` ff a) (zip ts xs)

mkGenOpDist :: (Muta term mOpt , Cros term cOpt ) => Problem term a gOpt mOpt cOpt -> Dist (GenOp term)
mkGenOpDist p = fmap (mkGenOp mo co) eo
 where eo = eOpt p
       mo = mOpt p
       co = cOpt p

mkGenOp :: (Muta term mOpt , Cros term cOpt ) => mOpt -> cOpt -> GenOpType -> GenOp term
mkGenOp mo co opType = case opType of
 Reproduction -> MonoOp return
 Mutation     -> MonoOp (mutateIt mo)
 Crossover    -> DiOp   (crossIt  co)

performOps :: Dist (GenOp term) -> [term] -> Ralog [term]
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



