{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}

module GP_Classes where

import Control.Monad
import Data.Maybe
import Data.Typeable

import Util
import Dist
import Heval

data Problem term a gOpt mOpt cOpt = Problem
 { popSize :: PopSize
 , eOpt    :: EOpt
 , gOpt    :: gOpt
 , mOpt    :: mOpt
 , cOpt    :: cOpt
 , fitFun  :: FitFun2 term a
 , ass     :: a
 }

type FitFun term = term -> Rand FitVal

data FitFun2 term a = FF1 (term -> Rand FitVal) a
                    | FF2 (term->String) a (a->Rand FitVal)

mkFF1 :: (term -> Rand FitVal) -> FitFun2 term ()
mkFF1 ff = FF1 ff () 

getFFType :: FitFun2 term a -> a
getFFType (FF1 _ a  ) = a
getFFType (FF2 _ a _) = a

type Credit = Double

type PopSize    = Int
type NumGens    = Int
type FitVal     = Double
type Prob       = Double



data GenOpType  = Reproduction | Mutation | Crossover
data GenOp term = MonoOp (term->Rand term) | DiOp (term->term->Rand(term,term))

type EOpt = Dist GenOpType
--data EOpt = EOpt { probRep :: Prob , probMut :: Prob , probCro :: Prob }

mkEOpt :: (Prob,Prob,Prob) -> EOpt
mkEOpt (probRep,probMut,probCro) = 
 mkDist [ ( Reproduction , probRep ) 
        , ( Mutation     , probMut )   
        , ( Crossover    , probCro ) ]


class Gene term opt where
  generateIt :: opt -> Rand [term]

class Muta term opt where
 mutateIt :: opt -> term -> Rand term 

class Cros term opt where
 crossIt :: opt -> term -> term -> Rand (term,term)  

class Evolvable term a gOpt mOpt cOpt where
  evolveIt  :: Problem term a gOpt mOpt cOpt -> Credit -> Rand [Dist term]
  
instance (Gene term gOpt, Muta term mOpt, Cros term cOpt,Typeable a) => Evolvable term a gOpt mOpt cOpt where
 evolveIt problem credit = 
  wrapper $ evolveBegin problem credit >>= infChainRand (evolveStep problem) 

wrapper :: Rand [(Dist term,a)] -> Rand [Dist term]
wrapper rand = do
 xs <- rand
 return . takeWhile (not . distIsEmpty) . map fst $ xs 

evolveBegin :: ( Gene term gOpt, Typeable a ) => Problem term a gOpt mOpt cOpt -> Credit -> Rand (Dist term,Credit)
evolveBegin p credit = do
 let toTake = min (popSize p) (floor credit) 
 pop0 <- (take toTake `liftM` generateIt (gOpt p)) >>= evalFF p
 return (pop0, credit - fromIntegral toTake )

evolveStep ::(Muta term mOpt,Cros term cOpt,Typeable a)=>Problem term a gOpt mOpt cOpt->(Dist term,Credit)->Rand (Dist term,Credit)
evolveStep p x@(_,credit) = do
 pop <- ( getWinners x >>= performOps p ) >>= evalFF p
 return (pop, credit - fromIntegral (distSize pop) )

evalFF :: (Typeable a) => Problem term a gOpt mOpt cOpt -> [term] -> Rand (Dist term)
evalFF p ts = case fitFun p of
 FF1 ff _ -> mkDist `liftM` mapM (\t->(t,) `liftM` ff t) ts
 FF2 toStr a ff -> 
  let strs = map toStr ts
      as   = evals strs a
   in mkDist `liftM` mapM (\(t,a)->(t,) `liftM` ff a) (zip ts as)


getWinners :: (Dist term,Credit) -> Rand ( [term] , [term] )
getWinners (pop,credit) = 
  let Just (best,_) = distMax pop
      bests  = if credit >= 1 then [best] else []
      toTake = (min (distSize pop) (floor credit) ) - length bests 
   in ( bests , ) `liftM` distTake_new toTake pop 

performOps :: (Muta term mOpt , Cros term cOpt ) => Problem term a gOpt mOpt cOpt -> ([term],[term]) -> Rand [term]
performOps p (best,terms) = (best ++ ) `liftM` performOps' (mkOpDist p) terms  
 where
  mkOpDist p = fmap (f p) (eOpt p)
  f p opType = case opType of
   Reproduction -> MonoOp return
   Mutation     -> MonoOp (mutateIt $ mOpt p)
   Crossover    -> DiOp   (crossIt  $ cOpt p)
  
  performOps' :: Dist (GenOp term) -> [term] -> Rand [term]
  performOps' _ [] = return [] 
  performOps' opDist terms@(t:ts) = do
    op <- distGet opDist
    case op of
     MonoOp f -> do 
      t'  <- f t
      ts' <- performOps' opDist ts
      return $ t' : ts'
     DiOp f -> case terms of
      [t] -> return [t]
      (t1:t2:tt) -> do
       (t1',t2') <- f t1 t2
       tt'       <- performOps' opDist tt
       return $ t1' : t2' : tt'


-- printing and testing ---------------------------------------------------------------------------

putEvolve :: (Show term , Evolvable term a gOpt mOpt cOpt) => Credit -> Problem term a gOpt mOpt cOpt -> IO()
putEvolve credit problem =
 liftM (map $ (\(i,d)->(i,fromJust . distMax $ d) ) ) (runRand $ (zip [0..]) `liftM` evolveIt problem credit ) >>= putList

putEvolveMaximas :: (Show term , Evolvable term a gOpt mOpt cOpt) => Credit -> Problem term a gOpt mOpt cOpt -> IO ()
putEvolveMaximas credit problem = do
 ds <- runRand $ zip [0..] `liftM` evolveIt problem credit
 let f (i,t) = ( i , fromJust.distMax$t)
     bs = map f ds
     g = snd . snd 
     lt x y = (g x) < (g y)
     ms = maximasBy lt bs
 putList ms


data TermLog t = LogGenom t | LogLine1 | LogLine2 | LogLine3 

instance Show t => Show (TermLog t) where
 show l = case l of
  LogGenom t -> show t
  LogLine3 -> replicate 80 '#'
  LogLine2 -> replicate 80 '='
  LogLine1 -> replicate 80 '-'

testGene :: (Show t, Gene t o) => t -> o -> IO [t]
testGene _ o = do
 ts <- runRand $ generateIt o
 putList ts
 return ts 

testMuta :: (Show t, Gene t oGen, Muta t oMut) => t -> oGen -> oMut -> IO [TermLog t]
testMuta _ oGen oMut = do
 ts <- runRand $ generateIt oGen
 ms <- runRand $ forM ts (\t-> mutateIt oMut t)  
 let ls = concatMap (\(t,m) -> [LogGenom t , LogLine1 , LogGenom m , LogLine2] ) ( zip ts ms )
 putList ls
 return ls

testCros :: (Show t, Gene t oGen, Cros t oCro) => t -> oGen -> oCro -> IO [TermLog t]
testCros _ oGen oCro = do
 ts <- runRand $ generateIt oGen
 let ps = pairs ts
 cs <- runRand $ forM ps (\(t1,t2)-> crossIt oCro t1 t2)  
 let ls = concatMap (\((t1,t2),(t1',t2')) -> 
                       [LogGenom t1  , LogLine1 ,
                        LogGenom t2  , LogLine2 , 
                        LogGenom t1' , LogLine1 , 
                        LogGenom t2' , LogLine3] ) ( zip ps cs )
 putList ls
 return ls



