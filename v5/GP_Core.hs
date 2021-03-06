{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TupleSections #-}

module GP_Core 
( Gene(..)
, Muta(..)
, Cros(..)
, Prob
, FitFun(..)
, Problem(..)
, NumGene
, PopSize
, GenOpProbs
, mkGenOps
, FitVal
, mkFF1
, nRunsByServer
) where

import Control.Monad ( liftM )
import Data.Typeable ( Typeable )
import Text.Printf   ( printf )
import Data.Maybe    ( fromJust,isJust )


import Eva   ( Eva, runEva, evalsWith, evals, setOutputBuffer, sendJSON, flushStdout, 
               amIStillAlive, resetAfterStop,
               eva_getPopInfo,eva_resetPopInfo,
               eva_popi_setBest,eva_inc_rep,eva_inc_elite,
               eva_popi_setPop, eva_nextRC)
import Dist  ( Dist , mkDist, distTake_new, distGet, distMax, distMin, distAvg )
import Dist  ( distToList )
import Utils ( logIt, boxIt , JShow, jshow, putList, jss_size)
import ServerInterface ( graphCmd, multiCmd, OutputBuffer  ) 
import PopulationInfo  ( popInfoToJSON )

class Gene term opt where
  generateIt :: Int -> opt -> Eva [term]

class Muta term opt where
 mutateIt :: opt -> term -> Eva term 

class Cros term opt where
 crossIt :: opt -> term -> term -> Eva (term,term) 

class Evolvable term a gOpt mOpt cOpt where
  evolveIt :: RunInfo -> Problem term a gOpt mOpt cOpt -> Eva (term,FitVal,Maybe Int)


data Problem term a gOpt mOpt cOpt = Problem
 { problemName :: String
 , popSize     :: PopSize
 , numGene     :: NumGene
 , genOps      :: Dist (GenOp term)
 , gOpt        :: gOpt
 , mOpt        :: mOpt
 , cOpt        :: cOpt
 , fitFun      :: FitFun term a
 , saveBest    :: Bool
 }


type PopSize = Int
type NumGene = Int
type FitVal  = Double
type Prob    = Double
type RunInfo = (Int,Int)

data FitFun term a = 
  FF1 (term -> Eva FitVal) a | 
  FF2 (term->String) a (a->Eva FitVal) |
  FF3 (term->String) a (a->Eva (FitVal,Bool) ) |
  FF4 (term->String) String a | -- poslední a je fake jako u FF1, String je jmeno ff
  FF5 String String a | -- jemeno FF , jmeno modulu , fake (showStr předpokladá show)
  FF6 a (a->(FitVal,Bool)) String -- asType , ff , jmeno modulu 

type GenOpProbs = (Prob,Prob,Prob)
data GenOpType = Reproduction | Mutation | Crossover
data GenOp term = 
  MonoOp (term->Eva term) | 
  DiOp   (term->term->Eva (term,term))


instance (Gene term gOpt, Muta term mOpt, Cros term cOpt,Typeable a,JShow term) => Evolvable term a gOpt mOpt cOpt where
 evolveIt runInfo p = do
   (pop0,mWin) <- evolveBegin runInfo p
   case mWin of
    Nothing -> chain numGens (evolveStep runInfo p) (fromJust . distMax) pop0
    Just (te,ge) -> return ( te,ge, Just 0 ) 
  where 
   numGens = numGene p
   chain :: Int -> (Int -> a -> Eva (a, Maybe (b,c) ) ) -> (a->(b,c)) -> a -> Eva (b,c,Maybe Int)
   chain 0 _ r x = let (b,c) = r x in return ( b ,c , Nothing )
   chain n f r x = do 
    (a,mbc) <- f (numGens - n + 1) x 
    isAlive <- amIStillAlive 
    if isAlive 
      then case mbc of
            Nothing  -> chain (n-1) f r a 
            Just (b,c) -> return (b,c,Just (numGens-n+1) ) 
      else do
       logIt "Umrel!"   
       let (b,c) = r x in return ( b ,c , Nothing ) 

evolveBegin :: ( Gene t go, Typeable a,JShow t) => RunInfo -> Problem t a go mo co -> Eva (Dist t , Maybe (t,FitVal) )
evolveBegin runInfo p = do
 let n = popSize p 
 terms <- generateIt n (gOpt p)
 ret@(pop0,mWin) <- evalFF (fitFun p) terms
 logGeneration runInfo 0 (isJust mWin) pop0
 return ret

evolveStep ::(Muta t mo,Cros t co,Typeable a,JShow t)=> RunInfo->Problem t a go mo co -> Int -> Dist t -> Eva(Dist t,Maybe(t,FitVal))
evolveStep runInfo p i pop = do
 let best     =  getBest pop 
 terms        <- distTake_new (popSize p - (if saveBest p then 1 else 0) ) pop   
 terms'       <- performOps (genOps p) terms 
 ret@(pop',mWin) <- evalFF (fitFun p) (if saveBest p then ( best : terms' ) else terms' )
 logGeneration runInfo i (isJust mWin) pop'
 (if saveBest p then eva_inc_elite else return ())

 return ret



evalFF :: (Typeable a,Show t) => FitFun t a -> [t] -> Eva (Dist t , Maybe (t,FitVal) )
evalFF ff ts = case ff of
 FF1 ff _ -> ((,Nothing) . mkDist) `liftM` mapM (\t->(t,) `liftM` ff t) ts
 FF2 toStr a ff -> do 
  let strs = map toStr ts
  xs <- evals strs a
  ((,Nothing) . mkDist) `liftM` mapM (\(t,a)->(t,) `liftM` (ff a >>= checkNaN) ) (zip ts xs)
 FF3 toStr a ff -> do
  let strs = map toStr ts
  xs <- evals strs a
  resultFF3 `liftM` mapM (\(t,a)->(t,) `liftM` (ff a >>= checkNaN_FF3) ) (zip ts xs)
 FF4 toStr ffName _ -> do
  let strs = map (\t-> ffName ++ " $ " ++ (toStr t) ) ts
  xs  <- evals strs (1::FitVal,True::Bool) 
  xs' <- mapM checkNaN_FF3 xs
  return $ resultFF3 (zip ts xs')
 FF5 ffName modul _ -> do
  let strs = map (\t-> ffName ++ " $ " ++ (show t) ) ts
  xs  <- evalsWith modul strs (1::FitVal,True::Bool) 
  xs' <- mapM checkNaN_FF3 xs
  return $ resultFF3 (zip ts xs')
 FF6 as ff modul -> do
  let strs = map show ts
  xs <- evalsWith modul strs as
  let results = map ff xs
  results' <- mapM checkNaN_FF3 results
  return $ resultFF3 (zip ts results')


resultFF3 :: [(t,(FitVal,Bool))] -> ( Dist t , Maybe (t,FitVal) )   
resultFF3 xs = 
  let ( distList , mWinner ) = foldr f ([],Nothing) xs
   in ( mkDist distList , mWinner )
 where
  f :: (t,(FitVal,Bool)) -> ( [(t,FitVal)] , Maybe (t,FitVal) ) -> ( [(t,FitVal)] , Maybe (t,FitVal) )
  f (term,(fv,isWinner)) ( dList , mWin ) = 
   let dElem = (term,fv)
    in ( dElem:dList , if isWinner then Just dElem else mWin  )

checkNaN_FF3 :: (FitVal,Bool) -> Eva (FitVal,Bool)
checkNaN_FF3 (x,b) = 
 if isNaN x 
 then do
  boxIt ">>> Warning : Fitness Value is NaN ; changed to 0 <<<"
  return (0,b) 
 else return (x,b)

checkNaN :: FitVal -> Eva FitVal
checkNaN x = 
 if isNaN x 
 then do
  boxIt ">>> Warning : Fitness Value is NaN ; changed to 0 <<<"
  return 0 
 else return x

performOps :: Dist (GenOp term) -> [term] -> Eva [term]
performOps _ [] = return [] 
performOps opDist terms@(t:ts) = do
  op <- distGet opDist
  case op of
   MonoOp f -> do 
    t'  <- f t
    ts' <- performOps opDist ts
    return $ t' : ts'
   DiOp f -> case terms of
    [t] -> do eva_inc_rep; return [t]
    (t1:t2:tt) -> do
     (t1',t2') <- f t1 t2
     tt'       <- performOps opDist tt
     return $ t1' : t2' : tt'



getBest :: Dist term -> term 
getBest pop = best 
 where Just (best,_) = distMax pop 

logGeneration :: (JShow term) => RunInfo -> Int -> Bool -> Dist term -> Eva ()
logGeneration (actRun,allRuns) i isWinner pop = do  
 let Just (best,b) = distMax pop
     a             = distAvg pop
     Just (_   ,w) = distMin pop 
     p1 = printf "%*d" (12::Int)
     p2 = printf "%*.5f" (14::Int)
     p0 = printf "%*s" (18::Int)
     runStr = show actRun ++ "/" ++ show allRuns
 logIt  $ " ┌────────────────────────┐"
 if (allRuns<2) then return () else 
  logIt $ " │ Run " ++ p0 runStr ++" │"
 logIt  $ " │ Genration "++ p1 i ++" │"
 logIt  $ " ├────────────────────────┤"
 logIt  $ " │ Best    " ++ p2 b ++ " │"
 logIt  $ " │ Average " ++ p2 a ++ " │"
 logIt  $ " │ Worst   " ++ p2 w ++ " │"
 logIt  $ " └────────────────────────┘" 
 if isWinner then logIt "WINNER!!!!!!!" else return ()
 let popiList = distToList 32 pop
 let m_averageTermSize = if True 
                          then let popList = map fst $ popiList
                                   sizeOf  = fromIntegral . fromJust . jss_size
                                   len     = fromIntegral $ length popList
                                   isOK    = isJust . jss_size . head $ popList
                                in if isOK then Just (  ( sum $ map sizeOf popList  ) / len ) else Nothing 
                          else Nothing 
 boxIt   $ show best 
 stdout  <- flushStdout
 eva_popi_setBest best b
 eva_popi_setPop popiList
 popInfo <- eva_getPopInfo
 eva_resetPopInfo
 sendJSON $ multiCmd [ 
  graphCmd actRun i (b,a,w) isWinner m_averageTermSize , 
  stdout , 
  jshow best,
  popInfoToJSON popInfo ] -- new i V5!






-- Creating problem structure --------------------------------------------------

mkGenOps :: (Muta term mOpt , Cros term cOpt ) => (mOpt,cOpt) -> GenOpProbs -> Dist (GenOp term)
mkGenOps opt (probRep,probMut,probCro) = fmap (mkGenOp opt) d
 where d = mkDist [ ( Reproduction , probRep ) 
                  , ( Mutation     , probMut )   
                  , ( Crossover    , probCro ) ]
 
mkGenOp :: (Muta term mOpt , Cros term cOpt ) => (mOpt,cOpt) -> GenOpType -> GenOp term
mkGenOp (mo,co) opType = case opType of
 Reproduction -> MonoOp (\term ->do eva_inc_rep; return term)
 Mutation     -> MonoOp (mutateIt mo)
 Crossover    -> DiOp   (crossIt  co)

mkFF1 :: (term -> Eva FitVal) -> FitFun term ()
mkFF1 ff = FF1 ff () 

-- Running --------------------------------------------------------------------

nRunsByServer :: (Show term , Evolvable term a gOpt mOpt cOpt) => 
                     OutputBuffer -> Int -> Problem term a gOpt mOpt cOpt -> IO ()
nRunsByServer buff numRuns problem = do
  ret <- runEva $ setOutputBuffer buff >> multipleRuns numRuns problem
  putList ret

multipleRuns :: Evolvable t a go mo co => Int -> Problem t a go mo co -> Eva [(t,FitVal,Maybe Int)]
multipleRuns numRuns problem = multipleRuns' numRuns problem
 where
  multipleRuns' :: Evolvable t a go mo co => Int -> Problem t a go mo co -> Eva [(t,FitVal,Maybe Int)]
  multipleRuns' 0 _ = return [] 
  multipleRuns' n p = do
     x  <- evolveIt ( numRuns-n+1 , numRuns ) p
     isAlive <- amIStillAlive
     if isAlive then do
      logIt "BULL"
      xs <- multipleRuns' (n-1) p
      return (x:xs)
     else do
      resetAfterStop
      return [x]
