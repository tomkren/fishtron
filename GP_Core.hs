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
, runTest, runTestWith, testGene, testCros
) where

import Control.Monad ( liftM,  forM )
import Data.Typeable ( Typeable )
import Data.Maybe    ( fromJust )

import Util  ( Ral, runRal, runRalWith, maximasBy, putList, logIt, boxIt, boxThem, statIt , StatRecord(..) )
import Dist  ( Dist, mkDist, distGet, distMax,distMin,distAvg, distSize, distTake_new )
import Heval ( evals, eval )

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

-- running -----------------------------------------------------------

run :: (Show term , Evolvable term a gOpt mOpt cOpt) => Problem term a gOpt mOpt cOpt -> IO ()
run problem = do 
  ret <- runRal [] $ evolveIt problem
  putStrLn . show $ ret

runWith :: (Show term , Evolvable term a gOpt mOpt cOpt) => String -> Problem term a gOpt mOpt cOpt -> IO ()
runWith seedStr problem = do 
  ret <- runRalWith seedStr [] $ evolveIt problem
  putStrLn . show $ ret


-- testing ----------------------------------------------------------

runTest :: Ral a -> IO ()
runTest test = do
 runRal [] test
 return ()

-- 2097148790 558345920
runTestWith :: String -> Ral a -> IO ()
runTestWith seedStr test = do
 runRalWith seedStr [] test
 return ()


testGene :: (Gene t o, Typeable a,Show t) => Int -> o -> FitFun t a -> (a->String) -> Ral [t]
testGene n opt fitFun showResult = do
 ts <- generateIt n opt
 mapM (\(i,t) -> testGene1 (show i ++ "/" ++ show n) fitFun showResult t) (zip [1..] ts) 
 return ts

testCros :: (Gene t go, Cros t co, Typeable a,Show t) => Int -> Int -> go -> co -> FitFun t a -> (a->String) -> Ral [(t,t)]
testCros i n gOpt cOpt fitFun showResult = do
 ts <- testGene n gOpt fitFun showResult
 testCros' i cOpt fitFun showResult . toPairs $ ts

testCros' :: (Cros t o, Show t,Typeable a) => Int -> o -> FitFun t a -> (a->String) -> [(t,t)] -> Ral [(t,t)]
testCros' 0 _ _      _          ps = return ps
testCros' n o fitFun showResult ps = do
  ps' <- forM ps $ \ (t1,t2) -> do
   pair'@(u1,u2) <- crossIt o t1 t2
   (fv1,res1) <- testEval fitFun u1
   (fv2,res2) <- testEval fitFun u2
   boxThem $  [ show t1 , 
                show t2 , 
                show u1 ] ++ (f showResult res1) ++ [ show fv1 , 
                show u2 ] ++ (f showResult res2) ++ [ show fv2 ]
   -- boxThem [ show t1 , show t2 , show u1 , show u2 ]
   return pair'
  logIt "------------------------------------------------------------------------"
  testCros' (n-1) o fitFun showResult ps'
 where
  f :: (a->String) -> Maybe a -> [String]
  f _ Nothing = []
  f showFun (Just x) = [showFun x] 

toPairs :: [a] -> [(a,a)]
toPairs []  = []
toPairs [_] = []
toPairs (x1:x2:xs) = (x1,x2) : toPairs xs

unPair :: [(a,a)]->[a]
unPair = foldr (\(x1,x2) acc->x1:x2:acc) [] 

testEval :: (Typeable a) => FitFun term a -> term -> Ral (FitVal , Maybe a)
testEval fitFun term = case fitFun of
 FF1 ff _ -> do
  fitVal <- ff term
  return (fitVal , Nothing )
 FF2 toStr as ff -> do
  let haskellTerm = toStr term
      result      = eval haskellTerm as
  fitVal <- ff result
  return ( fitVal , Just result )

testGene1 :: (Typeable a,Show term) => String -> FitFun term a -> (a->String) -> term -> Ral ()
testGene1 str fitFun showResult term = case fitFun of 
 FF1 ff _ -> do
  fitVal <- ff term
  boxThem [ str , show term , show fitVal ]
 FF2 toStr as ff -> do
  let haskellTerm = toStr term
      result      = eval haskellTerm as
  fitVal <- ff result
  boxThem [ str , haskellTerm , showResult result , show fitVal ]

