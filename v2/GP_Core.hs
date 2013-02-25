{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TupleSections #-}

module GP_Core where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad ( liftM,  forM , replicateM )
import Data.Typeable ( Typeable )
import Data.Maybe    ( fromJust )
import Text.Printf (printf)
import Data.List 
import System.Directory

import Control.Monad.State ( liftIO )

import Dist ( Dist, mkDist, distGet, distMax,distMin,distAvg, distSize, distTake_new )
import Eva (Eva,runEva,runEvaWith,statIt,evals,eval,GenInfoType(..),StatRecord(..)
           ,RunID,RunInfos,Stats , sendJSON , flushStdout )
import Utils (logIt,boxIt,putList,boxThem,JShow,jshow)

import ServerInterface (graphCmd, multiCmd )

type PopSize = Int
type NumGene = Int
type FitVal  = Double
type Prob    = Double
type RunInfo = (Int,Int)

data Problem term a gOpt mOpt cOpt = Problem
 { problemName :: String
 , popSize     :: PopSize
 , numGene     :: NumGene
 , genOps      :: Dist (GenOp term)
 , gOpt        :: gOpt
 , mOpt        :: mOpt
 , cOpt        :: cOpt
 , fitFun      :: FitFun term a
 }

data FitFun term a = 
  FF1 (term -> Eva FitVal) a | 
  FF2 (term->String) a (a->Eva FitVal) |
  FF3 (term->String) a (a->Eva (FitVal,Bool) )

type GenOpProbs = (Prob,Prob,Prob)
data GenOpType = Reproduction | Mutation | Crossover
data GenOp term = 
  MonoOp (term->Eva term) | 
  DiOp   (term->term->Eva (term,term))

class Gene term opt where
  generateIt :: Int -> opt -> Eva [term]

class Muta term opt where
 mutateIt :: opt -> term -> Eva term 

class Cros term opt where
 crossIt :: opt -> term -> term -> Eva (term,term)  

class Evolvable term a gOpt mOpt cOpt where
  evolveIt :: RunInfo -> Problem term a gOpt mOpt cOpt -> Eva (term,FitVal,Maybe Int)

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
    case mbc of
     Nothing  -> chain (n-1) f r a 
     Just (b,c) -> return (b,c,Just (numGens-n+1) ) 
 
evolveBegin :: ( Gene t go, Typeable a,JShow t) => RunInfo -> Problem t a go mo co -> Eva (Dist t , Maybe (t,FitVal) )
evolveBegin runInfo p = do
 let n = popSize p 
 terms <- generateIt n (gOpt p)
 ret@(pop0,_) <- evalFF (fitFun p) terms
 logGeneration runInfo 0 pop0
 return ret

evolveStep ::(Muta t mo,Cros t co,Typeable a,JShow t)=> RunInfo->Problem t a go mo co -> Int -> Dist t -> Eva(Dist t,Maybe(t,FitVal))
evolveStep runInfo p i pop = do
 let best     =  getBest pop 
 terms        <- distTake_new (popSize p - 1) pop   
 terms'       <- performOps (genOps p) terms 
 ret@(pop',_) <- evalFF (fitFun p) ( best : terms' )
 logGeneration runInfo i pop'
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
    [t] -> return [t]
    (t1:t2:tt) -> do
     (t1',t2') <- f t1 t2
     tt'       <- performOps opDist tt
     return $ t1' : t2' : tt'



getBest :: Dist term -> term 
getBest pop = best 
 where Just (best,_) = distMax pop 

logGeneration :: (JShow term) => RunInfo -> Int -> Dist term -> Eva ()
logGeneration (actRun,allRuns) i pop = do  
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
 boxIt  $ show best 
 statIt $ GenInfo actRun i BestOfGen  b
 statIt $ GenInfo actRun i AvgOfGen   a
 statIt $ GenInfo actRun i WorstOfGen w
 stdout <- flushStdout 
 sendJSON $ multiCmd [ stdout , graphCmd actRun i (b,a,w) , jshow best ]
  
-- statIt $ SR_Best  i b
-- statIt $ SR_Avg   i a 
-- statIt $ SR_Worst i w 



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

mkFF1 :: (term -> Eva FitVal) -> FitFun term ()
mkFF1 ff = FF1 ff () 

-- running -----------------------------------------------------------

runByServer :: (Show term , Evolvable term a gOpt mOpt cOpt) => String -> Problem term a gOpt mOpt cOpt -> IO ()
runByServer jobID problem = 
 let evaToRun = (statIt (StrInfo "jobID" jobID) ) >> evolveIt (1,1) problem
  in do
    (ret,stats) <- runEva $ evaToRun
    -- writeStats problem stats []
    putStrLn . show $ ret

nRunsByServer :: (Show term , Evolvable term a gOpt mOpt cOpt) => 
                 String -> Int -> Problem term a gOpt mOpt cOpt -> IO ()
nRunsByServer jobID numRuns problem = do
  (ret,stats) <- runEva $ statIt (StrInfo "jobID" jobID) >> multipleRuns numRuns problem
  -- writeStats problem stats ret
  putList ret


run :: (Show term , Evolvable term a gOpt mOpt cOpt) => Problem term a gOpt mOpt cOpt -> IO ()
run problem = do 
  (ret,stats) <- runEva $ evolveIt (1,1) problem
  writeStats problem stats []
  putStrLn . show $ ret

runWith :: (Show term , Evolvable term a gOpt mOpt cOpt) => String -> Problem term a gOpt mOpt cOpt -> IO ()
runWith seedStr problem = do 
  (ret,stats) <- runEvaWith (read seedStr) $ evolveIt (1,1) problem
  writeStats problem stats []
  putStrLn . show $ ret

nRuns :: (Show term , Evolvable term a gOpt mOpt cOpt) => Int -> Problem term a gOpt mOpt cOpt -> IO ()
nRuns numRuns p = do
  (ret,stats) <- runEva $ multipleRuns numRuns p
  writeStats p stats ret
  --let pilatPerfStr = showPilatPerformance $ computePilatPerformance (map (\(_,_,m)->m) ret)
  --putStrLn kozaPerfStr
  --writeFile "kozaPerf.txt" kozaPerfStr
  putList ret

-- where

multipleRuns :: Evolvable t a go mo co => Int -> Problem t a go mo co -> Eva [(t,FitVal,Maybe Int)]
multipleRuns numRuns problem = multipleRuns' numRuns problem
 where
  multipleRuns' :: Evolvable t a go mo co => Int -> Problem t a go mo co -> Eva [(t,FitVal,Maybe Int)]
  multipleRuns' 0 _ = return [] 
  multipleRuns' n p = do 
   x  <- evolveIt ( numRuns-n+1 , numRuns ) p
   xs <- multipleRuns' (n-1) p
   return (x:xs)



showKozaPerformance :: [Prob] -> String
showKozaPerformance xs = concat [ show i ++ " " ++ show d ++ "\n" | (i,d) <- zip [0..] xs ]

computeKozaPerformance :: Int -> [Maybe Int] -> [Prob]
computeKozaPerformance nGene xs = map toProb (foldr f (replicate (nGene+1) 0) xs)
 where
  numRuns = fromIntegral $ length xs
  toProb  = (/numRuns) . fromIntegral
  f :: Maybe Int -> [Int] -> [Int]
  f Nothing  xs = xs
  f (Just i) xs = let (as,bs) = splitAt i xs in as ++ (map (+1) bs)

showPilatPerformance :: [(FitVal,FitVal,FitVal)] -> String
showPilatPerformance xs 
 = concat [ show i++" "++show best++" "++show avg++" "++show worst++"\n" | (i,(best,avg,worst)) <- zip [0..] xs ]

computePilatPerformance :: [[FitVal]] -> [(FitVal,FitVal,FitVal)]
computePilatPerformance bestsPerRuns = 
 let bestsPerGens = transpose bestsPerRuns
     avg xs = (sum xs) / (fromIntegral $ length xs)
  in map (\bestsOfGenI -> ( maximum bestsOfGenI , avg bestsOfGenI , minimum bestsOfGenI ) ) bestsPerGens
  
   
-- stating ------------------------

showStats :: Map RunID RunInfos -> [(RunID,String)]
showStats stats = 
  [ ( runID , oneRunGraphStr ++ 
              oneDataStream runInfos BestOfGen ++
              oneDataStream runInfos AvgOfGen  ++
              oneDataStream runInfos WorstOfGen ) | 
    (runID,runInfos) <- Map.toAscList stats ] 

oneDataStream :: RunInfos -> GenInfoType -> String
oneDataStream runInfos git 
 = ( concat [ show genID++" "++show val++"\n" | 
              (genID,genInfos) <- Map.toAscList runInfos ,
              let Just val = Map.lookup git genInfos ] ) ++ "e\n"

writeStats :: Problem term a gOpt mOpt cOpt -> Stats -> [(term,FitVal,Maybe Int)] -> IO ()
writeStats problem (stats,_) retFromNRuns = do
  isThere <- doesDirectoryExist pName 
  pName' <- if isThere then checkFreeName 2 pName else return pName
  createDirectory pName' 
  setCurrentDirectory pName'
  createDirectory dirName
  if null retFromNRuns then return ()
  else do 
   let kozaPerfStr = showKozaPerformance $ computeKozaPerformance (numGene problem) (map (\(_,_,m)->m) retFromNRuns)
   putStrLn kozaPerfStr
   writeFile "kozaPerf.txt" kozaPerfStr
  setCurrentDirectory ".."  
  mapM_ ( \ (runID,str) -> writeFile ( pName' ++ "/" ++ dirName ++"/" ++ "run-" ++ show runID ++ ".gpl") str ) (showStats stats)
 where 
  pName = problemName problem
  dirName = "runs"


checkFreeName :: Int -> String -> IO String
checkFreeName i base = 
  let tryName = (base++"-"++show i)
   in do 
    itExist <- doesDirectoryExist tryName 
    if itExist
    then checkFreeName (i+1) base 
    else return tryName 

oneRunGraphStr :: String
oneRunGraphStr = 
 "plot \'-\' title 'best'  with linespoints ,\\\n" ++
 "     \'-\' title 'avg'   with linespoints ,\\\n" ++
 "     \'-\' title 'worst' with linespoints\n"





-- testing ----------------------------------------------------------

runTest :: Eva a -> IO ()
runTest test = do
 runEva test
 return ()

-- 2097148790 558345920
runTestWith :: String -> Eva a -> IO ()
runTestWith seedStr test = do
 runEvaWith (read seedStr) test
 return ()


testGene :: (Gene t o, Typeable a,Show t) => Int -> o -> FitFun t a -> (a->String) -> Eva [t]
testGene n opt fitFun showResult = do
 ts <- generateIt n opt
 mapM (\(i,t) -> testGene1 (show i ++ "/" ++ show n) fitFun showResult t) (zip [1..] ts) 
 return ts

testCros :: (Gene t go, Cros t co, Typeable a,Show t) => Int -> Int -> go -> co -> FitFun t a -> (a->String) -> Eva [(t,t)]
testCros i n gOpt cOpt fitFun showResult = do
 ts <- testGene n gOpt fitFun showResult
 testCros' i cOpt fitFun showResult . toPairs $ ts

testCros' :: (Cros t o, Show t,Typeable a) => Int -> o -> FitFun t a -> (a->String) -> [(t,t)] -> Eva [(t,t)]
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

testEval :: (Typeable a) => FitFun term a -> term -> Eva (FitVal , Maybe a)
testEval fitFun term = case fitFun of
 FF1 ff _ -> do
  fitVal <- ff term
  return (fitVal , Nothing )
 FF2 toStr as ff -> do
  let haskellTerm = toStr term
  result <- eval haskellTerm as
  fitVal <- ff result
  return ( fitVal , Just result )

testGene1 :: (Typeable a,Show term) => String -> FitFun term a -> (a->String) -> term -> Eva ()
testGene1 str fitFun showResult term = case fitFun of 
 FF1 ff _ -> do
  fitVal <- ff term
  boxThem [ str , show term , show fitVal ]
 FF2 toStr as ff -> do
  let haskellTerm = toStr term
  result <- eval haskellTerm as
  fitVal <- ff result
  boxThem [ str , haskellTerm , showResult result , show fitVal ]

