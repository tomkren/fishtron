module InhabitationMachines where


import Data.List((\\),nub,intercalate,unlines )
import Data.Maybe(catMaybes)
-- import Data.Maybe
import TTerm (Symbol,Typ(..),TTerm(..),Context,typeArgs,ttermTyp,isInCtx)
import Util  ( newSymbol' , fillStr ,singletonQueue ,  
              Queue, insertsQueue , popQueue , putList ,getRandomL, 
              Ral, runRal ,  boxIt, logIt )

-- import Data.Set (Set)
-- import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import Control.Monad
-- import Debug.Trace

import Text.ParserCombinators.Parsec
import Text.Parsec.Pos

data IM = IM Typ Context IMGraph
type IMGraph = Map Typ [ForkEdge]


-- ( "Edge label" , "Vertices where the fork-edge (= edge with one begin and multiple ends) is pointing" )
type ForkEdge = ( EdgeLabel , [Typ] )


data EdgeLabel = LLams [(Symbol,Typ)] 
               | LVar  Symbol Typ 

-- Structure used for IM construction. 
-- It is graph with fork-edges instead of edges. 
-- Vertices also contain Context needed during IM construction.
type PreGraph  = Map Typ PreVertex
type PreVertex = ( [ForkEdge] , Context )


type Stack = [ ( Typ , Context ) ]

-- TODO : nahradit Context Set-kou
--        realizovat ořezávání nedostupných vrcholů při konstrukci grafu


data UpdateCmd = MkVertex Typ Context
               | DeltaCtx Typ Context PreVertex


----------------------------------------------------------------------------------------------

dou, dou1, dou2 :: Typ
dou  = Typ "Double"
dou1 = dou :-> dou
dou2 = dou :-> dou :-> dou

ctx_ttSSR :: Context
ctx_ttSSR = ([("plus",dou2),("minus",dou2),("krat",dou2),("rdiv",dou2),("sin",dou1),("cos",dou1),("exp",dou1),("rlog",dou1)])

ctx_mini :: Context
ctx_mini = ([("plus",dou2),("sin",dou1)])


testIM :: Int -> Ral [TTerm]
testIM n = ( randProveUnique n 100 dou1 ctx_ttSSR )

-- uniqueN :: Int -> Ral TTerm -> Ral [TTerm]
-- uniqueN n mo = do 
--  gen <- get
--  let (gen1,gen2) = split gen
--  put gen1
--  xs <- uniqueN' Set.empty n mo
--  put gen2
--  return xs

uniqueN :: Int -> Ral TTerm -> Ral [TTerm]
uniqueN num = uniqueN' Set.empty num
 where 
  uniqueN' :: Set TTerm -> Int -> Ral TTerm -> Ral [TTerm]
  uniqueN' _ 0 _ = return []
  uniqueN' set n mo = do
   x <- mo
   if Set.member x set 
    then uniqueN' set n mo
    else do
     boxIt $ "[" ++ show (num-n+1) ++ "/" ++ show num ++ "]\n" ++ show x
     (x:) `liftM` uniqueN' (Set.insert x set) (n-1) mo


-------------------

proveOneWithLimit :: Int -> Typ -> Context -> Maybe TTerm
proveOneWithLimit limit typ ctx = 
 let graph = mkIMGraph typ ctx
     taxi  = mkTaxi'   typ ctx
     tokss = proveWL' limit graph (singletonQueue taxi)
  in case tokss of
   []       -> Nothing
   (toks:_) -> Just (ttParse' ctx toks)
   

proveWL' :: Int -> IMGraph -> Queue Taxi -> [[Token2]]
proveWL' 0 _ _ = []
proveWL' limit im q = case popQueue q of
  Nothing -> []
  Just (taxi,q') -> case nextTaxis' im taxi of
   Left toks   -> toks : (proveWL' (limit-1) im q') 
   Right taxis -> proveWL' (limit-1) im $ insertsQueue taxis q'


----

proveN :: Int -> Typ -> Context -> [TTerm]
proveN n typ ctx = take n $ prove typ ctx 

prove :: Typ -> Context -> [TTerm]
prove typ ctx = 
 let graph = mkIMGraph typ ctx
     taxi  = mkTaxi'   typ ctx
  in map (ttParse' ctx) . prove' graph . singletonQueue $ taxi   

prove' :: IMGraph -> Queue Taxi -> [[Token2]]
prove' im q = case popQueue q of
  Nothing -> []
  Just (taxi,q') -> case nextTaxis' im taxi of
   Left toks   -> toks : (prove' im q')
   Right taxis -> prove' im $ insertsQueue taxis q'


randProveUnique :: Int -> Int -> Typ -> Context -> Ral [TTerm]
randProveUnique n limit typ ctx = uniqueN n $ randProveOne limit typ ctx -- infSetRand $ randProveOne limit typ ctx

randProveN :: Int -> Int -> Typ -> Context -> Ral [TTerm]
randProveN n limit typ ctx = replicateM n $ randProveOne limit typ ctx

randProveOne :: Int -> Typ -> Context -> Ral TTerm
randProveOne limit typ ctx = 
 let graph = mkIMGraph typ ctx
     taxi  = mkTaxi'   typ ctx
  in do
   mToks <- randProveOne' limit graph taxi
   case mToks of
    Just toks -> do 
     let tterm = ttParse' ctx toks
     logIt $ show tterm
     return tterm
    Nothing   -> randProveOne (limit+1) typ ctx


randProveOne' :: Int -> IMGraph -> Taxi -> Ral ( Maybe [Token2] )
randProveOne' 0 _ _ = return Nothing
randProveOne' limit graph taxi = do
 nxt <- randOneNext graph taxi
 case nxt of
  Left toks          -> return . Just $ toks
  Right Nothing      -> return Nothing 
  Right (Just taxi') -> randProveOne' (limit-1) graph taxi'


-- Left : finished tokens ; Right : Unfinished Taxi or Unsuccesfull
randOneNext :: IMGraph -> Taxi -> Ral (  Either [Token2] (Maybe Taxi)  )
randOneNext graph taxi = case nextTaxis' graph taxi of
  Left  toks  -> return $ Left toks
  Right taxis -> case taxis of
   [] -> return $ Right Nothing
   _  -> do
    taxi' <- getRandomL taxis
    return . Right . Just $ taxi' 



-- proveStr :: Context -> Typ -> Int -> IO()
-- proveStr ctx t i 
--   = putStr . unlines $
--     (:) (show iM) $ take i $ map (\toks-> concatMap show toks) $ prove' im $ singletonQueue taxi
--  where
--   iM@(IM _ _ im) = mkIM t ctx
--   taxi = mkTaxi iM
 
-- generating using IM - GENERATING TOKENS ---------------------------------

-- rekonstrukce: Taxi <výstupObrácene> <keZpracování> <dostupnéProměnné>    

type NSymbol  = (Symbol,Int)
type NContext = [NSymbol]

data Token2 = 
 T2Lam [(Symbol,Int,Typ)]   |
 T2Var Symbol Int Typ       |
 T2ParL                     |
 T2ParR                     |
 T2ParR_lam [(Symbol,Int)]  |
 T2Typ Typ                  |
 End

data Taxi = Taxi [Token2] [Token2] NContext  deriving (Show)

mkTaxi' :: Typ -> Context -> Taxi
mkTaxi' t ctx = Taxi [] [T2Typ t] $ map (\(x,_)->(x,1)) ctx

mkTaxi :: IM -> Taxi
mkTaxi (IM t ctx im) = Taxi [] [T2Typ t] $ map (\(x,_)->(x,1)) ctx

nextTaxis' :: IMGraph -> Taxi -> Either [Token2] [Taxi]
nextTaxis' im taxi = case taxi of
  Taxi ret []     _    -> Left $ reverse ret
  Taxi ret (x:xs) nCtx -> case x of
    T2Typ t         -> Right [ Taxi ret (toks++xs) nCtx' | (toks,nCtx') <- nextTaxis im nCtx t ]
    T2ParR_lam vars -> Right [ Taxi (x:ret) xs (nCtx \\ vars) ]
    _               -> Right [ Taxi (x:ret) xs nCtx ]

nextTaxis :: IMGraph -> NContext -> Typ ->  [ ( [Token2] , NContext ) ]
nextTaxis im nCtx typ = case Map.lookup typ im of
  Nothing -> error "unexpected Nothing"
  Just edges -> concatMap (next nCtx) edges

next :: NContext -> (EdgeLabel,[Typ]) -> [ ( [Token2] , NContext ) ]
next nCtx (tok,ts) = case tok of
 LLams vars -> 
  let (nCtx' , lamTok2 , rparTok2 ) = solveTokLam vars nCtx 
      tok2s = [T2ParL , lamTok2] ++ ( map T2Typ ts ) ++ [rparTok2]  
   in [ ( tok2s  , nCtx' ) ]
 LVar x t   -> 
  let f t2Var | null ts   = ( [t2Var] , nCtx )
              | otherwise = ( [T2ParL , t2Var] ++ ( map T2Typ ts ) ++ [T2ParR]  , nCtx ) 
   in map f $ getT2Vars x t nCtx 

getT2Vars :: Symbol -> Typ -> NContext -> [Token2]
getT2Vars x t nCtx = map (\(_,n)-> T2Var x n t ) $ getThatVars nCtx x

solveTokLam :: Context -> NContext -> (NContext , Token2 , Token2 )
solveTokLam ctx nCtx = ( ctx'' ++ nCtx , T2Lam ctx' , T2ParR_lam ctx'' )
 where
  ctx' = map f ctx
  ctx''= map (\(s,i,_)->(s,i)) ctx'
  f :: (Symbol,Typ) -> (Symbol,Int,Typ)
  f (x,t) = (x, 1 + (numOfThatVar nCtx x) ,t)

numOfThatVar :: NContext -> Symbol -> Int
numOfThatVar nCtx x = length $ getThatVars nCtx x 

getThatVars :: NContext -> Symbol -> NContext
getThatVars nCtx x = filter (\(x',_)->x'==x) nCtx 

-- constructing graph ----------------------------------------------------------------------------------

mkIM :: Typ -> Context -> IM
mkIM typ ctx = IM typ ctx (mkIMGraph typ ctx)

mkIMGraph :: Typ -> Context -> IMGraph
mkIMGraph typ ctx = 
 let preGraph = mkPreGraph [MkVertex typ ctx] Map.empty
  in Map.fromList . map (\(t,(es,_))->(t,es)) . Map.toList $ preGraph

mkPreGraph :: [UpdateCmd] -> PreGraph -> PreGraph
mkPreGraph [] graph = graph
mkPreGraph  (cmd:stack) graph = case cmd of
 MkVertex typ ctx ->
  let ( newV , ctx' ) = mkVertex typ ctx
      graph'  = Map.insert typ newV graph
      newCmds = mkNewCmds graph' newV ctx'
   in mkPreGraph (newCmds++stack) graph'
 DeltaCtx typ deltaCtx vertex -> 
  let graph'  = updateEdges graph typ deltaCtx
      newCmds = mkNewCmds graph' vertex deltaCtx
   in mkPreGraph (newCmds++stack) graph'

mkNewCmds :: PreGraph -> PreVertex -> Context -> [UpdateCmd]
mkNewCmds graph vertex ctx = 
  catMaybes . map (whatToDo graph ctx) . succs $ vertex

whatToDo :: PreGraph -> Context -> Typ -> Maybe UpdateCmd
whatToDo graph sentCtx child = case Map.lookup child graph of
 Nothing -> Just $ MkVertex child sentCtx
 Just vertex@(_,ctx) -> case sentCtx \\ ctx of 
  []       -> Nothing 
  deltaCtx -> Just $ DeltaCtx child deltaCtx vertex  

succs :: PreVertex -> [Typ]
succs (edges,_) = nub . concatMap snd $ edges

updateEdges :: PreGraph -> Typ -> Context -> PreGraph
updateEdges graph typ deltaCtx = Map.update upF typ graph
 where
  upF (edges,ctx) = case typ of 
   _:->_     -> Just ( edges , deltaCtx ++ ctx )
   Typ alpha -> 
    let newEdges = mkVarEdges alpha deltaCtx
     in Just ( newEdges ++ edges , deltaCtx ++ ctx ) 

mkVertex :: Typ -> Context -> ( PreVertex , Context ) 
mkVertex typ ctx = case typ of 
  _:->_ ->
   let ( ts , alpha ) = typeArgs typ
       varz           = zip (mkNewVars ctx (length ts)) ts
       forkEdge       = ( LLams varz , [Typ alpha] )
    in ( ( [ forkEdge ] , ctx ) , varz ++ ctx ) 
  Typ alpha ->
   ( ( mkVarEdges alpha ctx , ctx ) , ctx )

mkVarEdges :: Symbol -> Context -> [ForkEdge]
mkVarEdges alpha ctx = map mkVarEdge . filter (hasDesiredEnd alpha) $ ctx

mkVarEdge :: (Symbol,Typ) -> ForkEdge
mkVarEdge (varName , typ) = 
 let ( ts , _ ) = typeArgs typ
  in ( LVar varName typ , ts ) 

hasDesiredEnd :: Symbol -> (Symbol,Typ) -> Bool 
hasDesiredEnd alpha (_,typ) = 
 let (_,beta) = typeArgs typ 
  in alpha == beta 

-- Creates number of new names for vars, so the new names do not occur in Context.
mkNewVars :: Context -> Int -> [Symbol]
mkNewVars ctx num = reverse $ mkNewVars' (map fst ctx) [] num 
 where
  varAlphabet = ['x'..'z'] ++ ['t'..'w'] ++ ['a'..'s']
  mkNewVars' :: [Symbol] -> [Symbol] -> Int -> [Symbol]
  mkNewVars' occ acc 0 = acc
  mkNewVars' occ acc n 
   = let var = newSymbol' varAlphabet occ  
      in mkNewVars' (var:occ) (var:acc) (n-1)


-- generating using IM - PARSING ----------------------------------------------

type Token3 = (SourcePos,Token2)
type MyParser a = GenParser Token3 () a


ttParse' :: Context -> [Token2] -> TTerm
ttParse' ctx tok2s = let Right tt = ttParse tok2s in ctxVarsToVals ctx tt

ctxVarsToVals :: Context -> TTerm -> TTerm
ctxVarsToVals ctx tt = case tt of
 TVar x   t -> if x `isInCtx` ctx then TVal x t else tt 
 TVal _   _ -> tt 
 TLam x m t -> TLam x (ctxVarsToVals ctx m) t
 TApp m n t -> TApp (ctxVarsToVals ctx m) (ctxVarsToVals ctx n) t

ttParse :: [Token2] -> Either ParseError TTerm
ttParse tok2s = parse parseTTEnd "(ttParse : Syntax Error)" (toT3 $ tok2s ++ [End])

parseTT :: MyParser TTerm
parseTT = parseVariable
   <|> do parseLpar
          tt <- parseLambda <|> parseApp
          parseRpar
          return tt  

parseVariable :: MyParser TTerm
parseVariable = do
 (s,i,t) <- parseVar
 return $ TVar (ivar s i) t

ivar :: Symbol -> Int -> Symbol
ivar s i 
 | i==1      = s 
 | otherwise = s ++ show i

parseLambda :: MyParser TTerm
parseLambda = do 
 vars <- parseLam 
 body <- parseTT 
 return $ toTLam vars body 

parseApp :: MyParser TTerm
parseApp = do
 tts <- many1 parseTT
 return $ toTApp tts

toTApp :: [TTerm] -> TTerm
toTApp = foldl1 f
 where
  f :: TTerm -> TTerm -> TTerm 
  f tt1 tt2 = 
   let ( _ :-> typ ) = ttermTyp tt1
    in TApp tt1 tt2 typ

toTLam :: [(Symbol,Int,Typ)] -> TTerm -> TTerm
toTLam vars body = foldr f body vars
 where
  f :: (Symbol,Int,Typ) -> TTerm -> TTerm
  f (s,i,t) acc = TLam (ivar s i) acc (t :-> ttermTyp acc)

toT3 :: [Token2] -> [Token3]
toT3 ts = zip (map (setSourceColumn (initialPos "input")) [1..] ) ts

parseTTEnd :: MyParser TTerm
parseTTEnd = do
  tt <- parseTT
  parseEnd
  return tt

mytoken :: (Token2 -> Maybe a) -> MyParser a
mytoken test = token showToken posToken testToken
 where 
  showToken (pos,tok) = show tok
  posToken  (pos,tok) = pos
  testToken (pos,tok) = test tok

parseEnd :: MyParser ()
parseEnd = mytoken $ \tok -> case tok of
 End -> Just ()
 _   -> Nothing

parseLpar :: MyParser ()
parseLpar = mytoken $ \tok -> case tok of
 T2ParL -> Just ()
 _      -> Nothing

parseRpar :: MyParser ()
parseRpar = mytoken $ \tok -> case tok of
 T2ParR      -> Just ()
 T2ParR_lam _-> Just ()
 _           -> Nothing

parseLam :: MyParser [(Symbol,Int,Typ)]
parseLam = mytoken $ \tok -> case tok of
 T2Lam x -> Just x
 _       -> Nothing

parseVar :: MyParser (Symbol,Int,Typ)
parseVar = mytoken $ \tok -> case tok of
 T2Var s i t -> Just (s,i,t)
 _           -> Nothing



---------------------------------------------------

test_binTree = putList . take 256 $ prove (t1_2:->o:->o) []


o = t0
a = Typ "a"
b = Typ "b"
c = Typ "c"
d = Typ "d"

t0 = Typ "0"
t1 = t 1
t2 = t 2
t3 = t 3
t1_2 = tt 1 2

t :: Int -> Typ
t 0 = t0
t n = (t $ n-1) :-> t0

tt :: Int -> Int -> Typ  
tt n k = foldr (\_ acc -> t (n-1) :-> acc ) t0 [1..k]

contx1 = [("0",o),("succ",t1),("+",t1_2 ),("*",t1_2 )]

---------------------------------------------------

instance Show EdgeLabel where
 show (LLams cxt) = (++) "\\ " $ concatMap (\(x,t)->x ++ ":" ++ show t ++" ") cxt
 show (LVar x _) = x  

instance Show IM where
  show (IM typ ctx graph) = 
    (hLine ++ show typ ++ "\n" ++ show ctx ++ "\n" ++ hLine) ++ 
    (showIMGraph graph) 
   where hLine = (replicate 60 '-') ++ "\n" 

instance Show Token2 where
 show t = case t of
   T2Lam xs     -> "\\ " ++ (concatMap (\(x,n,t)-> showVar x n ++ ":" ++ show t ++" ") xs) ++ ". "
   T2Var x n _  -> showVar x n ++ " "
   T2ParL       -> "( "
   T2ParR       -> " )"
   T2ParR_lam _ -> " )"
   T2Typ t      -> show t
  where
   showVar x n = x ++ ( if n == 1 then "" else show n )


showIMGraph :: IMGraph -> String
showIMGraph graph = concatMap showVertex $ Map.toAscList graph
 where
  showVertex :: ( Typ , [ForkEdge] ) -> String
  showVertex ( typ , edges ) 
   = fillStr 20 (show typ) ++ " :  " ++ 
     (g $ intercalate (replicate 24 ' ') $ map (\(label,ts)-> (show label) ++ " => " ++ show ts ++"\n") edges ) 
  g str = if null str then "\n" else str

showPreGraph :: PreGraph -> String
showPreGraph graph = concatMap showVertex $ Map.toDescList graph
 where
  showVertex :: ( Typ , ( [ForkEdge] , Context ) ) -> String
  showVertex ( typ , ( edges , ctx )  ) 
   = fillStr 20 (show typ) ++ " :  " ++ 
     (g $ intercalate (replicate 24 ' ') $ map (\(label,ts)-> (show label) ++ " => " ++ show ts ++"\n") edges ) 
  g str = if null str then "\n" else str

-----------------

-------------------------------------------------------------------------------------------------------

{-- Tenhle rekurzivní přístup zamítam kul blbýmu kontrolovatelnosti, s frontou de líp


mkTTermsByTyp :: IMGraph -> NContext -> Typ -> [ TTerm ]
mkTTermsByTyp graph nCtx typ = case Map.lookup typ graph of
 Just edges -> map (mkTTermsByEdge graph nCtx typ) edges
 Nothing -> error "mkTTerms : Type " ++ show typ ++ "is not in the IM-graph."

mkTTermsByTypes :: IMGraph -> NContext -> [ Typ ] -> [[ TTerm ]]
mkTTermsByTypes graph nCtx [] = 


mkTTermsByEdge :: IMGraph -> NContext -> Typ -> ForkEdge -> [ TTerm ] 
mkTTermsByEdge graph nCtx typ edge = case edge of
 ( LLams vars , [t] ) -> 
  let tterms = mkTTermsByTyp graph nCtx t
   in map (ttermFromLLams vars typ) tterms  
 ( LVar v t , ts ) -> 
  let ttss = transpose . map (mkTTermsByTyp graph nCtx) $ ts
   in 
 _ -> error "mkTTermsByEdge : LLams must be label of a simple edge. OR is not of '->' type."


ttermFromLLams :: [(Symbol,Typ)] -> Typ -> TTerm -> TTerm
ttermFromLLams ((v,_):vars) typ@( _ :-> b ) body = 
 TLam v ( ttermFromLLams vars b body ) typ 
ttermFromLLams [] typ body = body
ttermFromLLams _ _ _ = error "ttermFromLLams : lambda must have '->' type."

--}

-- next :: NContext -> ForkEdge -> [ ( [Token2] , NContext ) ]

----------------------------------------------------------------------------

{--
mkPreGraph' :: Stack -> PreGraph -> PreGraph
mkPreGraph' [] graph = graph
mkPreGraph' ((typ,ctx):stack) graph = case Map.lookup typ graph of
 Nothing ->
  let newVertex@( edges , ctx' ) = succesors typ ctx
      stack' = addToStack stack edges ctx'
      graph' = Map.insert typ newVertex graph
   in mkPreGraph' stack' graph'
 Just oldVertex@( edges , vCtx ) -> case ctx \\ vCtx of 
  [] -> mkPreGraph' stack graph
  cs -> 
   let ( edges' , vCtx' ) = succesors typ cs
       stack' = addToStack stack edges' vCtx'
       graph' = Map.insert typ ( edges' ++ edges , union vCtx' vCtx ) graph
    in mkPreGraph' stack' graph'


f :: Typ -> Context -> ( ( [ForkEdge] , Context ) , Stack )
f typ ctx
 = let xs@( edges , ctx') = succesors typ ctx 
       stack' = map (\t->(t,ctx')) . nub . concatMap snd $ edges
    in ( xs , stack' )  

addToStack :: Stack -> [ForkEdge] -> Context -> Stack 
addToStack stack edges ctx =
 let news = map (\t->(t,ctx)) . nub . concatMap snd $ edges
  in news ++ stack

-- asi to předělat aby to ten kontexct vracelo jen ty přidaný.. (aby clovek mohl rýznovat co to dělá)
succesors :: Typ -> Context -> ( [ForkEdge] , Context )
succesors typ ctx = case typ of 
  _ :-> _ ->
   let ( ts , alpha ) = typeArgs typ
       varz           = zip (mkNewVars ctx (length ts)) ts
       forkEdge       = ( LLams varz , [Typ alpha] )
    in ( [ forkEdge ] , varz ++ ctx ) 
  Typ alpha ->
   ( map mkVarEdge . filter (hasDesiredEnd alpha) $ ctx , ctx )
--}

