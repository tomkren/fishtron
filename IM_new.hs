module IM_new where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List((\\),nub,intercalate)
import Data.Maybe(catMaybes)
-- import Data.Maybe
import TTerm (Symbol,Typ(..),Context,typeArgs)
import Util  (newSymbol' , fillStr )

import Debug.Trace


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


data UpdateCmd = MkVertex Typ Context
               | DeltaCtx Typ Context PreVertex


mkIM :: Typ -> Context -> IM
mkIM typ ctx = 
 let preGraph = mkPreGraph [MkVertex typ ctx] Map.empty
  in IM typ ctx $ Map.fromList . map (\(t,(es,_))->(t,es)) . Map.toList $ preGraph

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
mkVarEdge (varName , typ) 
 = let ( ts , _ ) = typeArgs typ
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


---------------------------------------------------

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

