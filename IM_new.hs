module IM_new where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List((\\),nub)
import Data.Maybe(catMaybes)
-- import Data.Maybe
import TTerm (Symbol,Typ(..),Context,typeArgs)
import Util  (newSymbol')

-- ( "Edge label" , "Vertices where the fork-edge (= edge with one begin and multiple ends) is pointing" )
type ForkEdge = ( EdgeLabel , [Typ] )


data EdgeLabel = LLams [(Symbol,Typ)] 
               | LVar  Symbol Typ 
               deriving (Show)

-- Structure used for IM construction. 
-- It is graph with fork-edges instead of edges. 
-- Vertices also contain Context needed during IM construction.
type PreGraph  = Map Typ PreVertex
type PreVertex = ( [ForkEdge] , Context )

type Stack = [ ( Typ , Context ) ]

-- TODO : nahradit Context Set-kou


data UpdateCmd = MkVertex Typ Context
               | DeltaCtx Typ Context


mkPreGraph :: [UpdateCmd] -> PreGraph -> PreGraph
mkPreGraph [] graph = graph
mkPreGraph  (cmd:stack) graph = case cmd of
 MkVertex typ ctx ->
  let ( newV , ctx' ) = mkVertex typ ctx
      newCmds = catMaybes . map (whatToDo graph ctx') . succs $ newV
   in mkPreGraph (newCmds:stack) (Map.insert typ newV graph)
 DeltaCtx typ ctx -> 
  let ...


whatToDo :: PreGraph -> Context -> Typ -> Maybe UpdateCmd
whatToDo graph sentCtx child = case Map.lookup typ graph of
 Nothing      -> Just $ MkVertex child sentCtx
 Just (_,ctx) -> case sentCtx \\ ctx of 
  []    -> Nothing 
  delta -> Just $ DeltaCtx child delta


succs :: PreVertex -> [Typ]
succs (edges,_) = nub . concatMap snd $ edges

updateVertex :: PreGraph -> Typ -> Context -> PreGraph
updateVertex graph typ deltaCtx = Map.update upF typ graph
 where
  upF (edges,ctx) = case typ of 
   _:->_     -> Just ( edges , deltaCtx ++ ctx )
   Typ alpha -> ... 

mkVertex :: Typ -> Context -> ( PreVertex , Context ) 
mkVertex typ ctx = case typ of 
  _:->_ ->
   let ( ts , alpha ) = typeArgs typ
       varz           = zip (mkNewVars ctx (length ts)) ts
       forkEdge       = ( LLams varz , [Typ alpha] )
    in ( ( [ forkEdge ] , ctx ) , varz ++ ctx ) 
  Typ alpha ->
   ( ( map mkVarEdge . filter (hasDesiredEnd alpha) $ ctx , ctx ) , ctx )



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

 where

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
  mkNewVars ctx num =  reverse $ mkNewVars' (map fst ctx) [] num
   where
    varAlphabet = ['x'..'z'] ++ ['a'..'w']
    mkNewVars' :: [Symbol] -> [Symbol] -> Int -> [Symbol]
    mkNewVars' occ acc 0 = acc
    mkNewVars' occ acc n 
     = let var = newSymbol' varAlphabet occ  
        in mkNewVars' (var:occ) (var:acc) (n-1)


