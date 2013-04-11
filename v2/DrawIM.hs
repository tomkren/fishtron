module DrawIM (imGraphInJSON) where

import Data.List  ((\\),nub)
import Data.Maybe (catMaybes)

import qualified Data.Map as Map
import Data.Map (Map)


import TTerm (Symbol,Typ(..),Context,typeArgs)
import Utils (newSymbol')

import Text.JSON
import JSONUtils


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

data UpdateCmd = MkVertex Typ Context
               | DeltaCtx Typ Context PreVertex


--  var dummyGraph = [
--    { node : 'a' , opts : {name:'foobar'}               } , 
--    { node : 'b' , opts : {color:'red',name:'Int',sq:1} } ,  
--    { node : 'c' , opts : {sq:1}                        } ,
--    { node : 'd' , opts : {sq:1}                        } ,
--    { node : 'e' , opts : {sq:1}                        } ,   
--    
--    { edge : ['a','b'] , opts : {dir:1,n:2}             } ,
--    { edge : ['c','c'] , opts : {}                      } ,
--    { edge : ['a','c'] , opts : {}                      } ,
--    { edge : ['a','d'] , opts : {}                      } ,
--    { edge : ['a','e'] , opts : {}                      } 
--  ];


imGraphInJSON :: Typ -> Context -> JSValue
imGraphInJSON typ ctx = imGraph2JSON typ $ mkIMGraph typ ctx

imGraph2JSON :: Typ -> IMGraph -> JSValue
imGraph2JSON typ graph = jsArr $ imTypes2nodes typ graph


imTypes2nodes :: Typ -> IMGraph -> [JSValue]
imTypes2nodes problemTyp graph = 
   concatMap (\(typ,forkEdges) -> f typ : concatMap (imForkEdge2jss typ) forkEdges ) $ Map.toList graph
  where
   f :: Typ -> JSValue
   f typ = jsObj [
    ( "node" , typ2nodeID typ ),
    ( "opts" , jsObj $ [ ("name"   , jsStr $ show typ ),
                         ("circle" , jsNum 6 ) ] 
                  ++ (if typ == problemTyp then [("color",jsStr "green")] else [] ) )]




imForkEdge2jss :: Typ -> ForkEdge -> [JSValue]
imForkEdge2jss startTyp ( edgeLabel , finTyps ) =
  let nodeID   = edgeLabel2nodeID   edgeLabel
      nodeName = edgeLabel2nodeName edgeLabel
      f (finTyp,i) = let finNodeID  = typ2nodeID finTyp
                         meziNodeID = jsStr $ "__" ++ nodeID ++ "__" ++ show i 
                      in [ jsObj [ ("node" , meziNodeID ) ] ,
                           jsObj [ ("edge" , jsArr [ jsStr nodeID , meziNodeID ] ), 
                                   ("opts" , jsObj [ ("color", jsStr "red" ) ] ) ] ,
                           jsObj [ ("edge" , jsArr [ meziNodeID , finNodeID ] ), 
                                   ("opts" , jsObj [ ("color", jsStr "red" ) ] ) ]
                         ]
   in [ jsObj [ ("node" , jsStr nodeID ) , ( "opts" , jsObj [ ("name", nodeName ), 
                                                        ("circle",jsNum 3),
                                                        ("color",jsStr "red") ] ) ] ,
        jsObj [ ("edge" , jsArr [ typ2nodeID startTyp , jsStr nodeID ]) ,("opts",jsObj[("color",jsStr "black")])]
      ] ++ concatMap f (zip finTyps [0..])


typ2nodeID :: Typ -> JSValue
typ2nodeID typ = jsStr $ show typ


edgeLabel2nodeID :: EdgeLabel -> String
edgeLabel2nodeID el = case el of
  LVar sym typ -> sym ++ ":" ++ show typ
  LLams ctx    -> show ctx 

edgeLabel2nodeName :: EdgeLabel -> JSValue
edgeLabel2nodeName el = jsStr $ case el of
  LVar sym typ -> sym
  LLams ctx    -> concatMap (\(x,t)->x ++ ":" ++ show t ++" ") ctx


-----------------------------------------------------------------------


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

