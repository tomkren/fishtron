--  Nový pokus o Inhabitation Machines bez potřeby generovat graf dopředu a vůbec to udělat pořádně

--  Asi by to chtělo celý předelat tak, že ZTree bude stavová monada, ale radši 
-- to nejdřív už dodělat když je to takle rozdělaný a až pak dyštak překopat.




module IM 
 ( SearchOptions(..) 
 , defaultSearchOptions 
 , kozaSearchOptions
 , allEdgesSearchOptions
 , geomSearchOptions 
 , prove 
 ) where

import TTerm (Symbol,Typ(..),TTerm(..),Context,typeArgs,ttermTyp,toSki,toSki',checkTyp)

import TTree (CTT,mkCTT2)

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)

import System.Random

import qualified Data.PSQueue as Q


--proveAll :: Int -> Typ -> Context -> [CTT]

prove :: SearchOptions -> [CTT]
prove so = 
  let ( so' , problemHead ) = problemHeadPreproccess so
      trees = proveWith2 so'
   in map ( mkCTT2 problemHead . toSki' . tree2tterm ) trees   -- <============== toSki s ' je s typeCheckem ...........

problemHeadPreproccess :: SearchOptions -> ( SearchOptions , Context )
problemHeadPreproccess so = 
  let ( typ' , ctx' , problemHead ) = problemHeadPreproccess' (so_typ so) (so_ctx so)
   in ( so{ so_typ = typ' , so_ctx = ctx' } , problemHead )

problemHeadPreproccess' :: Typ -> Context -> (Typ,Context,Context)
problemHeadPreproccess' typ ctx = 
  let (ts,alpha)  = typeArgs typ
      ss          = map (\i->'x':show i) [0..]
      problemHead = zip ss ts
   in (Typ alpha , problemHead ++ ctx , problemHead )






data ZTree = ZTree { 

  current     :: Tree , 
  dads        :: [ DTree ]  ,

  searchOpts  :: SearchOptions,
  rand        :: StdGen,

  locals      :: Table ,
  globals     :: Table ,

  depth       :: Int ,
  nextVar     :: Int ,
  numUnsolved :: Int ,
  numSteps    :: Int ,
  zTreeID     :: [Int]   -- pro rychlé porovnávání, mění se při expandu

 } --deriving ( Eq , Ord )

mkZTree :: SearchOptions -> ZTree
mkZTree so    = ZTree                  { 
  current     = TreeTyp (so_typ so)    , 
  dads        = []                     ,  
  locals      = emptyTable             , 
  globals     = ctxToTable (so_ctx so) , 
  depth       = 0                      , 
  nextVar     = 0                      ,
  numUnsolved = 1                      ,
  numSteps    = 0                      ,
  zTreeID     = []                     ,
  searchOpts  = so                     ,
  rand        = so_stdGen so           
 } 


data Tree = TreeApp  Symbol  SymbolOfAtomicType [Tree] 
          | TreeLam [Symbol] Typ  Tree
          | TreeTyp Typ


tree2tterm :: Tree -> TTerm
tree2tterm tree = case tree of
  TreeTyp _      -> t2tError "Tree must be without type-nodes."
  TreeLam [] _ _ -> t2tError "Lambda with no vars."
  TreeLam ss typ t -> 
   let (typs,alpha) = typeArgs typ
       tt0 = tree2tterm t
    in fst $ foldr (\(s,ty1) (tt,ty2)->let ty = ty1:->ty2 in(TLam s tt ty,ty) ) (tt0,Typ alpha) (zip ss typs)
  TreeApp s alpha ts -> 
   let tts  = map tree2tterm ts
       tys  = map ttermTyp tts
       sTyp = foldr (:->) (Typ alpha) tys
    in fst $ foldl (\(f,_:->ty) tt-> (TApp f tt ty,ty) ) (varOrVal s sTyp,sTyp) tts

 where
  varOrVal :: String -> Typ -> TTerm
  varOrVal s@('_':_) typ = TVar s typ
  varOrVal s         typ = TVal s typ    

t2tError str = error $ "ERROR in tree2tterm : " ++ str



data DTree = DTreeApp [Tree] Symbol SymbolOfAtomicType [Tree]
           | DTreeLam [Symbol] Typ


type Table = Map SymbolOfAtomicType (Set Entry)
data Entry = Entry [Typ] Symbol deriving (Eq,Ord)

type SymbolOfAtomicType = Symbol
type PriorityQueue = Q.PSQ ZTree Int

type Edge = (Symbol,[Typ],SymbolOfAtomicType)
type Depth = Int

-- snad neni nebezpečné, je tu kvuli zatřiďování do prioritní fronty
instance Eq  ZTree where zt1 == zt2 = zTreeID zt1 == zTreeID zt2  
instance Ord ZTree where zt1 <= zt2 = zTreeID zt1 <= zTreeID zt2


data SearchOptions = SearchOptions{
  so_n                  :: Int                , 
  so_typ                :: Typ                ,  
  so_ctx                :: Context            ,
  so_runLen             :: Maybe Int          , -- Nothing means unlimited runLen 
  so_stdGen             :: StdGen             ,
  so_edgeSelectionModel :: EdgeSelectionModel ,
  so_randomRunState     :: RandomRunState     
 }

defaultSearchOptions :: Int -> Typ -> Context -> SearchOptions
defaultSearchOptions n typ ctx = SearchOptions {
  so_n                  = n ,
  so_typ                = typ ,
  so_ctx                = ctx ,
  so_runLen             = Nothing ,
  so_stdGen             = mkStdGen 42424242 ,
  so_edgeSelectionModel = AllEdges ,
  so_randomRunState     = NoRandomRunState 
 }

kozaSearchOptions :: Int -> Typ -> Context -> StdGen -> SearchOptions
kozaSearchOptions n typ ctx stdGen = SearchOptions {
    so_n                  = n      ,
    so_typ                = typ    ,
    so_ctx                = ctx    ,
    so_stdGen             = stdGen ,
    so_runLen             = Just 1 , 
    so_randomRunState     = KozaRandomRunState Nothing Nothing ,
    so_edgeSelectionModel = KozaESM 
  } 

allEdgesSearchOptions :: Int -> Typ -> Context -> StdGen -> SearchOptions
allEdgesSearchOptions n typ ctx stdGen = SearchOptions {
    so_n                  = n      ,
    so_typ                = typ    ,
    so_ctx                = ctx    ,
    so_stdGen             = stdGen ,
    so_runLen             = Nothing   ,
    so_randomRunState     = NoRandomRunState , 
    so_edgeSelectionModel = AllEdges   
  } 

geomSearchOptions :: Double -> Int -> Typ -> Context -> StdGen -> SearchOptions
geomSearchOptions p n typ ctx stdGen = SearchOptions {
    so_n                  = n      ,
    so_typ                = typ    ,
    so_ctx                = ctx    ,
    so_stdGen             = stdGen ,
    so_runLen             = Nothing   ,
    so_randomRunState     = NoRandomRunState , 
    so_edgeSelectionModel = ContinueProbGeomWithDepth p   
  } 



data RandomRunState =
  NoRandomRunState |
  KozaRandomRunState (Maybe IsFullMethod) (Maybe MaximalDepth)

type IsFullMethod = Bool -- Nothing značí že chci aby se vygeneroval
type MaximalDepth = Int  -- 

spinRandomRunState :: RandomRunState -> StdGen -> (RandomRunState, StdGen)
spinRandomRunState rrs gen0 = case rrs of
  NoRandomRunState -> ( NoRandomRunState , gen0 )
  KozaRandomRunState _ _ -> 
    let (     isFull , gen1 ) = random gen0
        ( m_maxDepth , gen2 ) = randomL [1..6] gen1 --randomL [1..6] gen1
     in ( KozaRandomRunState (Just isFull) m_maxDepth , gen2 )     



data EdgeSelectionModel = 
  AllEdges                               | 
  OneRandomEdgeWithPedicat (Edge->Bool)  |
  KozaESM                                |
  ContinueProbESM Double                 |
  ContinueProbGeomWithDepth Double

selectEdges :: EdgeSelectionModel -> ZTree -> [Edge] -> StdGen -> ( [Edge] , StdGen ) 
selectEdges esm zt edges gen0 = case esm of
  AllEdges                    -> ( edges, gen0 )
  OneRandomEdgeWithPedicat p  -> oneRandomEdgeWithPedicat edges p gen0
  ContinueProbESM prob        -> continueProbESM edges prob gen0
  ContinueProbGeomWithDepth q -> continueProbGeomWithDepth edges q (depth zt) gen0
  KozaESM                     -> 
    let KozaRandomRunState (Just isFull) (Just maxDepth) = so_randomRunState (searchOpts zt)
     in kozaESM isFull maxDepth (depth zt) edges gen0


continueProbESM :: [a] -> Double -> StdGen -> ( [a] , StdGen )
continueProbESM edges prob gen0 = 
  let len = length edges
      (gen1,gen2) = split gen0
      ds          = take len $ randomRs (0.0,1.0) gen1
   in ( map fst . filter (\(e,d)->d<=prob) $ zip edges ds , gen2)

continueProbGeomWithDepth :: [a] -> Double -> Int -> StdGen -> ( [a] , StdGen )
continueProbGeomWithDepth edges q currDepth gen0 = 
  continueProbESM edges (q**(fromIntegral currDepth)) gen0

kozaESM :: IsFullMethod -> MaximalDepth -> Depth -> [Edge] -> StdGen -> ( [Edge] , StdGen )
kozaESM isFull maxDepth depth edges gen0 =
 let (ts,ns) = partition isTerminal edges
     candids | depth == 0        = ns -- 0
             | depth == maxDepth = ts
             | depth >  maxDepth = []
             | isFull            = ns
             | otherwise         = edges
     ( m_edge , gen1 ) = randomL candids gen0
  in ( maybeToList m_edge , gen1 )

isTerminal :: Edge -> Bool
isTerminal (_,[],_) = True
isTerminal _        = False


oneRandomEdgeWithPedicat :: [Edge] -> (Edge->Bool) -> StdGen -> ( [Edge] , StdGen )
oneRandomEdgeWithPedicat edges p gen0 = 
  let okEdges = filter p edges
      ( m_edge , gen1 ) = randomL okEdges gen0
   in ( maybeToList m_edge , gen1 )





proveWith2 :: SearchOptions -> [Tree]
proveWith2 so = proveWith' so (so_n so)
 where
  m_runLen = so_runLen so
  
  min' :: Maybe Int -> Int -> Int
  min' (Just x) y = min x y
  min' Nothing  y = y

  proveWith' :: SearchOptions -> Int -> [Tree]
  proveWith' so0 toMake 
   | toMake <= 0 = []
   | otherwise   = 
     let (so1,so2)= splitSearchOptions so0  --         <=============================
         trees    = proveN_ (min' m_runLen toMake) (spinRandomRunState_ so1)
         numTrees = length trees
      in if numTrees < toMake then
          trees ++ ( proveWith' so2 (toMake - numTrees) )  
         else
          trees 


proveWith :: SearchOptions -> [Tree]
proveWith so = case m_runLen of
  Nothing -> proveN_ n so
  _       -> proveWith' so n
 where
  --so       = initSO pre_so
  n        = so_n so
  m_runLen = so_runLen so
  runLen   = fromJust m_runLen
  proveWith' :: SearchOptions -> Int -> [Tree]
  proveWith' so0 toMake 
   | toMake <= 0 = []
   | otherwise   = 
     let (so1,so2)= splitSearchOptions so0  --         <=============================
         trees    = proveN_ (min runLen toMake) (spinRandomRunState_ so1)
         numTrees = length trees
      in if numTrees < toMake then
          trees ++ ( proveWith' so2 (toMake - numTrees) )  
         else
          trees 

splitSearchOptions :: SearchOptions -> (SearchOptions,SearchOptions)
splitSearchOptions so = let (g1,g2) = split (so_stdGen so) in ( so{ so_stdGen = g1 } , so{ so_stdGen = g2 } )

spinRandomRunState_ :: SearchOptions -> SearchOptions
spinRandomRunState_ so0 = 
  let gen0            = so_stdGen so0
      rrs0            = so_randomRunState so0
      ( rrs1 , gen1 ) = spinRandomRunState rrs0 gen0
   in so0{ so_randomRunState = rrs1 , 
           so_stdGen         = gen1 }



proveN_ :: Int -> SearchOptions -> [Tree]
proveN_ n so = map zTreeToTree . proveN' n $ (initQueue $ mkZTree so )
 where 
  proveN' :: Int -> PriorityQueue -> [ZTree]
  proveN' 0 _ = []
  proveN' n q = case proveStep q of
    Nothing -> []
    Just ( m_solution , q' ) -> case m_solution of
      Nothing  -> proveN' n q'
      Just sol -> sol : proveN' (n-1) q'


initQueue :: ZTree -> PriorityQueue
initQueue zt = Q.singleton zt (optimistNumStepPrediction zt)

proveStep :: PriorityQueue -> Maybe ( Maybe ZTree , PriorityQueue )
proveStep q = case Q.minView q of
  Nothing                  -> Nothing
  Just ( zt Q.:-> _ , q' ) -> case step zt of
    Nothing  -> Just ( Just zt , q' )
    Just zts -> Just ( Nothing , foldl insertZTree q' zts )
 where
  insertZTree :: PriorityQueue -> ZTree -> PriorityQueue
  insertZTree q zt = Q.insert zt (optimistNumStepPrediction zt) q

step :: ZTree -> Maybe [ZTree]
step zt = nextTreeTypNode zt >>= return . map incrementNumSteps . expand

incrementNumSteps :: ZTree -> ZTree
incrementNumSteps zt = zt{ numSteps = (numSteps zt) + 1 }

expand :: ZTree -> [ZTree]
expand zt = case current zt of
  TreeTyp typ -> case typ of
    Typ alpha -> let (edges,zt') = getEdges zt -- <=========== důležitý místo
                     zts = map (expandApp zt') edges 
                  in map (\(z,i,gen)-> z{ zTreeID = i:(zTreeID z) , rand = gen } ) (zip3 zts [1..] (splitInf $ rand zt') )
    _         -> [let z = expandLam typ zt in z{ zTreeID = 1:(zTreeID z)  }] 
  _ -> error "expand : Only type-node can be expanded !"




splitInf :: StdGen -> [StdGen]
splitInf g0 = let (g1,g2) = split g0 in g1 : splitInf g2

randomL :: RandomGen g => [a] -> g -> ( Maybe a , g)
randomL [] g = (Nothing , g)
randomL xs g = let (i,g') = randomR (0,length xs - 1) g in ( Just $ xs !! i , g' ) 



getEdges :: ZTree -> ( [ (Symbol,[Typ],SymbolOfAtomicType) ] , ZTree )
getEdges zt = case current zt of
  TreeTyp (Typ alpha) -> 
    let edges               = (getEdges' (locals zt) alpha ) ++ (getEdges' (globals zt) alpha )
        esm                 = so_edgeSelectionModel (searchOpts zt)
        ( edges' , gen' )   = selectEdges esm zt edges (rand zt)
     in ( edges' , zt{ rand = gen' }) 

  _ -> error "getEdges : Only applicable in atomic-type-node !"


getEdges' :: Table -> SymbolOfAtomicType -> [ (Symbol,[Typ],SymbolOfAtomicType) ]
getEdges' table alpha = case Map.lookup alpha table of
  Nothing -> []
  Just entrySet -> map (\(Entry ts sym)->(sym,ts,alpha)) (Set.toAscList entrySet)

expandApp :: ZTree -> (Symbol,[Typ],SymbolOfAtomicType) -> ZTree
expandApp zt edge = 
  let (tree,deltaUnsolved) = expandApp' edge
   in zt{ current = tree , numUnsolved = (numUnsolved zt) + deltaUnsolved }

expandApp' :: (Symbol,[Typ],SymbolOfAtomicType) -> (Tree,Int)
expandApp' (s,ts,alpha) = ( TreeApp s alpha (map TreeTyp ts) , (length ts) - 1 )  

expandLam :: Typ -> ZTree -> ZTree
expandLam typ zt = 
  let (tree,i) = expandLam' (nextVar zt) (typeArgs typ)
   in zt{ current = tree , nextVar = i }

expandLam' :: Int -> ([Typ],SymbolOfAtomicType) -> (Tree,Int)
expandLam' i (ts,alpha) = 
   let n = length ts
       ( ss , typ , _ ) = foldr f ([],Typ alpha,i+n-1) ts
    in ( TreeLam ss typ (TreeTyp (Typ alpha)) , i+n )
 where
  f :: Typ -> ([Symbol],Typ,Int) -> ([Symbol],Typ,Int)
  f typ1 (ss,typ2,i) = ( ('_' : show i) : ss , typ1 :-> typ2 , i-1 )



zTreeToTree :: ZTree -> Tree
zTreeToTree zt = current $ goTop zt

--treeToTTerm :: Tree -> TTerm
--treeToTTerm t = case t of
--  TreeApp s alpha ts -> 

optimistNumStepPrediction :: ZTree -> Int
optimistNumStepPrediction zt = (numSteps zt) + (numUnsolved zt) 

emptyTable :: Table
emptyTable = Map.empty

addToTableWith :: (Set Entry->Set Entry->Set Entry) -> Table -> Context -> Table
addToTableWith op table ctx = foldr (f op) table ctx
-- where
f :: (Set Entry->Set Entry->Set Entry)->        (Symbol,Typ) -> Table -> Table
f op      (sym,typ) acc = 
  let (ts,alpha) = typeArgs typ
   in Map.insertWith op alpha (Set.singleton $ Entry ts sym) acc 

ctxToTable :: Context -> Table
ctxToTable ctx = addToTableWith Set.union emptyTable ctx

addToTable   :: Table -> [Symbol] -> Typ -> Table
addToTable   table ss typ = addToTableWith Set.union table (zip ss (fst $ typeArgs typ))

subFromTable_bug :: Table -> [Symbol] -> Typ -> Table
subFromTable_bug table ss typ = addToTableWith (Set.\\)  table (zip ss (fst $ typeArgs typ))

subFromTable :: Table -> [Symbol] -> Typ -> Table
subFromTable table ss typ = addToTableWith (flip (Set.\\))  table (zip ss (fst $ typeArgs typ))




nextTreeTypNode :: ZTree -> Maybe ZTree 
nextTreeTypNode zt = case current zt of 
  TreeTyp _ -> Just zt
  _         -> dfsNext zt >>= nextTreeTypNode

dfsNext :: ZTree -> Maybe ZTree
dfsNext zt = case current zt of
  TreeLam _ _ _     -> goDown zt
  TreeApp _ _ (_:_) -> goDown zt
  _                 -> dfsNextRight zt
 where
  dfsNextRight :: ZTree -> Maybe ZTree
  dfsNextRight zt = 
    case rightBrother zt of
      Nothing -> goUp zt >>= dfsNextRight
      x -> x



rightBrother :: ZTree -> Maybe ZTree
rightBrother zt = case dads zt of
  [] -> Nothing
  dad:ancestors -> case dad of
    DTreeLam _ _      -> Nothing
    DTreeApp _ _ _ [] -> Nothing    
    DTreeApp leftBrothers s typ (thatBrother:rightBrothers) ->
      Just $ zt{ current = thatBrother , 
                 dads    = (DTreeApp ( current zt :leftBrothers) s typ rightBrothers )  : ancestors  }



goUp :: ZTree -> Maybe ZTree
goUp zt = case dads zt of
  [] -> Nothing
  dad:ancestors -> 
   let (t ,localVars ) = (current zt,locals zt)
       (t',localVars') = case dad of
                          DTreeApp ts1 s alpha ts2 -> ( TreeApp s alpha ( reverseConcat ts1 (t:ts2) ) , localVars )  
                          DTreeLam ss typ          -> ( TreeLam ss typ t , subFromTable localVars ss typ )
    in Just $ zt{ current = t' , 
                  locals  = localVars' ,
                  dads    = ancestors  ,
                  depth   = (depth zt) - 1 }                        
 where  
  reverseConcat :: [a] -> [a] -> [a]
  reverseConcat []     ys = ys
  reverseConcat (x:xs) ys = reverseConcat xs (x:ys)

goDown :: ZTree -> Maybe ZTree
goDown zt = case current zt of
  TreeApp s  alpha (t:ts) -> Just $ zt{ current = t , dads = (DTreeApp [] s alpha ts):(dads zt) , depth = (depth zt) + 1 }
  TreeLam ss typ    t     -> Just $ zt{ current = t , dads = (DTreeLam   ss typ     ):(dads zt) , depth = (depth zt) + 1 ,   
                                        locals  = addToTable (locals zt) ss typ } 
  _                       -> Nothing

goTop :: ZTree -> ZTree
goTop zt = case goUp zt of
  Nothing  -> zt
  Just zt' -> goTop zt'



instance Show Entry where show (Entry ts sym) = sym ++ " ... " ++ show ts

instance Show Tree where
  show (TreeTyp t)       = show t
  show (TreeApp s  _ []) = s 
  show (TreeApp s  _ ts) = "(" ++ s ++ " " ++ (fillSpaces ts) ++ ")" 
  --show (TreeLam ss _ t ) = "(\\ " ++ (intercalate " " ss) ++ " . " ++ show t ++ ")"  
  show (TreeLam ss typ t ) = "(\\ " ++ (showLamHead ss typ) ++ " . " ++ show t ++ ")"  
   where
    showLamHead :: [Symbol] -> Typ -> String
    showLamHead ss typ = 
      let (ts,_) = typeArgs typ
       in intercalate " " . map (\(s,t)-> s ++ ":" ++ (show t) ) $ zip ss ts

instance Show ZTree where
  show zt = "\n----------------------------\n\n"++
            ( foldl f (" { " ++ (show $ current zt) ++ " } ") (dads zt) ) ++ "\n" ++ 
            "\n zTreeID     : " ++ show (zTreeID zt)++
            "\n numSteps    : " ++ show (numSteps zt)++
            "\n numUnsolved : " ++ show (numUnsolved zt)++
            "\n nextVar     : " ++ show (nextVar zt) ++
            "\n depth       : " ++ show (depth zt) ++
            "\nlocals:\n" ++ showTable (locals  zt) ++
            "globals:\n"  ++ showTable (globals zt) 
   where 
    f :: String -> DTree -> String
    f str dt = case dt of
      DTreeApp ts1 s _ ts2 -> "(" ++ s ++ " " ++ (fillSpaces (reverse ts1)) ++ str ++ (fillSpaces ts2) ++ ")"
      DTreeLam ss _        -> "(\\ " ++ (intercalate " " ss) ++ " . " ++ str ++ ")" 

showTable :: Table -> String
showTable table = concatMap f (Map.toAscList table)
 where
  f :: (SymbolOfAtomicType,Set Entry) -> String
  f (alpha,entrySet) ="-> "++ alpha ++ "\n   " ++ (intercalate "\n   " . map show $ (Set.toAscList entrySet))  ++ "\n" 
  

fillSpaces :: Show a => [a] -> String  
fillSpaces = intercalate " " . map show 



-- examples/tests : -----------

h0' = mkZTree $ defaultSearchOptions 100 type_head ctx_head 
h0  = Just h0'
h1  = h0 >>= step >>= return . head 
h2  = h1 >>= step >>= return . last 
h3  = h2 >>= step >>= return . head
h4  = h3 >>= step >>= return . head 
h5  = h4 >>= step >>= return . head
h6  = h5 >>= step >>= return . head . tail
h7  = h6 >>= step >>= return . head 
h8  = h7 >>= step

hs = map fromJust [h0,h1,h2,h3,h4,h5,h6,h7] 

t0'  = TreeTyp (Typ "A")
t1'  = expandApp' ("f", [Typ "A",Typ "B"], "C")  -- ==> (f A B)
t2'  = expandLam' 0   ([Typ "A",Typ "B"],"C")  -- ==> (\ x0 x1 . C )


t_1 = TreeLam ["x","y"] (Typ "A" :-> Typ "B" :-> Typ "C") 
      ( TreeApp "f" "C" [ 
        ( TreeApp "x" "A" [] ) , 
        ( TreeApp "y" "B" [] ) ] )

t_2 = TreeLam ["x","y"] (Typ "A" :-> Typ "B" :-> Typ "C") 
      ( TreeApp "f" "C" [ 
        ( TreeTyp (Typ "A") ) , 
        ( TreeApp "y" "B" [] ) ] )

--t1 = mkZTree ctx_head t_2


int :: Typ
int   = Typ "Int"
m_int = Typ "MaybeInt"
l_int = Typ "[Int]"
int1 = int :-> int
int2 = int :-> int :-> int

type_head :: Typ
type_head = l_int :-> m_int

ctx_head :: Context
ctx_head = [  ( "listCase" , l_int :-> m_int :-> (int:->l_int:->m_int) :-> m_int ),
              ( "Nothing"  , m_int ),
              ( "Just"     , int :-> m_int ) ]


bool   = Typ "Bool"
l_bool = Typ "[Bool]"

ctx_yu = [
 ( "(&&)"  , bool :-> bool :-> bool                            ),
 ( "(||)"  , bool :-> bool :-> bool                            ),
 ( "nand"  , bool :-> bool :-> bool                            ),
 ( "nor"   , bool :-> bool :-> bool                            ),
 ( "foldr" , (bool:->bool:->bool) :-> bool :-> l_bool :-> bool ),
 ( "head_" , l_bool :-> bool                                   ),
 ( "tail_" , l_bool :-> l_bool                                 )]




ctx_map :: Context
ctx_map = [
  ( "foldr" , (int:->l_int:->l_int) :-> l_int :-> l_int :-> l_int ),
  ( "(:)"   , int  :-> l_int :-> l_int         ),
  ( "[]"    , l_int                            )
 ]

ctx_big = ctx_head ++ ctx_map

type_map :: Typ
type_map = int1 :-> l_int :-> l_int



dou, dou1, dou2 :: Typ
dou  = Typ "Double"
dou1 = dou :-> dou
dou2 = dou :-> dou :-> dou

ctx_ttSSR :: Context
ctx_ttSSR = [("(+)",dou2),("(-)",dou2),("(*)",dou2),("rdiv",dou2),("sin",dou1),("cos",dou1),("exp",dou1),("rlog",dou1)]






testSO :: (StdGen->SearchOptions) -> IO ()
testSO soFun = do
  stdGen <- newStdGen
  let trees = proveWith2 (soFun stdGen)
  mapM_ (putStrLn . show) trees
  putStrLn $ "num terms : " ++ (show $ length trees)

testSO2 :: (StdGen->SearchOptions) -> IO ()
testSO2 soFun = do
  stdGen <- newStdGen
  let ctts = prove (soFun stdGen)
  mapM_ (putStrLn . show) ctts
  putStrLn $ "num terms : " ++ (show $ length ctts)

test_ep = testSO2 $ \ stdGen -> SearchOptions {
    so_n                  = 500             ,
    so_typ                = l_bool :-> bool ,
    so_ctx                = ctx_yu          ,
    so_stdGen             = stdGen          ,
    so_runLen             = Nothing         ,
    so_randomRunState     = NoRandomRunState , 
    so_edgeSelectionModel = ContinueProbGeomWithDepth 0.75  --0.9  --AllEdges  
  }

test_head = testSO2 $ \ stdGen -> SearchOptions {
    so_n                  = 500       ,
    so_typ                = type_head ,
    so_ctx                = ctx_head  ,
    so_stdGen             = stdGen    ,
    so_runLen             = Nothing   ,
    so_randomRunState     = NoRandomRunState , 
    so_edgeSelectionModel = ContinueProbGeomWithDepth 0.75  --0.9  --AllEdges  
  }

-- \ x0 -> listCase x0 (listCase x0 Nothing (s (s (k s) (s (k (s (s (k listCase) i))) (s (k k) (s (k Ju
-- st) i)))) (k (k (k (k (listCase x0 Nothing (k (k Nothing))))))))) (k (k Nothing))

test_map = testSO2 $ \ stdGen -> SearchOptions {
    so_n                  = 500       ,
    so_typ                = type_map ,
    so_ctx                = ctx_map  ,
    so_stdGen             = stdGen    ,
    so_runLen             = Nothing   ,
    so_randomRunState     = NoRandomRunState , 
    so_edgeSelectionModel = ContinueProbGeomWithDepth 0.85
  }

test_ssr = testSO2 $ \ stdGen -> SearchOptions {
    so_n                  = 500       ,
    so_typ                = dou1 ,
    so_ctx                = ctx_ttSSR  ,
    so_stdGen             = stdGen    ,
    so_runLen             = Nothing   ,
    so_randomRunState     = NoRandomRunState ,
    so_edgeSelectionModel = ContinueProbGeomWithDepth 0.6
  } 

test_big = testSO $ \ stdGen -> SearchOptions {
    so_n                  = 500       ,
    so_typ                = type_map ,
    so_ctx                = ctx_big  ,
    so_stdGen             = stdGen    ,
    so_runLen             = Nothing   ,
    so_randomRunState     = NoRandomRunState , 
    so_edgeSelectionModel = ContinueProbGeomWithDepth 0.7  --AllEdges  
  }

test_head_2 = testSO2 $ \ stdGen -> SearchOptions {
    so_n                  = 500       ,
    so_typ                = type_head ,
    so_ctx                = ctx_head  ,
    so_stdGen             = stdGen    ,
    so_runLen             = Just 1    ,  --Nothing   ,
    so_randomRunState     = NoRandomRunState,
    so_edgeSelectionModel = OneRandomEdgeWithPedicat (const True)  --AllEdges  , 
  } 

test_head_koza = testSO2 $ \ stdGen -> SearchOptions {
    so_n                  = 500       ,
    so_typ                = type_head ,
    so_ctx                = ctx_head  ,
    so_stdGen             = stdGen    ,
    so_runLen             = Just 1    ,  --Nothing   ,
    so_randomRunState     = KozaRandomRunState Nothing Nothing ,
    so_edgeSelectionModel = KozaESM
  } 

test_map_koza = testSO $ \ stdGen -> SearchOptions {
    so_n                  = 500       ,
    so_typ                = type_map ,
    so_ctx                = ctx_map  ,
    so_stdGen             = stdGen    ,
    so_runLen             = Just 1    ,  --Nothing   ,
    so_randomRunState     = KozaRandomRunState Nothing Nothing ,
    so_edgeSelectionModel = KozaESM
  }


test_ssr_koza = testSO2 $ \ stdGen -> SearchOptions {
    so_n                  = 500       ,
    so_typ                = dou1 ,
    so_ctx                = ctx_ttSSR  ,
    so_stdGen             = stdGen    ,
    so_runLen             = Just 1    ,  --Nothing   ,
    so_randomRunState     = KozaRandomRunState Nothing Nothing ,
    so_edgeSelectionModel = KozaESM
  } 
