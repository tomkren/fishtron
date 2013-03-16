-- nový pokus o Inhabitation Machines bez potřeby generovat graf dopředu a vůbec to udělat pořádně

module IM where

import TTerm (Symbol,Typ(..),Context,typeArgs)

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)


type SymbolTable = Map Typ (Set Symbol)
type LocalVars = SymbolTable


type Table = Map SymbolOfAtomicType (Set Entry)
data Entry = Entry [Typ] Symbol deriving (Eq,Ord)

--instance Eq   Entry where (Entry s1 _) == (Entry s2 _) = s1 == s2
--instance Ord  Entry where (Entry s1 _) <= (Entry s2 _) = s1 <= s2
instance Show Entry where show (Entry ts sym) = sym ++ " ... " ++ show ts

data Tree = TreeApp  Symbol  SymbolOfAtomicType [Tree] 
          | TreeLam [Symbol] Typ  Tree
          | TreeTyp Typ

data ZTree = ZTree { 
  current :: Tree , 
  dads    :: [ DTree ]  ,

  locals  :: Table ,
  globals :: Table ,

  depth       :: Int ,
  nextVar     :: Int ,
  numUnsolved :: Int
 }


data DTree = DTreeApp [Tree] Symbol SymbolOfAtomicType [Tree]
           | DTreeLam [Symbol] Typ


type SymbolOfAtomicType = Symbol


emptyTable :: Table
emptyTable = Map.empty

addToTableWith :: (Set Entry->Set Entry->Set Entry) -> Table -> Context -> Table
addToTableWith op table ctx = foldr f table ctx
 where
  f :: (Symbol,Typ) -> Table -> Table
  f (sym,typ) acc = 
    let (ts,alpha) = typeArgs typ
     in Map.insertWith op alpha (Set.singleton $ Entry ts sym) acc 

ctxToTable :: Context -> Table
ctxToTable ctx = addToTableWith Set.union emptyTable ctx

addToTable   :: Table -> [Symbol] -> Typ -> Table
addToTable   table ss typ = addToTableWith Set.union table (zip ss (fst $ typeArgs typ))

subFromTable :: Table -> [Symbol] -> Typ -> Table
subFromTable table ss typ = addToTableWith (Set.\\)  table (zip ss (fst $ typeArgs typ))





emptyST :: SymbolTable
emptyST = Map.empty

addToSTWith :: (Set Symbol->Set Symbol->Set Symbol) -> SymbolTable -> Context -> SymbolTable
addToSTWith op z ctx = foldr (\(s,t) acc -> Map.insertWith op t (Set.singleton s) acc ) z ctx

ctxToST :: Context -> SymbolTable
ctxToST ctx = addToSTWith Set.union emptyST ctx

addToLocalVars :: LocalVars -> [Symbol] -> Typ -> LocalVars
addToLocalVars localVars ss typ = addToSTWith Set.union localVars (zip ss (fst $ typeArgs typ))

subFromLocalVars :: LocalVars -> [Symbol] -> Typ -> LocalVars
subFromLocalVars localVars ss typ = addToSTWith (Set.\\) localVars (zip ss (fst $ typeArgs typ))


step :: ZTree -> Maybe [ZTree]
step zt = nextTreeTypNode zt >>= return . expand

expand :: ZTree -> [ZTree]
expand zt = case current zt of
  TreeTyp typ -> case typ of
    Typ alpha -> map (expandApp zt) (getEdges zt)
    _         -> [expandLam typ zt] 
  _ -> error "expand : Only type-node can be expanded !"


getEdges :: ZTree -> [ (Symbol,[Typ],SymbolOfAtomicType) ]
getEdges zt = case current zt of
  TreeTyp (Typ alpha) -> (getEdges' (locals zt) alpha ) ++ (getEdges' (globals zt) alpha ) 
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
  f typ1 (ss,typ2,i) = ( ('x' : show i) : ss , typ1 :-> typ2 , i-1 )




mkZTree :: Context -> Typ -> ZTree
mkZTree ctx typ = ZTree        { 
  current     = TreeTyp typ    , 
  dads        = []             ,  
  locals      = emptyTable     , 
  globals     = ctxToTable ctx , 
  depth       = 0              , 
  nextVar     = 0              ,
  numUnsolved = 1
 } 



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





h0 = Just $ mkZTree ctx_head $ l_int :-> m_int
h1 = h0 >>= step >>= return . head 
h2 = h1 >>= step >>= return . last 
h3 = h2 >>= step >>= return . head
h4 = h3 >>= step >>= return . head 
h5 = h4 >>= step >>= return . head
h6 = h5 >>= step >>= return . head . tail
h7 = h6 >>= step >>= return . head 
h8 = h7 >>= step

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


ctx_head :: Context
ctx_head = [  ( "listCase" , l_int :-> m_int :-> (int:->l_int:->m_int) :-> m_int ),
              ( "Nothing"  , m_int ),
              ( "Just"     , int :-> m_int ) ]



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
            "\nnumUnsolved : " ++ show (numUnsolved zt)++
            "\nnextVar     : " ++ show (nextVar zt) ++
            "\ndepth       : " ++ show (depth zt) ++
            "\nlocals:\n" ++ showTable (locals  zt) ++
            "globals:\n"  ++ showTable (globals zt) ++
            "-----------------------------\n"
   where 
    f :: String -> DTree -> String
    f str dt = case dt of
      DTreeApp ts1 s _ ts2 -> "(" ++ s ++ " " ++ (fillSpaces ts1) ++ str ++ (fillSpaces ts2) ++ ")"
      DTreeLam ss _        -> "(\\ " ++ (intercalate " " ss) ++ " . " ++ str ++ ")" 

showTable :: Table -> String
showTable table = concatMap f (Map.toAscList table)
 where
  f :: (SymbolOfAtomicType,Set Entry) -> String
  f (alpha,entrySet) ="-> "++ alpha ++ "\n   " ++ (intercalate "\n   " . map show $ (Set.toAscList entrySet))  ++ "\n" 
  
showSymbolTable :: SymbolTable -> String
showSymbolTable localVars = concatMap f (Map.toAscList localVars)
 where
  f :: (Typ,Set Symbol) -> String
  f (typ,symSet) = show typ ++ "  :  " ++ (intercalate " " (Set.toAscList symSet))  ++ "\n" 

fillSpaces :: Show a => [a] -> String  
fillSpaces = intercalate " " . map show 



-- probably redundant : -----------

