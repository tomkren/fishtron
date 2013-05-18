{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, IncoherentInstances #-}

module PolyUtils 
( Substi 
, mgu 
, applySubsti 
, applySubstiWithLog 
, composeSubsti 
, emptySubsti 
, match 
, Table, Entry(..), TypHead, Edge
, emptyTable, ctxToTable , addToTable , subFromTable, showTable
, typeArgz
) where

import Data.List 
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)




import TTerm (Typ(..),Symbol,Context)


-- ukazka rozdílu mezi "globals" a "locals" a co na to let
kons :: a -> b -> a
kons x y = x

test1 = kons 'a' (kons "a" "b")
--test2 = (\ f -> f 'a' (f "a" "b") ) kons 
test3 = let f = kons in f 'a' (f "a" "b")


-----------------------------------------------

typeArgz :: Typ -> ([Typ],Typ)
typeArgz typ = case typ of
 (a :-> b) -> let (  as, alpha) = typeArgz b
               in (a:as, alpha)
 typHead   -> ([],typHead)



------------------------------------------------- 

type Substi = Map Symbol Typ

emptySubsti :: Substi
emptySubsti = Map.empty

applySubsti :: Substi -> Typ -> Typ
applySubsti s typ 
 | Map.null s = typ
 | otherwise  = case typ of
  Typ    _    -> typ
  TypVar x    -> case Map.lookup x s of
                  Nothing -> typ
                  Just t  -> t
  TypFun f ts -> TypFun f (map (applySubsti s) ts)
  a :-> b     -> (applySubsti s a) :-> (applySubsti s b)

-- in addition it returns sub-substitution - the part which was used 
applySubstiWithLog :: Substi -> Typ -> (Typ,Substi)
applySubstiWithLog s typ 
  | Map.null s = (typ,emptySubsti)
  | otherwise  = aswl emptySubsti s typ
 where  
  aswl :: Substi -> Substi -> Typ -> (Typ,Substi) 
  aswl acc s typ = case typ of
    Typ _       -> (typ,acc)
    
    TypVar x    -> case Map.lookup x s of
                    Nothing -> (typ, acc )
                    Just t  -> (t  , Map.insert x t acc )
  
    a :-> b     -> let (a',acc')  = aswl acc  s a
                       (b',acc'') = aswl acc' s b
                    in ( a' :-> b' , acc'' )
  
    TypFun f ts -> let (ts',acc') = foldr (\ t (xs,ac) -> let (x,ac') = aswl ac s t in (x:xs,ac')  ) ([],acc) ts
                    in ( TypFun f ts' , acc')            


freshenTypVars :: Int -> Typ -> (Typ,(Int,Substi))
freshenTypVars nextVar typ = freshenTypVars' (nextVar,emptySubsti) typ

freshenTypVars' :: (Int,Substi) -> Typ -> (Typ,(Int,Substi))
freshenTypVars' acc@(nextVar,s) typ = case typ of
    Typ _       -> (typ,acc)
    
    TypVar x    -> case Map.lookup x s of
                    Just t  -> ( t , acc )
                    Nothing -> let newTyp = TypVar $ '*' : show nextVar
                                in ( newTyp  , (nextVar+1,Map.insert x newTyp s) )
  
    a :-> b     -> let (a',acc' ) = freshenTypVars' acc  a
                       (b',acc'') = freshenTypVars' acc' b
                    in ( a' :-> b' , acc'' )
  
    TypFun f ts -> let (ts',acc') = foldl (\ (xs,ac) t -> let (x,ac') = freshenTypVars' ac t in (x:xs,ac')  ) ([],acc) ts
                    in ( TypFun f (reverse ts') , acc')  

composeSubsti :: Substi -> Substi -> Substi
composeSubsti new old = 
 let old' = fmap (applySubsti new) old
  in Map.filterWithKey (\ x t -> t /= TypVar x ) $ Map.union old' new 


s1 = Map.fromList [ ("x",a_:->b_),("k",m_) ]

s2 = Map.fromList [ ("a",TypFun "F" [i_,j_,k_] ) , ("m",k_) , ("k",i_) ]


j_ = TypVar "j"
i_ = TypVar "i"
e_ = TypVar "e"
g_ = TypVar "g"
k_ = TypVar "k"
m_ = TypVar "m"

typ1 = ((a_:->a_):->a_)
typ2 = ((((j_:->k_):->i_):->((e_:->e_):->(g_:->k_))):->m_)

typ3 = TypFun "f" [ ((j_:->k_):->i_) , ((e_:->e_):->(g_:->k_)) , m_ ]


testik = mgu typ1 typ2

mgu :: Typ -> Typ -> Maybe Substi
mgu t1 t2 = mgu' (Map.empty) [(t1,t2)]
 where

  mgu' :: Substi -> [(Typ,Typ)] -> Maybe Substi
  mgu' s todo = case todo of
    [] -> Just s
    (TypFun f1 ts1,TypFun f2 ts2) : rest
     | f1 == f2 && length ts1 == length ts2 -> mgu' s $ (zip ts1 ts2) ++ rest
     | otherwise -> Nothing
    (a1:->b1,a2:->b2) : rest -> mgu' s $ (a1,a2):(b1,b2):rest
    (Typ s1,Typ s2) : rest
     | s1 == s2  -> mgu' s rest
     | otherwise -> Nothing
    (TypVar x1, t@(TypVar x2) ) : rest 
     | x1 == x2  -> mgu' s rest
     | otherwise -> bingo (x1,t) s rest
    (TypVar x , t ) : rest 
     | x `elem` vars t -> Nothing -- inifinite type
     | otherwise -> bingo (x,t) s rest
    ( t , v@(TypVar _) ) : rest -> mgu' s ((v,t):rest)
    _ -> Nothing
  
  bingo :: (Symbol,Typ) -> Substi -> [(Typ,Typ)] -> Maybe Substi
  bingo p@(x,t) s rest = mgu' ( Map.insert x t $ fmap (update p) s ) ( map (\(t1,t2)->(update p t1,update p t2)) rest ) 
  
  update :: (Symbol,Typ) -> Typ -> Typ
  update p@(x,t) typ = case typ of
   Typ _        -> typ
   TypVar y     
    | x == y    -> t
    | otherwise -> typ
   a :-> b      -> (update p a) :-> (update p b) 
   TypFun f ts  -> TypFun f (map (update p) ts)

  vars :: Typ -> [Symbol]
  vars typ = case typ of
   Typ _       -> []
   TypVar x    -> [x]
   a :-> b     -> vars a ++ vars b
   TypFun _ ts -> concatMap vars ts


{- 

data Typ  = 
 Typ Symbol          |
 TypVar Symbol       | 
 TypFun Symbol [Typ] |
 Typ :-> Typ 

-}

type HeadTyp    = Typ
type CurrentTyp = Typ


match :: CurrentTyp -> HeadTyp -> Maybe Substi
match = match' (Map.empty)




match' :: Substi -> CurrentTyp -> HeadTyp -> Maybe Substi
match' s (Typ    a)    (Typ    x)    = if a == x then Just s                      else Nothing
match' s (TypFun f as) (TypFun g xs) = if f == g then matchList s as      xs      else Nothing
match' s (a1:->a2)     (x1:->x2)     =                matchList s [a1,a2] [x1,x2] 
match' s c             (TypVar x)    = case Map.lookup x s of
                                        Nothing -> Just $ Map.insert x c s
                                        Just t  -> if t == c then Just s else Nothing
match' _ _ _ = Nothing


matchList :: Substi -> [CurrentTyp] -> [HeadTyp] -> Maybe Substi
matchList s as xs | length as == length xs = matchList' s as xs
                  | otherwise              = Nothing 

matchList' :: Substi -> [CurrentTyp] -> [HeadTyp] -> Maybe Substi
matchList' s []     []     = Just s
matchList' s (a:as) (x:xs) = do
  s' <- match' s  a  x
  matchList'   s' as xs



-------------------------------------------------------------------------------

type Edge = (Symbol,[Typ],TypHead)


type NextVar = Int
type GlobalsTable = Table
type LocalsTable  = Table

type TrampSubsti = Substi
type MGU         = Substi


updateByMGU :: MGU -> (TrampSubsti , LocalsTable) -> (TrampSubsti , LocalsTable)
updateByMGU lastMgu (trampSubsti,locals) =
  let trampSubsti' = composeSubsti lastMgu trampSubsti
      locals'      = applySubstiOnTable lastMgu locals
   in ( trampSubsti' , locals' )


getEdges_all :: NextVar -> CurrentTyp -> GlobalsTable -> LocalsTable -> ( [( Edge , MGU )]  , NextVar )
getEdges_all nextVar currentTyp globals locals =
  let (esAndMgus1,nextVar') = getEdges_globals nextVar currentTyp globals
      esAndMgus2            = getEdges_locals          currentTyp locals
   in ( esAndMgus1 ++ esAndMgus2 , nextVar' )    

getEdges_globals :: NextVar -> CurrentTyp -> GlobalsTable -> ( [( Edge , MGU )]  , NextVar )
getEdges_globals nextVar currentTyp table = 
  let (table',nextVar') = freshenTable( table ,nextVar )
   in ( getEdges_locals currentTyp table' , nextVar' )


getEdges_locals :: CurrentTyp -> LocalsTable -> [( Edge , MGU )] 
getEdges_locals currentTyp table = 
  concatMap transformIt $ filterTable table currentTyp


transformIt :: ( HeadTyp , Set Entry , MGU ) -> [( Edge , MGU )]
transformIt ( headTyp , entrySet , substi ) = 
  map ( \ (Entry ts sym) -> ( (sym , map (applySubsti substi) ts , headTyp ),substi) ) (Set.toList entrySet)


filterTable :: Table -> CurrentTyp -> [( HeadTyp , Set Entry , MGU )] -- NEFEKTIVNI !!!
filterTable table currentTyp = 
  map (\(headTyp,entrySet,Just substi)->(headTyp,entrySet,substi)) 
  $ filter (\(_,_,m_substi)-> isJust m_substi) 
  $ map ( \(headTyp ,entrySet) -> ( headTyp , entrySet , mgu currentTyp headTyp ) ) (Map.toList table)





-- freshenTypVars   :: Int         -> Typ -> (Typ,(Int,Substi))
-- freshenTypVars' :: (Int,Substi) -> Typ -> (Typ,(Int,Substi))

freshenTable :: (GlobalsTable,NextVar) -> (GlobalsTable,NextVar)   -- NEEFEKTIVNI už tuplem
freshenTable (table,nextVar) = Map.foldrWithKey f (emptyTable,nextVar) table
 where
  f :: Typ -> Set Entry -> (Table,NextVar) -> (Table,NextVar)
  f key set (accTab,nextVar') = 
    let (key',mezi)     = freshenTypVars nextVar key
        (setoid',maxes) = unzip $ map (\(Entry ts sym)-> let (ts',mezis) = unzip $ map (freshenTypVars' mezi) ts
                                                             maxNext     = maximum' nextVar' $ map fst mezis 
                                                          in (Entry ts' sym , maxNext ) ) 
                                      (Set.toList set)

        set'            = Set.fromList setoid'
        maxMaxNext      = maximum' nextVar' maxes   
        g :: Maybe (Set Entry) -> Maybe (Set Entry)
        g Nothing     = Just set'
        g (Just set2) = Just $ Set.union set2 set'
     in ( Map.alter g key' accTab , max maxMaxNext nextVar' ) 

maximum' :: Ord a => a -> [a] -> a
maximum' defa [] = defa
maximum' _    xs = maximum xs  

-------------------------------------------------------------------------------
--- Table stuff ---------------------------------------------------------------
-------------------------------------------------------------------------------

type Table = Map TypHead (Set Entry)
data Entry = Entry [Typ] Symbol deriving (Eq,Ord)

type TypHead = Typ -- asser: nemelo by bejt (a:->b)

--------------------------------


emptyTable :: Table
emptyTable = Map.empty

addToTableWith :: (Set Entry->Set Entry->Set Entry) -> Table -> Context -> Table
addToTableWith op table ctx = foldr (f op) table ctx
 where
  f :: (Set Entry->Set Entry->Set Entry)-> (Symbol,Typ) -> Table -> Table
  f op      (sym,typ) acc = 
    let (ts,typHead) = typeArgz typ
     in Map.insertWith op typHead (Set.singleton $ Entry ts sym) acc 

ctxToTable :: Context -> Table
ctxToTable ctx = addToTableWith Set.union emptyTable ctx

addToTable   :: Table -> [Symbol] -> Typ -> Table
addToTable   table ss typ = addToTableWith Set.union table (zip ss (fst $ typeArgz typ))

subFromTable_bug :: Table -> [Symbol] -> Typ -> Table
subFromTable_bug table ss typ = addToTableWith (Set.\\)  table (zip ss (fst $ typeArgz typ))

subFromTable :: Table -> [Symbol] -> Typ -> Table
subFromTable table ss typ = addToTableWith (flip (Set.\\))  table (zip ss (fst $ typeArgz typ))


----

showTable :: Table -> String
showTable table = concatMap f (Map.toAscList table)
 where
  f :: (TypHead,Set Entry) -> String
  f (typHead,entrySet) = 
    "-> "++ (show typHead) ++ 
    "\n   " ++ (intercalate "\n   " . map show $ (Set.toAscList entrySet))  ++ "\n" 

instance Show Entry where show (Entry ts sym) = show ts ++ "->    ... " ++ sym
instance Show Table where show = showTable



--------------------------------




applySubstiOnTable :: Substi -> Table -> Table
applySubstiOnTable s table = 
  Map.map (applySubstiOnEntrySet s) . mapKeys' s $ table 


mapKeys' :: Substi -> Table -> Table
mapKeys' s table = Map.foldrWithKey f emptyTable table
 where
  f :: Typ -> Set Entry  -> Table -> Table
  f key set accTab = 
    let key' = applySubsti s key
        g :: Maybe (Set Entry) -> Maybe (Set Entry)
        g Nothing     = Just   set
        g (Just set2) = Just $ Set.union set2 set
     in Map.alter g key' accTab
  

applySubstiOnEntrySet :: Substi -> Set Entry -> Set Entry
applySubstiOnEntrySet s set = 
  Set.map (\(Entry ts sym) -> Entry (map (applySubsti s) ts) sym ) set

-- test


int   = Typ "Int"
int1 = int :-> int
int2 = int :-> int :-> int

a_ = TypVar "a"
b_ = TypVar "b"

m_a   = TypFun "Maybe" [a_]
l_a   = TypFun "List"  [a_]
m_Int = TypFun "Maybe" [int]
l_Int = TypFun "List"  [int]

ctx_poly_head :: Context
ctx_poly_head = 
  [ ( "listCase" , l_a :-> b_ :-> (a_:->l_a:->b_) :-> b_ )
  , ( "Nothing"  , m_a )--m_a )
  , ( "Just"     , a_ :-> m_a ) ]


sub0 = Map.fromList [ ("a", int ) ]

tab0 = ctxToTable 
 [ ( "x0" , l_Int                                 )
 , ( "x1" , a_                                    )
 , ( "x2" , l_a                                   )
 , ( "f"  , l_a :-> b_ :-> (a_:->l_a:->b_) :-> b_ ) 
 , ( "g"  , a_ :-> m_a                            )
 , ( "y0" , m_a                                   )]

tab0B = Map.fromList 
 [ ( l_Int , Set.fromList $ [Entry [] "x0"]  )
 , ( a_    , Set.fromList $ [Entry [] "x1"]  )
 , ( l_a   , Set.fromList $ [Entry [] "x2"]  )
 ]


