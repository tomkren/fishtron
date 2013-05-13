module PolyUtils ( Substi , mgu , applySubsti , composeSubsti , match ) where

-- import Data.List 
-- import Data.Maybe
-- import qualified Data.Set as Set
-- import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)


import TTerm (Typ(..),Symbol)


type Substi = Map Symbol Typ

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

composeSubsti :: Substi -> Substi -> Substi
composeSubsti new old = 
 let old' = fmap (applySubsti new) old
  in Map.filterWithKey (\ x t -> t /= TypVar x ) $ Map.union old' new 


s1 = Map.fromList [ ("x",a_:->b_),("k",m_) ]

s2 = Map.fromList [ ("a",TypFun "F" [i_,j_,k_] ) , ("m",k_) , ("k",i_) ]


a_ = TypVar "a"
b_ = TypVar "b"
j_ = TypVar "j"
i_ = TypVar "i"
e_ = TypVar "e"
g_ = TypVar "g"
k_ = TypVar "k"
m_ = TypVar "m"

testik = mgu ((a_:->a_):->a_) ((((j_:->k_):->i_):->((e_:->e_):->(g_:->k_))):->m_)

mgu :: Typ -> Typ -> Maybe Substi
mgu t1 t2 = mgu' (Map.empty) [(t1,t2)]
 where

  mgu' :: Substi -> [(Typ,Typ)] -> Maybe Substi
  mgu' s todo = case todo of
    [] -> Just s
    (TypFun f1 ts1,TypFun f2 ts2) : rest
     | f1 == f2  -> mgu' s $ (zip ts1 ts2) ++ rest
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

a = TypVar "a"
b = TypVar "b"
x = TypVar "x"
y = TypVar "y"

