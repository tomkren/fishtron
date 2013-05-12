module PolyUtils ( Substi , match , applySubsti ) where

-- import Data.List 
-- import Data.Maybe
-- import qualified Data.Set as Set
-- import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)


import TTerm (Typ(..),Symbol)


type Substi = Map Symbol Typ

applySubsti :: Substi -> Typ -> Typ
applySubsti s typ = case typ of
 Typ    _    -> typ
 TypVar x    -> case Map.lookup x s of
                 Nothing -> typ
                 Just t  -> t
 TypFun f ts -> TypFun f (map (applySubsti s) ts)
 a :-> b     -> (applySubsti s a) :-> (applySubsti s b)


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

