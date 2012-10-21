module TTree 
( CTree (..),
  TTree (..),
  mkCTree
) where

import Data.List

import TTerm

data CTree = CTree Context TTree 

data TTree = TTree Symbol Typ [TTree] 

instance Show CTree where
 show (CTree ctx ttree) = case ctx of
  [] -> show ttree
  _  -> let vars = intercalate " " . map fst $ ctx
         in "\\ "++ vars ++ " -> " ++ show ttree 

instance Show TTree where
 show ttree = case showPars ttree of
   '(' : str -> init str
   str       -> str  
  where  
   showPars (TTree x _ ts) = case ts of
    [] -> x
    _  -> "(" ++ x ++ " " ++ (intercalate " " (map showPars ts)) ++ ")"


mkCTree :: TTerm -> CTree
mkCTree tt = 
 let (ctx,ttree) = mkCTree' [] tt
  in CTree ctx ttree

mkCTree' :: [Symbol] -> TTerm -> ( Context , TTree ) 
mkCTree' xs tt = case tt of
 TLam x m (tx:->_) -> 
  let ( rest , ret ) = mkCTree' (x:xs) m
   in ( (x,tx) : rest , ret ) 
 _ -> ( [] , fromTTerm . toSki . changeVarsToVals xs $ tt )  

changeVarsToVals :: [Symbol] -> TTerm -> TTerm
changeVarsToVals xs tt = case tt of
 TVar x   t | x `elem` xs -> TVal x t
            | otherwise   -> TVar x t 
 TVal v   t -> TVar v t
 TLam x m t -> TLam x (changeVarsToVals (xs \\ [x]) m ) t
 TApp m n t -> TApp (changeVarsToVals xs m) (changeVarsToVals xs n) t
 

fromTTerm :: TTerm -> TTree
fromTTerm = reverseTs . fromTTerm'
 where reverseTs (TTree x t ts) = TTree x t ( reverse . map reverseTs $ ts ) 

fromTTerm' :: TTerm -> TTree
fromTTerm' tt = case tt of
 TLam _ _ _ -> error "tterm must be without lambdas"
 TVar x   t -> TTree x t []
 TVal v   t -> TTree v t []
 TApp m n t -> 
  let TTree f _ trees = fromTTerm' m 
   in TTree f t ( (fromTTerm' n) : trees  ) 



i = Typ "I"

a = TVal "a" (i:->i:->i:->i)
b = TVal "b" (i:->i:->i)
c = TVal "c" i
d = TVal "d" i
e = TVal "e" (i:->i)
f = TVal "f" i
g = TVal "g" i

tt1 = TApp (TApp (TApp a (TApp (TApp b c (i:->i)) d i) (i:->i:->i)) (TApp e f i) (i:->i)) g i



