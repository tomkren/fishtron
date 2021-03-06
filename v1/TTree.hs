module TTree 
( CTT (..),
  TTree (..),
  TTPos,
  mkCTT,
  ttreeSubtree,
  ttreeSubtrees,
  ttreeChangeSubtree,
  ttreeDepth,
  ttreePoses2
) where

import Data.List
import Data.Either

import TTerm

data CTT = CTT Context TTree 

data TTree = TTree Symbol Typ [TTree] 

type TTPos = [Int]

instance Show CTT where
 show (CTT ctx ttree) = case ctx of
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



ttreeDepth :: TTree -> Int
ttreeDepth (TTree _ _ ts) = case ts of
 [] -> 0
 _  -> (1+) . maximum $ map ttreeDepth ts


ttreeSubtree :: TTree -> TTPos -> TTree
ttreeSubtree t [] = t
ttreeSubtree (TTree _ _ ts) (i:is) = 
 ttreeSubtree (ts !! (i-1)) is

ttreeSubtrees :: TTree -> [TTree]
ttreeSubtrees t@(TTree _ _ ts) = t : concatMap ttreeSubtrees ts

ttreeChangeSubtree :: TTree -> TTPos -> TTree -> (TTree,TTree)
ttreeChangeSubtree tree               []     newSub = (newSub,tree) 
ttreeChangeSubtree (TTree str typ ts) (i:is) newSub = 
 let (ts1,subt:ts2) = splitAt (i-1) ts
     (subt',oldSub) = ttreeChangeSubtree subt is newSub
  in (TTree str typ (ts1 ++ (subt':ts2) ) ,oldSub)  

ttreePoses2 :: TTree -> ([TTPos],[TTPos])
ttreePoses2 t = 
  let xs  = poses2 [] t 
      rev = map reverse 
   in ( rev . lefts $ xs , rev . rights $ xs )
 where
  poses2 :: [Int] -> TTree -> [ Either [Int] [Int] ]
  poses2 pos (TTree _ _ []) = [Left pos]
  poses2 pos (TTree _ _ ts) = 
   (Right pos) : (concatMap (\(i,t)-> poses2 (i:pos) t ) (zip [1..] ts) )



mkCTT :: TTerm -> CTT
mkCTT tt = 
 let (ctx,ttree) = mkCTT' [] tt
  in CTT ctx ttree

mkCTT' :: [Symbol] -> TTerm -> ( Context , TTree ) 
mkCTT' xs tt = case tt of
 TLam x m (tx:->_) -> 
  let ( rest , ret ) = mkCTT' (x:xs) m
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



