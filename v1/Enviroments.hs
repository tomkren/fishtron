module Enviroments where

import Base
import Parser
import Evaluation

import Data.Maybe
import Data.Dynamic

-- enviroment ------------------------------------------------------------------

int  = Typ "Int"
real = Typ "Real"
bool = Typ "Bool"
listInt = Typ "[Int]"

toDyn' :: (Typeable a) => a -> Comb
toDyn' = HaskComb . toDyn

mkComb env str = Comb $ fromJust $ parseTTerm env str

env4 = [
         ( "[]"    , listInt                                 , toDyn' ([]::[Int]) ) 
         -- když tam chybí [] tak se TC4 rozbije, zjistit proè
         ,(":"     , int :-> listInt :-> listInt             , ddotInt           )
         ,("+"     , int :-> int :-> int                     , plusFun           )
         ,("0"     , int                                     , toDyn' (0::Int)    )
         ,("foldr", (int:->int:->int):->int:->listInt:->int , foldrComb )    
       ]


env3 = [
         ( "[]"    , listInt                                 , toDyn' ([]::[Int]) )
         ,(":"     , int :-> listInt :-> listInt             , ddotInt           )
         ,("inc"   , int :-> int                             , incComb        )
        -- ,("foldr" , (int:->int:->int):->int:->listInt:->int , foldrInt          )
         ,("+"     , int :-> int :-> int                     , plusFun           )
         ,("0"     , int                                     , toDyn' (0::Int)    )
         ,("1"     , int                                     , toDyn' (1::Int)    )
         ,("3"     , int                                     , toDyn' (3::Int)    )
         ,("if" , bool :-> int :->  int :-> int  , IfComb          )
         ,("*"  , int :-> int :-> int            , kratFun        )   
         ,("-"  , int :-> int :-> int            , minusFun       )
         ,("=="  , int :-> int :-> bool           , eqFun          ) 
         ,("fac" , int:-> int                     , facComb       )
         ,("null", listInt :-> bool               , nullFun       )
         ,("foldr", (int:->int:->int):->int:->listInt:->int , foldrComb )    
         ,("head", listInt:->int     ,headFun)
         ,("tail", listInt:->listInt ,tailFun)
       ]


com :: (Typeable a) => TTerm -> a
com tt = case (compute 100000 tt )::((Typeable a) =>Either TTerm a) of
  Right i -> i

par = fromJust . parseTTerm env3

ttt1 = ( com $ par "foldr + 0" @@ ([1..100]::[Int])  ) :: Int

tt1 = par "+ 1 1"--"x:Int y:Int . + (+ x y) (* x y)"
tt2 = par "if (== 1 1)" --"x:Int y:Int . (z:Int .* x (+ z y)) (+ y y)"

incComb  = mkComb env1 "+ 1"
facComb = mkComb env3 "x : Int . if (== x 0) 1 (* x (fac (- x 1)) )"

foldrComb = mkComb env3 "f:Int->Int->Int z:Int xs:[Int] . if (null xs) z ( f (head xs) (foldr f z (tail xs)) )"

lol = parseTTerm env3 "x : Int . if (== x 0) 1 (* x (- x 1) )"
lol2 = parseTerm "x:Int .  (- x x )"
 
ddotInt  = toDyn' ( (:) :: Int -> [Int] -> [Int] )
foldrInt = toDyn' ( foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int )
plusFun  = toDyn' ((+) :: Int -> Int -> Int)
nullFun  = toDyn' (null:: [Int] -> Bool )
headFun  = toDyn' ( ( \ xs -> if null xs then 0  else head xs ) :: [Int] -> Int   ) 
tailFun  = toDyn' ( ( \ xs -> if null xs then [] else tail xs ) :: [Int] -> [Int] )


env1 =  [
          ( "abs", int :-> int                    , absFun         )
          ,("+"  , int :-> int :-> int            , plusFun        )
          ,("-"  , int :-> int :-> int            , minusFun       )
          ,("*"  , int :-> int :-> int            , kratFun        )            
          ,("%"  , int :-> int :-> int            , divvFun        )
          ,("1"  , int                            , toDyn' (1::Int) )
          ,("<"  , int :-> int :-> bool           , ltFun          )
          ,("if" , bool :-> int :->  int :-> int  , ifFun          )
        ]

env2 =  [
          ( "abs", real :-> real                    , absFunR          )
          ,("+"  , real :-> real :-> real           , plusFunR         )
          ,("-"  , real :-> real :-> real           , minusFunR        )
          ,("*"  , real :-> real :-> real           , kratFunR         )            
       --   ,("%"  , real :-> real :-> real           , divvFunR         )
          ,("1"  , real                             , toDyn' (1::Double))
       --   ,("<"  , real :-> real :-> bool           , ltFunR           )
       --   ,("if" , bool :-> real :->  real :-> real , ifFunR           )
        ]


absFun   = toDyn' (abs :: Int -> Int)
minusFun = toDyn' ((-) :: Int -> Int -> Int)
kratFun  = toDyn' ((*) :: Int -> Int -> Int)
divvFun  = toDyn' ((\a b->if b==0 then 1 else a `div` b) :: Int -> Int -> Int)
ltFun    = toDyn' ((<) :: Int -> Int -> Bool)
eqFun    = toDyn' ((==) :: Int -> Int -> Bool)
ifFun    = toDyn' ((\p a b ->if p then a else b) :: Bool -> Int -> Int -> Int)

absFunR   = toDyn' (abs :: Double -> Double)
plusFunR  = toDyn' ((+) :: Double -> Double -> Double)
minusFunR = toDyn' ((-) :: Double -> Double -> Double)
kratFunR  = toDyn' ((*) :: Double -> Double -> Double)
divvFunR  = toDyn' ((\a b->if b==0 then 1 else a / b) :: Double -> Double -> Double)
ltFunR    = toDyn' ((<) :: Double -> Double -> Bool)
ifFunR    = toDyn' ((\p a b ->if p then a else b) :: Bool -> Double -> Double -> Double)


 -- pro pøípadné testovaní :

t2 = Lam "a" ( App  t1 
     (Var "a" int) (int) ) (int :-> int)


t1 = Lam "a" ( App(App(Val "*" kratFun (int :-> int :-> int )) 
     (Var "a" int) (int:->int)) (Var "a" int) (int) ) (int :-> int)
t0 = Val "5" (toDyn' (5::Int)) (int)


