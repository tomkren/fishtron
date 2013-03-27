module Problems.SSR.Problem where

import TTerm (Typ(..),Context)

import Problems.Utils ( cttProblem3 , cttProblem' , cttProblem2' , cttProblem4 , asType )

import Problems.SSR.Funs (ff)

problem1 = cttProblem'  "ssr"  ff (asType::Double->Double) dou1 ctx
problem2 = cttProblem2' "ssr2" ff (asType::Double->Double) dou1 ctx
problem3 = cttProblem3  "ssr3" "Problems.SSR.Funs" dou1 ctx
problem4 = cttProblem4  "ssr4" "Problems.SSR.Funs" ff dou1 ctx (asType::Double->Double)


dou, dou1, dou2 :: Typ
dou  = Typ "Double"
dou1 = dou :-> dou
dou2 = dou :-> dou :-> dou

ctx :: Context
ctx = [("(+)",dou2),("(-)",dou2),("(*)",dou2),("rdiv",dou2),("sin",dou1),("cos",dou1),("exp",dou1),("rlog",dou1)]



