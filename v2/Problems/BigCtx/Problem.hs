module Problems.BigCtx.Problem where

import TTerm (Typ(..),Context)

import Problems.Utils ( cttProblem5 , asType )

problem1 =
 cttProblem5 "BigCtx" ff_head (l_int :-> m_int) ctx (asType::[Int]->Maybe Int)




int :: Typ
int   = Typ "Int"
m_int = Typ "Maybe Int"
l_int = Typ "[Int]"


ctx_head :: Context
ctx_head = [  ( "listCase" , l_int :-> m_int :-> (int:->l_int:->m_int) :-> m_int ),
              ( "Nothing"  , m_int ),
              ( "Just"     , int :-> m_int ) ]


ff_head :: ( [Int] -> Maybe Int ) -> (Double,Bool)
ff_head prog = 
  let ffval = ( if prog []       == Nothing then 1 else 0 ) +
              ( if prog [1]      == Just 1  then 1 else 0 ) +
              ( if prog [42,7,3] == Just 42 then 1 else 0 )
   in (ffval,ffval == 3)
