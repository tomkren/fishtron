module Problems.BigCtx.Funs where


s :: (a->b->c) -> (a->b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x y = x

i :: a -> a
i x = x


listCase :: [a] -> b -> (a->[a]->b) -> b
listCase as b1 b2 = case as of
 []   -> b1
 x:xs -> b2 x xs 

ff_head :: ( [Int] -> Maybe Int ) -> (Double,Bool)
ff_head prog = 
  let ffval = ( if prog []       == Nothing then 1 else 0 ) +
              ( if prog [1]      == Just 1  then 1 else 0 ) +
              ( if prog [42,7,3] == Just 42 then 1 else 0 )
   in (ffval,ffval == 3)


