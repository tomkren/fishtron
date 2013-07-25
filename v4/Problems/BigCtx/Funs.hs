module Problems.BigCtx.Funs where



mkAll :: h -> t -> m -> f -> e -> (h,t,m,f,e)
mkAll h t m f e = (h,t,m,f,e)

mkFst3 :: h -> t -> m -> (h,t,m)
mkFst3 h t m = (h,t,m)

mkFst2 :: h -> t -> (h,t)
mkFst2 h t = (h,t)


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

if' :: Bool -> a -> a -> a
if' p q r = if p then q else r 




--lol :: Int -> [Int] -> Bool
--lol = \ x0 x1 -> foldr (s (s (k if') (s (k ((==) x0)) i)) (k True )) False x1

