module Problems.EvenParity.Funs where


s :: (a->b->c) -> (a->b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x y = x

i :: a -> a
i x = x


nor :: Bool -> Bool -> Bool
nor a b = not ( a || b )

nand :: Bool -> Bool -> Bool
nand a b = not ( a && b )


head_ :: [Bool] -> Bool
head_ []    = False
head_ (x:_) = x

tail_ :: [Bool] -> [Bool]
tail_ []     = []
tail_ (_:xs) = xs


