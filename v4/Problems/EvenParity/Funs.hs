module Problems.EvenParity.Funs where


s :: (a->b->c) -> (a->b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x y = x

i :: a -> a
i x = x

s_ :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
s_ c f g x = c (f x) (g x)

b::(b -> c) -> (a -> b) -> a -> c
b    f g x = f (g x)

b_ :: (c -> d) -> (b -> c) -> (a -> b) -> a -> d
b_ c f g x = c (f (g x))

c :: (a->b->c)->b->a->c
c f g x = f x g 

c_ :: (b->c->d)->(a->b)->c->a->d
c_ c f g x = c (f x) g


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


