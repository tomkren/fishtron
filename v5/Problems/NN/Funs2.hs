-- {-# LANGUAGE FlexibleInstances  #-}

module Problems.EvenParity.Funs where


s :: (a->b->c) -> (a->b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x y = x

i :: a -> a
i x = x





neuron :: (Sumable a) => (R -> R) -> a -> R
neuron f xs = f $ suma xs  


sigmoida :: R -> R
sigmoida x = 1 / (1 + exp(-x))


sigNeuron :: (Sumable a) => a -> R
sigNeuron = neuron sigmoida   



class Sumable a where
 suma :: a -> R  
 w    :: R -> a -> a

instance Sumable Double where 
  suma x = x   
  w  v x = x * v

instance (Sumable a , Sumable b) => Sumable (a,b) where 
  suma (x,y) = suma x + suma y
  w  v (x,y) = (  v x ,    v y ) 


para :: (a->b) -> (c->d) -> ( (a,c) -> (b,d) )
para f g (x,y) = ( f x , g y ) 

seri :: (a->b) -> (b->c) -> (a->c)
seri f g = g . f 


copy :: a -> (a,a)
copy    a =  (a,a)

swap0 :: (a,b) -> (b,a)
swap0    (a,b) =  (b,a)

swap1 :: (a,(b,c)) -> ((a,b),c)
swap1    (a,(b,c)) =  ((a,b),c)

swap2 :: ((a,b),c) -> (a,(b,c))
swap2    ((a,b),c) =  (a,(b,c))

swap3 :: (a,(b,(c,d))) -> ((a,b),(c,d))
swap3    (a,(b,(c,d))) =  ((a,b),(c,d))

swap4 :: ((a,b),(c,d)) -> (a,(b,(c,d)))
swap4    ((a,b),(c,d)) =  (a,(b,(c,d)))


onFst :: (a->b) -> (a,c) -> (b  ,c)
onFst    f         (a,c) =  (f a,c)

onSnd :: (a->b) -> (c,a) -> (c,b  )
onSnd    f         (c,a) =  (c,f a)

onBoth :: (a->b) -> (c->d) -> (a,c) -> (b  ,d  ) 
onBoth    f         g         (a,c) =  (f a,g c)

copyAndGo :: (a->b) -> (a->c) -> a -> (b  ,c  ) 
copyAndGo    f         g         a =  (f a,g a) 



type Sol = T4 -> T6


type R = Double

type T0  = ()
type T1  = (R)
type T2  = (R,R)
type T3  = (R,(R,R))
type T4  = (R,(R,(R,R)))
type T5  = (R,(R,(R,(R,(R)))))
type T6  = (R,(R,(R,(R,(R,R)))))
type T7  = (R,(R,(R,(R,(R,(R,R))))))
type T8  = (R,(R,(R,(R,(R,(R,(R,R)))))))
type T9  = (R,(R,(R,(R,(R,(R,(R,(R,R))))))))
type T10 = (R,(R,(R,(R,(R,(R,(R,(R,(R,R)))))))))
type T11 = (R,(R,(R,(R,(R,(R,(R,(R,(R,(R,R))))))))))
type T12 = (R,(R,(R,(R,(R,(R,(R,(R,(R,(R,(R,R)))))))))))
type T13 = (R,(R,(R,(R,(R,(R,(R,(R,(R,(R,(R,(R,R))))))))))))
type T14 = (R,(R,(R,(R,(R,(R,(R,(R,(R,(R,(R,(R,(R,R)))))))))))))
type T15 = (R,(R,(R,(R,(R,(R,(R,(R,(R,(R,(R,(R,(R,(R,R))))))))))))))