{-# LANGUAGE FlexibleInstances  #-}

module Problems.EvenParity.Funs where


s :: (a->b->c) -> (a->b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x y = x

i :: a -> a
i x = x




type Shape a b = (a,b) 

type Network a b = (Shape a b , [Double] -> [Double])



neuron :: ( TInt a , TInt b) => ([Double] -> Double) -> Shape a b -> Network a b
neuron f shape = ( shape , \ xs -> replicate (numOutputs shape) $ f xs )

sigNeuron :: ( TInt a , TInt b) => Shape a b -> Network a b
sigNeuron = neuron (sigmoida . sum)   

sigmoida :: Double -> Double
sigmoida x = 1 / (1 + exp(-x))


-- idNetwork :: Int -> Int -> Network
-- idNetwork numInputs numOutputs = 

--para :: ( TInt a , TInt b , TInt c , TInt d ) => Network a b -> Network c d -> Network ? ?


numInputs :: ( TInt a ) => Shape a b -> Int
numInputs (n,_) = tInt n


numOutputs :: ( TInt b ) => Shape a b -> Int
numOutputs (_,n) = tInt n


ti = undefined

type T0  = ()
type T1  = Maybe T0
type T2  = Maybe T1
type T3  = Maybe T2
type T4  = Maybe T3
type T5  = Maybe T4
type T6  = Maybe T5
type T7  = Maybe T6
type T8  = Maybe T7
type T9  = Maybe T8
type T10 = Maybe T9
type T11 = Maybe T10
type T12 = Maybe T11
type T13 = Maybe T12
type T14 = Maybe T13
type T15 = Maybe T14

class TInt a where
  tInt :: a -> Int

instance TInt T0  where tInt _ = 0 
instance TInt T1  where tInt _ = 1  
instance TInt T2  where tInt _ = 2  
instance TInt T3  where tInt _ = 3  
instance TInt T4  where tInt _ = 4  
instance TInt T5  where tInt _ = 5  
instance TInt T6  where tInt _ = 6  
instance TInt T7  where tInt _ = 7  
instance TInt T8  where tInt _ = 8  
instance TInt T9  where tInt _ = 9  
instance TInt T10 where tInt _ = 10 
instance TInt T11 where tInt _ = 11 
instance TInt T12 where tInt _ = 12 
instance TInt T13 where tInt _ = 13 
instance TInt T14 where tInt _ = 14 
instance TInt T15 where tInt _ = 15 
  
