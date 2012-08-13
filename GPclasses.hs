{-# LANGUAGE MultiParamTypeClasses #-}

import System.Random


class Generable from by to where
 generateIt :: (RandomGen g) => g -> from -> by -> ( [to] , g )


class Mutable a where
 mutateIt :: (RandomGen g) => g -> a -> ( a , g )

class Xoverable a where
 xoverIt :: (RandomGen g) => g-> a -> a -> ( (a,a) , g )

