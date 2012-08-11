{-# LANGUAGE MultiParamTypeClasses #-}

import System.Random


class Generable from by to where
 generate :: (RandomGen g) => from -> by -> ( [to] , g )




