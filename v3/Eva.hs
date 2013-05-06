module Eva where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.Random
import Control.Monad.State
import Data.Typeable

import Heval (hevalsWith)


type Eva = StateT EvaState IO

data EvaState = EvaState {
     evaGen   :: StdGen ,
     evaJobID :: Int 
  }



evalsWith :: (Typeable a) => String -> [String] -> a -> Eva [a]
evalsWith file strs as = liftIO $ hevalsWith file strs as