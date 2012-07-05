-- Inhabitation Machines

module IM where
import qualified Data.Map as Map
import Data.Map (Map)
import Base
import Util



data PreIM =  Map NodeId Node

data Node = OrNode  Typ   Context [NodeId]
          | AndNode Token Context [NodeId]

data NodeId = TypId Typ | TokenId Int 



data Token = TLam [Symbol] 
           | TVar  Symbol


mkPre :: [(Symbol,Typ)] -> Map NodeId Node-> Typ -> Int -> Map NodeId Node
mkPre env nodeMap typ n = undefined
 where
  orNode  = OrNode typ env  
  andNode =  undefined


