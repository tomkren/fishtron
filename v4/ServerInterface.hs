module ServerInterface 
( OutputBuffer
, OutRecord(..) 
, writeNextOutput
, jsEmptyObj
, stdoutCmd 
, graphCmd 
, multiCmd ) where


import Text.JSON (JSValue)
import JSONUtils (jsArr,jsObj,jsStr,jsNum)

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (TVar,modifyTVar)

type OutputBuffer = TVar [OutRecord]
data OutRecord = OutStr String | OutEnd


writeNextOutput :: OutputBuffer -> String -> IO ()
writeNextOutput buff output 
  = atomically $ modifyTVar buff (\b->(OutStr output):b)


jsEmptyObj :: JSValue
jsEmptyObj = jsObj []

stdoutCmd :: String -> JSValue
stdoutCmd str = jsObj [ 
  ("type" , jsStr "stdout" ) ,
  ("msg"  , jsStr str      ) ]

graphCmd :: Int -> Int -> (Double,Double,Double) -> Bool -> Maybe Double -> JSValue
graphCmd runI genI (best,avg,worst) isWinner m_averageTermSize = jsObj $ 
 ( case m_averageTermSize of
     Nothing -> []
     Just x  -> [("averageTermSize" , jsNum x )]
 ) ++
 [ 
  ("type"   , jsStr "generationInfo" ) ,
  ("i"      , jsNum genI ) ,
  ("run"    , jsNum runI ) ,
  ("isWinner", jsNum (if isWinner then 1 else 0) ),
  ("ffvals" , jsObj [
      ( "best"  , jsNum best  ) ,
      ( "avg"   , jsNum avg   ) ,
      ( "worst" , jsNum worst )
  ] ) ]

multiCmd :: [JSValue] -> JSValue
multiCmd cmds = jsObj [
   ( "type" , jsStr "multi" ),
   ( "cmds" , jsArr cmds )
 ]








