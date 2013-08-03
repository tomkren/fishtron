module ServerInterface 
( OutputBuffer
, OutRecord(..) 
, ProcessData(..)
, writeNextOutput
, jsEmptyObj
, stdoutCmd 
, graphCmd 
, multiCmd 
, emptyProcessData
, isKilled
, resetProcessData
) where


import Text.JSON (JSValue)
import JSONUtils (jsArr,jsObj,jsStr,jsNum)

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (TVar,modifyTVar,readTVarIO, writeTVar)




type OutputBuffer = TVar ProcessData --[OutRecord]

data ProcessData = ProcessData {
  processBuff :: [OutRecord] ,
  kill        :: Bool        ,
  processRuns :: Bool
}

data OutRecord = OutStr String | OutEnd


emptyProcessData :: ProcessData
emptyProcessData = ProcessData {
   processBuff = [] ,
   kill        = False ,
   processRuns = False
 }

writeNextOutput :: OutputBuffer -> String -> IO ()
writeNextOutput buff output 
  = atomically $ modifyTVar buff (\b-> b{ processBuff = (OutStr output):(processBuff b) }   ) --(\b->(OutStr output):b)

resetProcessData :: OutputBuffer -> IO ()
resetProcessData buff = atomically $ writeTVar buff emptyProcessData


isKilled :: OutputBuffer -> IO Bool
isKilled buff = do
  pd <- readTVarIO buff
  return $ kill pd

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








