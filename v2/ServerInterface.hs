module ServerInterface where

import System.IO
import System.Directory

import Text.JSON



writeNextOutput :: Int -> String -> IO ()
writeNextOutput i output = do
  outI <- incrementFile ("server/output/" ++ (show i) ++ "/_hotovo.txt")
  myWriteFile ("server/output/"++ (show i) ++ "/" ++ (show outI )++ ".txt" ) output


incrementFile :: FilePath -> IO Int
incrementFile filename = do 
  file <- openFile filename ReadWriteMode
  str <- hGetLine file 
  let num  = 1 + ( read str ) :: Int
      str' = show num
  hSeek file AbsoluteSeek 0
  hPutStr file str'
  hClose file 
  return num


myWriteFile :: FilePath -> String -> IO ()
myWriteFile filename str = do 
  file <- openFile filename WriteMode
  hPutStr file str
  hClose file

myReadFile :: FilePath -> IO String
myReadFile filename = do
 file <- openFile filename ReadMode 
 str <- hGetLine file 
 hClose file
 return str


problemListToJSON :: [(String,String)] -> JSValue
problemListToJSON problemList =  jsArr . map toProblemObj $ problemList
 where
  toProblemObj :: (String,String) -> JSValue
  toProblemObj ( code , name ) = jsObj [
    ( "code" , jsStr code ) ,
    ( "name" , jsStr name )
   ]


jsEmptyObj :: JSValue
jsEmptyObj = jsObj []

stdoutCmd :: String -> JSValue
stdoutCmd str = jsObj [ 
  ("type" , jsStr "stdout" ) ,
  ("msg"  , jsStr str      ) ]

graphCmd :: Int -> Int -> (Double,Double,Double) -> JSValue
graphCmd runI genI (best,avg,worst) = jsObj [ 
  ("type"   , jsStr "generationInfo" ) ,
  ("i"      , jsNum genI ) ,
  ("run"    , jsNum runI ) ,
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


jsObj :: [(String, JSValue)] -> JSValue
jsObj = JSObject . toJSObject  

jsArr :: [JSValue] -> JSValue
jsArr = JSArray

jsStr :: String -> JSValue
jsStr = JSString . toJSString

jsNum :: Real a => a -> JSValue
jsNum = JSRational True . toRational







