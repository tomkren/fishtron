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
problemListToJSON problemList =  JSArray . map toProblemObj $ problemList
 where
  toProblemObj :: (String,String) -> JSValue
  toProblemObj ( code , name ) = JSObject $ toJSObject [
    ( "code" , JSString . toJSString $ code ) ,
    ( "name" , JSString . toJSString $ name )
   ]


jsEmptyObj :: JSValue
jsEmptyObj = JSObject $ toJSObject []

stdoutCmd :: String -> JSValue
stdoutCmd str = JSObject $ toJSObject [ 
  ("type" , JSString . toJSString $ "stdout" ) ,
  ("msg"  , JSString . toJSString $ str      ) ]

graphCmd :: Int -> Int -> (Double,Double,Double) -> JSValue
graphCmd runI genI (best,avg,worst) = toObj $ [ 
  ("type"   , JSString . toJSString $ "generationInfo" ) ,
  ("i"      , toRat genI ) ,
  ("run"    , toRat runI ) ,
  ("ffvals" , toObj [
      ( "best"  , toRat best  ) ,
      ( "avg"   , toRat avg   ) ,
      ( "worst" , toRat worst )
  ] ) ]
 where
  toObj x = JSObject . toJSObject $ x
  toRat x = JSRational True ( toRational x )

multiCmd :: [JSValue] -> JSValue
multiCmd cmds = JSObject . toJSObject $ [
   ( "type" , JSString . toJSString $ "multi" ),
   ( "cmds" , JSArray cmds )
 ]

