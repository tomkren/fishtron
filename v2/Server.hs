{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid
import Data.Enumerator (run_, enumList, ($$))

import Text.JSON ( encode )
 
-- já
import Control.Concurrent  
import Control.Monad
import Control.Monad.State
import System.IO
import System.Directory
import System.Environment


import Job (job,problemList)

import ServerInterface

main = do
    clearServer
    (port:_) <- getArgs
    putStrLn $ "Fishtron GUI is on http://localhost:" ++ port 
    run (read port) app

app :: Application
app req = 
 case pathInfo req of

    ["out",i,j] -> do
      let workerId = ( read . init . tail . show $ i ) :: Int
      let outputId = ( read . init . tail . show $ j ) :: Int
      outString <- liftIO $ serveOutput workerId outputId
      return $ myTextPlain outString

    [ ] -> return $ myIndex

    ["run",cmd] -> do 
      workerId <- liftIO newWorker
      let cmdStr = init . tail . show $ cmd 
      liftIO . forkIO $ runCmd workerId cmdStr
      return . myTextPlain . show $ workerId

    ["problems"] -> return . myTextPlain . encode . problemListToJSON $ problemList

    ["js",filename] -> do
      return $ myJSFile (init . tail . show $ filename)

    ["js","libs",filename] -> do
      return $ myJSFile $ "libs/" ++ (init . tail . show $ filename)
    
    ["js","Problems",filename] -> do
      return $ myJSFile $ "Problems/" ++ (init . tail . show $ filename)  

    ["files",filename] -> do
      return $ myFile (init . tail . show $ filename)

    ["css",filename] -> do
      return $ myCSSFile (init . tail . show $ filename)

    ["css","images",filename] -> return $ myCssImageFile (init . tail . show $ filename)

    ["img",filename] -> return $ myPng (init . tail . show $ filename)

    _ -> return $ my404



runCmd :: Int -> String -> IO ()
runCmd workerId cmd = do
  let logg = writeNextOutput workerId . encode . stdoutCmd 
  isWorkingSomeone <- isAnyoneWorking
  if isWorkingSomeone then do
    logg $ "Někdo již pracuje, zařazuji se do fronty........"
    setWaiting   workerId cmd
   else do
    setIsWorking workerId True
    logg $ replicate 80 '─'
    logg $ "run/" ++ cmd
    logg $ replicate 80 '─'
    job (show workerId) cmd
    logg $ "Done!"
    closeWorker workerId

serveOutput :: Int -> Int -> IO String
serveOutput wid oid = do
  let filename = "server/output/" ++ (show wid) ++ "/" ++ (show oid) ++ ".txt"
  itExists <- doesFileExist filename 
  if itExists 
   then do
    output <- myReadFile filename
    removeFile filename
    return output
   else do
    stillWorking <- isWorkingOrWaiting wid
    return $ if stillWorking then "_" else "" 



currentWorkerIdFile :: String
currentWorkerIdFile = "server/current.txt"

newWorker :: IO Int
newWorker = do
  i <- incrementFile "server/current.txt"
  createOutputFolder i
  -- setIsWorking i True
  return i


createOutputFolder :: Int -> IO ()
createOutputFolder i = do
 let dir = "server/output/" ++ (show i)
 createDirectory dir
 myWriteFile (dir ++ "/_hotovo.txt") "0"

  
clearServer :: IO ()
clearServer = do
  myRemoveDir "server/output"
  myRemoveDir "server/working"
  myRemoveDir "server/queue"
  createDirectory "server/output"
  createDirectory "server/working"
  createDirectory "server/queue"  
  myWriteFile "server/current.txt" "0"  

myRemoveDir :: String -> IO ()
myRemoveDir filename = do
  does <- doesDirectoryExist filename
  if does then removeDirectoryRecursive filename else return ()


workerFilename :: Int -> String
workerFilename i = "server/working/" ++ (show i) ++ ".txt" 

waiterFilename :: Int -> String
waiterFilename i = "server/queue/" ++ (show i) ++ ".txt" 

setIsWorking :: Int -> Bool -> IO ()
setIsWorking i True  = myWriteFile  (workerFilename i) ""
setIsWorking i False = removeFile (workerFilename i) 

setWaiting :: Int -> String -> IO ()
setWaiting i cmd = myWriteFile  (waiterFilename i) cmd 

closeWorker :: Int -> IO ()
closeWorker workerId = do
  setIsWorking workerId False
  fromQueue <- popQueue
  case fromQueue of
    Nothing            -> return ()
    Just ( wid , cmd ) -> runCmd wid cmd

popQueue :: IO (Maybe (Int,String) )
popQueue =  do
  dir <- getDirectoryContents "server/queue"
  let dir' = drop 2 $ reverse dir
  case dir' of
    []    -> return Nothing
    (x:_) -> do
      let filename = "server/queue/" ++ x 
      file <- openFile filename ReadMode
      cmd  <- hGetLine file 
      hClose file
      removeFile filename 
      return $ Just ( read $ takeWhile (/= '.') x  , cmd )     

isWorking :: Int -> IO Bool
isWorking i = doesFileExist (workerFilename i) 

isWaiting :: Int -> IO Bool
isWaiting i = doesFileExist (waiterFilename i) 

isWorkingOrWaiting :: Int -> IO Bool
isWorkingOrWaiting i = do
  a <- isWorking i
  if a then return True else isWaiting i

isAnyoneWorking :: IO Bool
isAnyoneWorking = do
  dir <- getDirectoryContents "server/working"
  return $ length dir > 2 





 
myTextPlain x = ResponseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ BU.fromString x ]


myIndex = ResponseFile status200 [ ("Content-Type", "text/html") ] "server/index.html" Nothing

myFile         filename = ResponseFile status200 [  ] ("server/files/" ++ filename ) Nothing
myJSFile       filename = ResponseFile status200 [ ("Content-Type", "text/javascript") ] ("server/js/" ++ filename ) Nothing
myCSSFile      filename = ResponseFile status200 [ ("Content-Type", "text/css") ] ("server/css/" ++ filename ) Nothing
myCssImageFile filename = ResponseFile status200 [ ("Content-Type", "image/png") ] ("server/css/images/" ++ filename ) Nothing

myPng filename = ResponseFile status200 [ ("Content-Type", "image/png") ] ("server/img/" ++ filename ) Nothing


my404 = ResponseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ "404" ]

-- nejveci prasarna, urcite se to ma delat jinak
htmlPage sourceCode = ResponseBuilder status200 [("Content-Type", "text/html")] $ 
 copyByteString $ BU.fromString sourceCode
 
index x = ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p>Hello from ", BU.fromString $ show x, "!</p>"
    , "<p><a href='/yay'>yay</a></p>\n" ]