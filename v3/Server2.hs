{-# LANGUAGE OverloadedStrings #-}
 
--import Network.Wai
--import Network.Wai.Handler.Warp
--import Network.HTTP.Types (status200)
--import Blaze.ByteString.Builder (copyByteString)
--import qualified Data.ByteString.UTF8 as BU
--import Data.Monoid
--import Data.Enumerator (run_, enumList, ($$))
--
--import Text.JSON ( encode )
-- 
---- já
--import Control.Concurrent  
--import Control.Monad
--import Control.Monad.State
--import System.IO
--import System.Directory (doesFileExist, removeFile, createDirectory, doesDirectoryExist,removeDirectoryRecursive,getDirectoryContents)
--import System.Environment
--
--import Utils ( unescape )
----import Job (job,problemList,problemList_,job_)
--import Job (problemList_,job_)
--
--import ServerInterface



import Network.Wai (Application,Request(..),Response(..),responseLBS)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

import Control.Monad.State (liftIO)
import Control.Concurrent (threadDelay,forkIO)

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (TVar,newTVar,modifyTVar,swapTVar,readTVarIO,readTVar,writeTVar)

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.ByteString.Lazy as B (concat, ByteString, append)

import Text.JSON ( encode )



import Job (problemList_,job_)



data OutRecord = OutStr String | OutEnd
type OutputBuffer = TVar [OutRecord]
type InputCmd = String

data ServerState = ServerState{
  ssBuff :: OutputBuffer  
 }

newServerState :: IO ServerState
newServerState = do
  buff <- atomically $ newTVar []
  return ServerState{
    ssBuff = buff   
   }

main = do
    --(port:_) <- getArgs
    let port = "3000"
    putStrLn $ "FAKE GUI is on http://localhost:" ++ port 
    ss     <- newServerState
    tvarSS <- atomically $ newTVar ss
    run (read port) (app tvarSS)

app :: TVar ServerState -> Application
app tvarSS req = case pathInfo req of
 []                     -> return $ serveFile "index_stm.html"
 ["js","libs",filename] -> return $ myJSFile $ "libs/" ++ (myUnpack $ filename)

 ["problems"] -> return . myTextPlain . fromString . encode $ problemList_

 ["run",cmd] -> do 
   let cmdStr = myUnpack $ cmd 
   ss <- liftIO . readTVarIO $ tvarSS
   liftIO . forkIO $ solver cmdStr (ssBuff ss)
   return $ myTextPlain "OK"

 ["out"] -> do
   ss     <- liftIO . readTVarIO $ tvarSS
   outStr <- liftIO . resolveOutRequest $ ssBuff ss
   return $ myTextPlain (fromString outStr)   

 x -> do 
  liftIO $ putStrLn $ "404: " ++ show x
  return $ myTextPlain "404"

serveFile filename = ResponseFile status200 [ ("Content-Type", "text/html") ] ("server/"++filename) Nothing
myJSFile filename  = ResponseFile status200 [ ("Content-Type", "text/javascript") ] ("server/js/" ++ filename ) Nothing
myTextPlain x      = responseLBS  status200 [ ("Content-Type", "text/plain") ] $ x
myUnpack x = (init . tail . show $ x)



resolveOutRequest :: OutputBuffer -> IO String
resolveOutRequest buff = do
  m_x <- atomically $ do 
          xs <- readTVar buff
          case xs of
           [] -> return Nothing
           _  -> do
             writeTVar buff (init xs)
             return $ Just (last xs)
  return $ case m_x of
   Nothing           -> ""
   Just (OutEnd)     -> "_"
   Just (OutStr str) -> str
    


solver :: InputCmd -> OutputBuffer -> IO ()
solver cmd buff = fakeSolver (read cmd)
 where
  fakeSolver :: Int -> IO ()
  fakeSolver 0 = do
    atomically $ modifyTVar buff (\b->OutEnd:b)
    return ()
  fakeSolver i = do
    putStrLn $ "fejcek " ++ show i
    atomically $ modifyTVar buff (\b->(OutStr $ show i):b)
    threadDelay 10000
    fakeSolver (i-1)

listener :: OutputBuffer -> IO ()
listener buff = do
  b <- atomically $ swapTVar buff []
  case reverse b of
    [] -> do
      putStrLn "nic"
      threadDelay 20000
      listener buff
    xs -> do
      continue <- processBuff xs
      if continue 
       then do
         threadDelay 20000
         listener buff
       else return () 
 where
  processBuff :: [OutRecord] -> IO Bool
  processBuff []         = return True
  processBuff (OutEnd:_) = do
    putStrLn "END"
    return False
  processBuff ((OutStr x):xs) = do
        putStrLn x
        processBuff xs
         



test :: IO ()
test = do
 buff <- atomically $ newTVar []
 forkIO $ solver "100" buff
 threadDelay 25000
 forkIO $ listener buff
 return ()






--main = do
--    clearServer
--    (port:_) <- getArgs
--    putStrLn $ "Fishtron GUI is on http://localhost:" ++ port 
--    run (read port) app
--
--app :: Application
--app req = 
-- case pathInfo req of
--
--    ["out",i,j] -> do
--      let workerId = ( read . init . tail . show $ i ) :: Int
--      let outputId = ( read . init . tail . show $ j ) :: Int
--      outString <- liftIO $ serveOutput workerId outputId
--      return $ myTextPlain outString
--
--    [ ] -> return $ myIndex
--
----    ["run",cmd] -> do 
----      workerId <- liftIO newWorker
----      let cmdStr = init . tail . show $ cmd 
----      liftIO . forkIO $ runCmd workerId cmdStr
----      return . myTextPlain . show $ workerId
--
--    ["run_",cmd] -> do 
--      workerId <- liftIO newWorker
--      let cmdStr = init . tail . show $ cmd 
--      liftIO . forkIO $ runCmd_ workerId (unescape cmdStr)
--      return . myTextPlain . show $ workerId
--
----    ["problems"] -> return . myTextPlain . encode . problemListToJSON $ problemList
--
--    ["problems_"]-> return . myTextPlain . encode $ problemList_    
--
--    ["js",filename] -> do
--      return $ myJSFile (init . tail . show $ filename)
--
--    ["js","libs",filename] -> do
--      return $ myJSFile $ "libs/" ++ (init . tail . show $ filename)
--    
--    ["js","Problems",filename] -> do
--      return $ myJSFile $ "Problems/" ++ (init . tail . show $ filename)  
--
--    ["files",filename] -> do
--      return $ myFile (init . tail . show $ filename)
--
--    ["css",filename] -> do
--      return $ myCSSFile (init . tail . show $ filename)
--
--    ["css","images",filename] -> return $ myCssImageFile (init . tail . show $ filename)
--
--    ["img",filename] -> return $ myPng (init . tail . show $ filename)
--
--    x -> do 
--      liftIO . putStrLn . show $ x
--      return $ my404
--
--
--
---- runCmd :: Int -> String -> IO ()
---- runCmd workerId cmd = do
----   let logg = writeNextOutput workerId . encode . stdoutCmd 
----   isWorkingSomeone <- isAnyoneWorking
----   if isWorkingSomeone then do
----     logg $ "Někdo již pracuje, zařazuji se do fronty........"
----     setWaiting   workerId cmd
----    else do
----     setIsWorking workerId True
----     logg $ replicate 80 '─'
----     logg $ "run/" ++ cmd
----     logg $ replicate 80 '─'
----     job (show workerId) cmd
----     logg $ "Done!"
----     closeWorker workerId
--
--runCmd_ :: Int -> String -> IO ()
--runCmd_ workerId cmd = do
--  let logg = writeNextOutput workerId . encode . stdoutCmd 
--  isWorkingSomeone <- isAnyoneWorking
--  if isWorkingSomeone then do
--    logg $ "Někdo již pracuje, zařazuji se do fronty........"
--    setWaiting   workerId cmd
--   else do
--    setIsWorking workerId True
--    logg $ replicate 80 '─'
--    logg $ "run/" ++ cmd
--    logg $ replicate 80 '─'
--    job_ (show workerId) cmd
--    logg $ "Done!"
--    closeWorker workerId
--
--
--serveOutput :: Int -> Int -> IO String
--serveOutput wid oid = do
--  let filename = "server/output/" ++ (show wid) ++ "/" ++ (show oid) ++ ".txt"
--  itExists <- doesFileExist filename 
--  if itExists 
--   then do
--    output <- myReadFile filename
--    removeFile filename
--    return output
--   else do
--    stillWorking <- isWorkingOrWaiting wid
--    return $ if stillWorking then "_" else "" 
--
--
--
--currentWorkerIdFile :: String
--currentWorkerIdFile = "server/current.txt"
--
--newWorker :: IO Int
--newWorker = do
--  i <- incrementFile "server/current.txt"
--  createOutputFolder i
--  -- setIsWorking i True
--  return i
--
--
--createOutputFolder :: Int -> IO ()
--createOutputFolder i = do
-- let dir = "server/output/" ++ (show i)
-- createDirectory dir
-- myWriteFile (dir ++ "/_hotovo.txt") "0"
--
--  
--clearServer :: IO ()
--clearServer = do
--  myRemoveDir "server/output"
--  myRemoveDir "server/working"
--  myRemoveDir "server/queue"
--  createDirectory "server/output"
--  createDirectory "server/working"
--  createDirectory "server/queue"  
--  myWriteFile "server/current.txt" "0"  
--
--myRemoveDir :: String -> IO ()
--myRemoveDir filename = do
--  does <- doesDirectoryExist filename
--  if does then removeDirectoryRecursive filename else return ()
--
--
--workerFilename :: Int -> String
--workerFilename i = "server/working/" ++ (show i) ++ ".txt" 
--
--waiterFilename :: Int -> String
--waiterFilename i = "server/queue/" ++ (show i) ++ ".txt" 
--
--setIsWorking :: Int -> Bool -> IO ()
--setIsWorking i True  = myWriteFile  (workerFilename i) ""
--setIsWorking i False = removeFile (workerFilename i) 
--
--setWaiting :: Int -> String -> IO ()
--setWaiting i cmd = myWriteFile  (waiterFilename i) cmd 
--
--closeWorker :: Int -> IO ()
--closeWorker workerId = do
--  setIsWorking workerId False
--  fromQueue <- popQueue
--  case fromQueue of
--    Nothing            -> return ()
--    Just ( wid , cmd ) -> runCmd_ wid cmd
--
--popQueue :: IO (Maybe (Int,String) )
--popQueue =  do
--  dir <- getDirectoryContents "server/queue"
--  let dir' = drop 2 $ reverse dir
--  case dir' of
--    []    -> return Nothing
--    (x:_) -> do
--      let filename = "server/queue/" ++ x 
--      file <- openFile filename ReadMode
--      cmd  <- hGetLine file 
--      hClose file
--      removeFile filename 
--      return $ Just ( read $ takeWhile (/= '.') x  , cmd )     
--
--isWorking :: Int -> IO Bool
--isWorking i = doesFileExist (workerFilename i) 
--
--isWaiting :: Int -> IO Bool
--isWaiting i = doesFileExist (waiterFilename i) 
--
--isWorkingOrWaiting :: Int -> IO Bool
--isWorkingOrWaiting i = do
--  a <- isWorking i
--  if a then return True else isWaiting i
--
--isAnyoneWorking :: IO Bool
--isAnyoneWorking = do
--  dir <- getDirectoryContents "server/working"
--  return $ length dir > 2 
--
--
--
--
--
-- 
--myTextPlain x = ResponseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
--    [ BU.fromString x ]
--
--
--myIndex = ResponseFile status200 [ ("Content-Type", "text/html") ] "server/index.html" Nothing
--
--myFile         filename = ResponseFile status200 [  ] ("server/files/" ++ filename ) Nothing
--myJSFile       filename = ResponseFile status200 [ ("Content-Type", "text/javascript") ] ("server/js/" ++ filename ) Nothing
--myCSSFile      filename = ResponseFile status200 [ ("Content-Type", "text/css") ] ("server/css/" ++ filename ) Nothing
--myCssImageFile filename = ResponseFile status200 [ ("Content-Type", "image/png") ] ("server/css/images/" ++ filename ) Nothing
--
--myPng filename = ResponseFile status200 [ ("Content-Type", "image/png") ] ("server/img/" ++ filename ) Nothing
--
--
--my404 = ResponseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
--    [ "404" ]
--
---- nejveci prasarna, urcite se to ma delat jinak
--htmlPage sourceCode = ResponseBuilder status200 [("Content-Type", "text/html")] $ 
-- copyByteString $ BU.fromString sourceCode
-- 
--index x = ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
--    [ "<p>Hello from ", BU.fromString $ show x, "!</p>"
--    , "<p><a href='/yay'>yay</a></p>\n" ]--