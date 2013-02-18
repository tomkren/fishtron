{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid
import Data.Enumerator (run_, enumList, ($$))
 
-- já
import Control.Concurrent  
import Control.Monad
import Control.Monad.State
import System.IO
import System.Directory

import GP_Test
import ServerInterface

foo num = do

  forM_ [1..num] (\ n -> (putStrLn . show $ n) )

bar = replicateM_ 100 (putStrLn "bar")

baz = do
  forkIO $ foo 10
  bar

--konec ja

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
clearServer = undefined


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

hardWork :: Int -> Int -> IO ()
hardWork workerId param = do
  forM_ [1..param] (\ n -> writeNextOutput workerId ( replicate n 'x' ) )
  closeWorker workerId

runCmd :: Int -> String -> IO ()
runCmd workerId cmd = do
  isWorkingSomeone <- isAnyoneWorking
  if isWorkingSomeone then do
    writeNextOutput workerId $ "Někdo již pracuje, zařazuji se do fronty........"
    setWaiting   workerId cmd
   else do
    setIsWorking workerId True
    writeNextOutput workerId $ "I want to do this : "  ++ cmd
    writeNextOutput workerId $ "But i can't ! :("
    writeNextOutput workerId $ "Instead i will run the job1"
    job1 (show workerId)
    writeNextOutput workerId $ "Done!"
    closeWorker workerId

serveOutput2 :: Int -> Int -> IO String
serveOutput2 wid oid = do
  let filename = "server/output/" ++ (show wid) ++ "/" ++ (show oid) ++ ".txt"
  itExists <- doesFileExist filename 
  if itExists 
   then myReadFile filename
   else do
    stillWorking <- isWorkingOrWaiting wid
    return $ if stillWorking then "_" else ""


main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req = 
 case pathInfo req of

    ["out",i,j] -> do
      let workerId = ( read . init . tail . show $ i ) :: Int
      let outputId = ( read . init . tail . show $ j ) :: Int
      outString <- liftIO $ serveOutput2 workerId outputId
      return $ yay outString

    [ ] -> return $ myIndex

    ["run",cmd] -> do 
      workerId <- liftIO newWorker
      let cmdStr = init . tail . show $ cmd 
      liftIO . forkIO $ runCmd workerId cmdStr
      return . yay . show $ workerId

    ["js",filename] -> do
      return $ myJSFile (init . tail . show $ filename)

    ["files",filename] -> do
      return $ myFile (init . tail . show $ filename)

    ["yay",n] -> do 
      let num = ( read . init . tail . show $ n ) :: Int
      liftIO $ putStrLn ( "yay = " ++ show (num) )
      workerId <- liftIO $ newWorker 
      liftIO $ forkIO ( hardWork workerId num )
      return $ yay (show workerId)

    _ -> do
      --index <- liftIO $ readFile "server/index.html"
      return $ my404 --htmlPage index
    --x -> return $ index x

 
yay x = ResponseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ BU.fromString x ]


myIndex = ResponseFile status200 [ ("Content-Type", "text/html") ] "server/index.html" Nothing

myFile   filename = ResponseFile status200 [  ] ("server/files/" ++ filename ) Nothing
myJSFile filename = ResponseFile status200 [ ("Content-Type", "text/javascript") ] ("server/js/" ++ filename ) Nothing

my404 = ResponseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ "404" ]

-- nejveci prasarna, urcite se to ma delat jinak
htmlPage sourceCode = ResponseBuilder status200 [("Content-Type", "text/html")] $ 
 copyByteString $ BU.fromString sourceCode
 
index x = ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p>Hello from ", BU.fromString $ show x, "!</p>"
    , "<p><a href='/yay'>yay</a></p>\n" ]