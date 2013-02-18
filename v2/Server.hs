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
  setIsWorking i True
  return i

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

createOutputFolder :: Int -> IO ()
createOutputFolder i = do
 let dir = "server/output/" ++ (show i)
 createDirectory dir
 writeFile (dir ++ "/_hotovo.txt") "0"
 writeFile (dir ++ "/_served.txt") "0"

  
writeNextOutput :: Int -> String -> IO ()
writeNextOutput i output = do
  outI <- incrementFile ("server/output/" ++ (show i) ++ "/_hotovo.txt")
  writeFile ("server/output/"++ (show i) ++ "/" ++ (show outI )++ ".txt" ) output
  

workerFilename :: Int -> String
workerFilename i = "server/working/" ++ (show i) ++ ".txt" 

setIsWorking :: Int -> Bool -> IO ()
setIsWorking i True  = writeFile (workerFilename i) ""
setIsWorking i False = removeFile (workerFilename i) 

isWorking :: Int -> IO Bool
isWorking i = doesFileExist (workerFilename i) 


hardWork :: Int -> Int -> IO ()
hardWork workerId param = do
  forM_ [1..param] (\ n -> writeNextOutput workerId ( replicate n 'x' ) )
  setIsWorking workerId False

serveOutput :: Int -> IO String
serveOutput i = do
  hotovo <- readFile ("server/output/" ++ (show i) ++ "/_hotovo.txt")
  --served <- readFile ("server/output/" ++ (show i) ++ "/_served.txt")
  file    <- openFile ("server/output/" ++ (show i) ++ "/_served.txt") ReadMode
  served' <- hGetLine file 
  hClose file
  if (read hotovo :: Int) > (read served' :: Int) then do
    outI <- incrementFile ("server/output/" ++ (show i) ++ "/_served.txt")
    output <- readFile ("server/output/" ++ (show i) ++ "/" ++ (show outI) ++ ".txt")
    return output 
   else
    return []

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req = 
 case pathInfo req of
    [ ] -> return $ myIndex
    ["js",filename] -> do
      return $ myJSFile (init . tail . show $ filename)
    ["files",filename] -> do
      return $ myFile (init . tail . show $ filename)
    ["out",i] -> do
      let workerId = ( read . init . tail . show $ i ) :: Int
      outString <- liftIO $ serveOutput workerId
      return $ yay outString
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