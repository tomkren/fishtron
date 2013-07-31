{-# LANGUAGE OverloadedStrings #-}
 
import System.Environment ( getArgs )

import Network.Wai (Application,Request(..),Response(..),responseLBS)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

import Control.Monad.State (liftIO)
import Control.Concurrent (forkIO)

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (TVar,newTVar,modifyTVar,swapTVar,readTVarIO,readTVar,writeTVar)

import Data.ByteString.Lazy.UTF8 (fromString)

import Text.JSON ( encode )

import Utils ( unescape )
import Register (problemList,job)
import ServerInterface( OutputBuffer , OutRecord(..) )



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
    args <- getArgs
    let port  = case args of
                 []       -> "3000"
                 (arg1:_) -> arg1 
    putStrLn $ "Fishtron GUI is accessible on http://localhost:" ++ port 
    ss     <- newServerState
    tvarSS <- atomically $ newTVar ss
    run (read port) (app tvarSS)

app :: TVar ServerState -> Application
app tvarSS req = case pathInfo req of
 []                         -> return $ serveFile "index.html"
 ["favicon.ico"]            -> return $ myIco           $ "favicon.ico"
 ["files",filename]         -> return $ myFile          $ myUnpack filename
 ["css",filename]           -> return $ myCSSFile       $ myUnpack filename
 ["css","images",filename]  -> return $ myCssImageFile  $ myUnpack filename
 ["img",filename]           -> return $ myPng           $ myUnpack filename
 ["js",filename]            -> return $ myJSFile        $ myUnpack filename
 ["js","libs",filename]     -> return $ myJSFile $ "libs/"     ++ (myUnpack filename)
 ["js","Problems",filename] -> return $ myJSFile $ "Problems/" ++ (myUnpack filename)  

 ["problems"] -> return . myTextPlain . fromString . encode $ problemList

 ["run",cmd] -> do 
   let cmdStr = myUnpack $ cmd 
   ss <- liftIO . readTVarIO $ tvarSS
   liftIO . forkIO $ do
     job (ssBuff ss) (unescape cmdStr) 
     atomically $ modifyTVar (ssBuff ss) (\b->OutEnd:b)
   return $ myTextPlain "OK"

 ["out"] -> do
   ss <- liftIO . readTVarIO $ tvarSS
   outStr <- liftIO . resolveOutRequest $ ssBuff ss
   return $ myTextPlain (fromString outStr)   

 x -> do 
  liftIO $ putStrLn $ "404: " ++ show x
  return $ myTextPlain "404"

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
   Nothing           -> "_"
   Just (OutEnd)     -> ""
   Just (OutStr str) -> str

myTextPlain    x        = responseLBS  status200 [ ("Content-Type", "text/plain") ]  x
serveFile      filename = ResponseFile status200 [ ("Content-Type", "text/html") ]       ("data/"++filename)               Nothing
myFile         filename = ResponseFile status200 [  ]                                    ("data/files/" ++ filename )      Nothing
myJSFile       filename = ResponseFile status200 [ ("Content-Type", "text/javascript") ] ("data/js/" ++ filename )         Nothing
myCSSFile      filename = ResponseFile status200 [ ("Content-Type", "text/css") ]        ("data/css/" ++ filename )        Nothing
myCssImageFile filename = ResponseFile status200 [ ("Content-Type", "image/png") ]       ("data/css/images/" ++ filename ) Nothing
myPng          filename = ResponseFile status200 [ ("Content-Type", "image/png") ]       ("data/img/" ++ filename )        Nothing
myIco          filename = ResponseFile status200 [ ("Content-Type", "image/x-icon") ]    ("data/img/" ++ filename )        Nothing

myUnpack x = (init . tail . show $ x)




    


