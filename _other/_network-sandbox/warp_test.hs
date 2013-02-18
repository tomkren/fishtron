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


foo = replicateM_ 100 (putStrLn "foo")
bar = replicateM_ 100 (putStrLn "bar")

baz = do
  forkIO foo
  bar

--konec ja


main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req = do
  liftIO $ return ()
  return $ case pathInfo req of
    ["yay"] -> yay
    x -> index x
 
yay = ResponseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ "yay" ]
 
index x = ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p>Hello from ", BU.fromString $ show x, "!</p>"
    , "<p><a href='/yay'>yay</a></p>\n" ]