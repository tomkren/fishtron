
import Ant
import Heval

main :: IO ()
main = do
  putStrLn "Trying to solve Ant problem!"
  i <- heval progStr2 (1::Double,True::Bool)
  putStrLn . show $ i 
  return ()

main0 :: IO ()
main0 = do
  putStrLn "Trying to solve Ant problem!"
  aant <- heval progStr (undefined::AAnt)
  let ant = unAAnt aant
  putStrLn . show $ runAnt antWorld ant 
  getLine
  return ()


progStr2 = "ffAnt $ " ++ progStr 

progStr = "ifa m (p3 l (p2 (ifa m r ) (p2 r (p2 l r) ) ) (p2 (ifa m l) m) )"

