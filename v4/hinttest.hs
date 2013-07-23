import Heval (hevalsWith, heval)


main :: IO ()
main = do
 x <- heval "42" (42::Int)
 putStrLn . show $ x
 _ <- getChar
 return ()