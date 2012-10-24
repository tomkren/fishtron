{-# LANGUAGE BangPatterns #-}

module Finance (

) where

import Data.Maybe
import Data.List
import Data.Time
import Data.Char
import Text.CSV

import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import Network

import qualified Data.ByteString.Lazy as BS


import Util

type URL = String

getSource :: URL -> IO String
getSource url = withSocketsDo $ do
     request' <- parseUrl url
     let request = request' { checkStatus = \_ _ -> Nothing }
     res <- withManager $ httpLbs request
     return . myUnpack . responseBody $ res

myUnpack :: BS.ByteString -> String
myUnpack = map chr . (read :: String -> [Int]) . show . BS.unpack

toCSV :: String -> CSV
toCSV str = let Right csv = parseCSV "" str in csv

toHistoryRec :: Record -> HistoryRec
toHistoryRec [date,open,high,low,close,volume,adjClose] = ( date , read adjClose )

toHistory :: CSV -> History
toHistory = reverse . tail . init . map toHistoryRec

toYahooURL :: FirmaID -> From -> To -> URL
toYahooURL fid datum1 datum2 = 
  "http://ichart.finance.yahoo.com/table.csv?"++
   "s=" ++ fid ++ "&" ++
   "a=" ++ m1 ++ "&" ++
   "b=" ++ d1 ++ "&" ++
   "c=" ++ y1 ++ "&" ++
   "d=" ++ m2 ++ "&" ++
   "e=" ++ d2 ++ "&" ++
   "f=" ++ y2 ++ "&" ++
   "g=d" 
 where
  (y1,m1,d1) = toDatum datum1 
  (y2,m2,d2) = toDatum datum2

toDatum :: String -> Datum
toDatum [c1,c2,c3,c4,_,c6,c7,_,c9,c10] = ([c1,c2,c3,c4], month [c6,c7] ,[c9,c10])
 where
  month :: String -> String
  month = show . (+(-1)) . (read :: String -> Int ) 


dailyReturns :: History -> [Double]
dailyReturns [] = []
dailyReturns [_] = []  
dailyReturns h = 
  let (d:rest) = map snd h
   in 0 : dailyReturns' d rest  -- ta nula je tam aby to bylo jako v tom tutoriale i když myslim že je to blbost
 where 
  dailyReturns' _ [] = []
  dailyReturns' d1 (d2:rest) = ((d2/d1) - 1) : dailyReturns' d2 rest

sharpeRatio :: History -> Double
sharpeRatio h = sqrt(250) * (mean drs) / (stdeva drs) 
 where drs = dailyReturns h

avgDailyRet :: History -> Double
avgDailyRet = mean . dailyReturns 

stddevDailyRet :: History -> Double
stddevDailyRet = stdeva . dailyReturns

mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

-- z Excelu
stdeva :: (Floating a) => [a] -> a 
stdeva xs = sqrt( ( n*(sum (map (^2) xs) ) - (sum xs)^2 ) / ( n*(n-1) ) )
 where n = fromIntegral $ length xs



--  "http://ichart.finance.yahoo.com/table.csv?s=GOOG&a=00&b=10&c=2012&d=00&e=25&f=2012&g=d&ignore=.csv"

appleTest  = test "aapl" "2011-12-01" "2011-12-30"
googleTest = test "GOOG" "2011-01-05" "2011-01-10"

test fid from to = do
  str <- getSource $ toYahooURL fid from to 
  let h = toHistory . toCSV $ str
  putList h
  putList (dailyReturns h)
  putStrLn $ "# of days : " ++ show (length h)
  putStrLn $ "avgDailyRet : " ++ show (avgDailyRet h)
  putStrLn $ "stddevDailyRet : " ++ show (stddevDailyRet h)
  putStrLn $ "sharpeRatio : " ++ show (sharpeRatio h)

  

type FirmaID = String 
type From    = String
type To      = String

type Datum   = (Ye,Mo,De)
type Ye = String
type Mo = String
type De = String 


data Firma = Firma

type Date     = String --UTCTime
type AdjClose = Double

type HistoryRec = (Date,AdjClose)

type History = [HistoryRec]



----------------------------

stddev :: (Floating a) => [a] -> a
stddev xs = sqrt $ var xs

stddevp :: (Floating a) => [a] -> a
stddevp xs = sqrt $ pvar xs

pvar :: (Floating a) => [a] -> a
pvar xs = centralMoment xs 2

var xs = (var' 0 0 0 xs) / (fromIntegral $ length xs - 1)
    where
      var' _ _ s [] = s
      var' m n s (x:xs) = var' nm (n + 1) (s + delta * (x - nm)) xs
         where
           delta = x - m
           nm = m + delta/(fromIntegral $ n + 1)

centralMoment :: (Floating b, Integral t) => [b] -> t -> b
centralMoment xs 1 = 0
centralMoment xs r = (sum (map (\x -> (x-m)^r) xs)) / n
    where
      m = mean xs
      n = fromIntegral $ length xs
