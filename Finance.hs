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


import System.Directory
import Control.Monad
import  System.IO

import Data.Array

import qualified Data.Map as Map
import Data.Map (Map)

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

toCSV :: String -> Maybe CSV
toCSV str = case parseCSV "" str of
  Right csv -> Just csv
  Left _    -> Nothing

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

appleTest  = test "2011-12-01" "2011-12-30" "aapl"
googleTest = test "2011-01-05" "2011-01-10" "GOOG"

y_2011 fid = test "2011-01-01" "2011-12-31" fid

test from to fid = do
  putStrLn $ "fid : " ++ fid
  str <- getSource $ toYahooURL fid from to 
  case toCSV str of 
    Nothing -> putStrLn "Unavalible data (Probably)"
    Just csv -> do 
     let h = toHistory csv 
     putList h
     putList (dailyReturns h)
     putStrLn $ "# of days : " ++ show (length h)
     putStrLn $ "avgDailyRet : " ++ show (avgDailyRet h)
     putStrLn $ "stddevDailyRet : " ++ show (stddevDailyRet h)
     putStrLn $ "sharpeRatio : " ++ show (sharpeRatio h)


y2011 = ("2011-01-01","2011-12-31")
  
downloadDailyReturns :: (Date,Date) -> String -> IO ()
downloadDailyReturns (from,to) fid = do
 putStrLn fid 
 source <- getSource $ toYahooURL fid from to
 case toCSV source of
  Nothing -> putStrLn $ " KO : Unavalible data for : " ++ fid
  Just csv -> do
   let adjCloses = map snd . toHistory $ csv
   writeFile ("fin_data/"++fid++".txt") (show adjCloses)
   putStrLn " OK!"

getAllFrom_fin_data :: IO [(String,[Double])]
getAllFrom_fin_data = do
 let dir = "fin_data"
 names <- (tail . tail . reverse) `liftM` getDirectoryContents dir
 ret <- forM names $ \ name -> do
  fi <- openFile (dir ++ "/" ++ name ) ReadMode
  x  <- hGetLine fi
  hClose fi
  return (takeWhile (/='.') name,read x)
 return ret

type FID   = Int
type DayID = Int

data FinDB = FinDB (Map String FID) (Array (FID,DayID) Double)

mkFinDB :: IO FinDB
mkFinDB = do 
 let numDays = 252
 xs <- getAllFrom_fin_data
 let (names,valss) = unzip (filter (\(_,vs)->length vs == numDays) xs)
 let xs' = concatMap (\(fid,vals) -> [ ( (fid,day) , val ) | (day,val) <- (zip [1..] vals)  ]  ) (zip [1..] valss)
 return $ FinDB ( Map.fromList (zip names [1..]) ) ( array ( (1,1) , (length names,numDays) ) xs' )




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




fids :: [String]
fids = ["AAIT" , "ADNC" ,
 "AACC","AAME","AAON","AAPL","AAWW","AAXJ","ABAX",
 "ABCB","ABCD","ABCO","ABFS","ABIO","ABMD","ABTL","ABVA","ACAD","ACAS","ACAT",
 "ACCL","ACET","ACFC","ACFN","ACGL","ACIW","ACLS","ACNB","ACOM","ACOR",
 "ACPW","ACRX","ACTG","ACTS","ACUR","ACWI","ACWX","ACXM","ADAT",
 "ADBE","ADEP","ADES","ADI","ADP","ADRA","ADRD","ADRE",
 "ADRU","ADSK","ADTN","ADUS","ADVS","AEGN","AEGR","AEHR","AEIS","AEPI",
 "AERL","AETI","AEY","AEZS","AFAM","AFCB","AFCE","AFFX","AFFY",
 "AFOP","AFSI","AGEN","AGII","AGIIL","AGNC","AGNCP","AGYS","AHGP","AHPI",
 "ACHC","ACHN","AIMC","AINV","AIRM","AIRT","AIXG","AKAM","AKRX",
 "ALCO","ALCS","ALGN","ALGT","ALIM","ALKS","ALLB","ALLT","ALNC","ALNY",
 "ALOG","ALOT","ALRN","ALSK","ALTE","ALTI","ALTR","ALVR","ALXA",
 "ALXN","AMAG","AMAP","AMAT","AMBA","AMBT","AMCC","AMCF","AMCN","AMCX",
 "AMED","AMGN","AMIC","AMKR","AMNB","AMOT","AMOV","AMPE","AMPL",
 "AMRB","AMRI","AMRN","AMRS","AMSC","AMSF","AMSG","AMSWA","AMWD","AMZN",
 "ANAC","ANAD","ANAT","ANCB","ANCI","ANCX","ANDA","ANDAU","ANDAW","ANDE",
 "ANEN","ANGI","ANGO","ANIK","ANLY","ANNB","ANSS","ANTH","ANTP",
 "AONE","AOSL","APAGF","APEI","APFC","APKT","APOG","APOL","APPY","APRI",
 "APSA","APWC","ARAY","ARCC","ARCI","ARCP","ARCT","ARCW","ARDNA",
 "AREX","ARIA","ARII","ARKR","ARLP","ARMH","ARNA","AROW","ARQL",
 "ARRS","ARRY","ARTC","ARTNA","ARTW","ARTX","ARUN","ARWR","ASBB","ASBC",
 "ASBCW","ASBI","ASCA","ASCMA","ASEI","ASFI","ASIA","ASMI","ASML",
 "ASNA","ASPS","ASRV","ASRVP","ASTC","ASTE","ASTI","ASTM","ASTX",
 "ASUR","ASYS","ATAI","ATAX","ATEA","ATEC","ATHN","ATHX","ATLO","ATMI"
 ,"ATML","ATNI","ATNY","ATOS","ATRC","ATRI","ATRM","ATRO","ATRS",
 "ATSG","ATTU","ATVI","ATX","AUBN","AUDC","AUXL","AVAV","AVCA","AVEO"
 ,"AVGO","AVHI","AVID","AVNR","AVNW","AWAY","AWRE","AXAS","AXFN",
 "AXJS","AXTI","AZPN","BABY","BAGL","BAMM","BANC","BANCL","BANF",
 "BANFP","BANR","BASI","BBBY","BBCN","BBEP","BBGI","BBNK","BBOX",
 "BBRG","BBSI","BCBP","BCDS","BCOM","BCOR","BCOV","BCPC","BCRX",
 "BCSB","BDE","BDGE","BDMS","BDSI","BEAT","BEAV","BEBE","BECN","BELFA",
 "BELFB","BERK","BFIN","BFLY","BGCP","BGFV","BGMD","BGSC","BGSCU",
 "BGSCW","BHLB","BIB","BICK","BIDU","BIDZ","BIIB","BIOC","BIOD",
 "BIOF","BIOL","BIOS","BIRT","BIS","BJRI","BKCC","BKEP","BKEPP",
 "BKMU","BKOR","BKSC","BKYF","BLDP","BLDR","BLIN","BLKB","BLMN",
 "BLMT","BLRX","BMC","BMRC","BMRN","BMTC","BMTI","BNCL","BNCN",
 "BNSO","BOBE","BODY","BOFI","BOCH","BOKF","BOLT","BONA","BONT",
 "BOOM","BOSC","BOTJ","BOVA","BPAX","BPFH","BPFHW","BPHX","BPOP","BPOPM","BPOPN","BRCD","BRCM","BREW","BRID","BRKL","BRKR",
 "BRKS","BRLI","BSDM","BSET","BSFT","BSPM","BSQR","BSRR","BSTC","BTFG","BTUI","BUR","BUSE","BV","BVSN","BWEN","BWINA","BWINB",
 "BWLD","BWOW","BWOWU","BWOWW","BYFC","CA","CAAS","CAC","CACB","CACC","CADC","CADX","CAFI","CACH","CAKE","CALD","CALI","CALL",
 "CALM","CAMP","CAMT","CAR","CARB","CART","CARV","CARZ","CASH","CASM","CASS","CASY","CATM","CATY","CAVM","CBAK","CBAN","CBEY",
 "CBF","CBIN","CBLI","CBMX","CBMXW","CBNJ","CBNK","CBOE","CBOU","CBPO","CBRL","CBRX","CBSH","CBST","CCBG","CCCL","CCCLU","CCCLW",
 "CCIH","CCIX","CCMP","CCNE","CCOI","CCRN","CCRT","CCUR","CCXI","CDNS","CDTI","CDXS","CDZI","CEBK","CECE","CECO","CEDC","CEDU",
 "CELG","CELGZ","CEMI","CEMP","CENT","CENTA","CENX","CERE","CERN","CERP","CERS","CETV","CEVA","CFBK","CFFC","CFFI","CFFN","CFNB",
 "CFNL","CG","CGEI","CGEIU","CGEIW","CGEN","CGNX","CGO","CIDM","CIEN","CIMT","CINF","CISG","CITZ","CIZN","CJJD","CKEC","CKSW","CLBH",
 "CLCT","CLDX","CLFD","CLIR","CLMS","CLMT","CLNE","CLNT","CLRO","CLRX","CLSN","CLUB","CLVS","CLWR","CLWT","CMCO","CMCSA","CMCSK","CME",
 "CMGE","CMLS","CMRG","CMSB","CMTL","CMVT","CMVTV","CNBC","CNBKA","CNDO","CNET","CNIT","CNMD","CNQR","CNSIV","CNSL","CNTF",
 "CNTY","CNYD","COBK","COBR","COBZ","COCO","COGO","COHR","COHU","COKE","COLB","COLM","CONN","COOL","CORE","CORT","COSI"]

fids_part2 = ["COST","COWN","CPAH","CPBC","CPGI","CPHC","CPHD","CPIX","CPLA","CPLP","CPNO","CPRT","CPRX","CPSI","CPSL","CPSS","CPST",
 "CPTS","CPWR","CRAI","CRAY","CRBC"]

fids_part3 = ["CRDC","CRDN","CRDS","CREE","CREG","CRESW","CRESY","CRFN","CRIS","CRMB","CRMBU","CRMBW",
 "CRME","CRMT","CRNT","CROX","CRRB","CRRC","CRTX","CRUS","CRVL","CRWN","CRWS","CRZO","CSBK","CSCD","CSCO","CSFL","CSGP",
 "CSGS","CSII","CSIQ","CSOD","CSPI","CSQ","CSRE","CSTE","CSTR","CSUN","CSWC","CTAS","CTBI","CTCM","CTCT","CTDC","CTEL",
 "CTFO","CTGX","CTHR","CTCH","CTIB","CTIC","CTRN","CTRP","CTRX","CTSH","CTWS","CTXS","CU","CUBA","CUI","CUNB","CUTR",
 "CVBF","CVCO","CVCY","CVGI","CVGW","CVLT","CVLY","CVTI","CVV","CWBC","CWCO","CWEI","CWST","CWTR","CXDC","CXPO","CY",
 "CYAN","CYBE","CYBI","CYBX","CYCC","CYCCP","CYMI","CYNO","CYOU","CYTK","CYTR","CYTX","CYTXW","CZFC","CZNC","CZR","CZWI",
 "DAEG","DAIO","DAKT","DARA","DATE","DAVE","DBLE","DBLEP","DCIN","DCIX","DCOM","DCTH","DECK","DEER","DELL","DENN","DEPO",
 "DEST","DFR","DFRG","DFZ","DGAS","DGICA","DGICB","DGII","DGIT","DGLY","DHFT","DHIL","DHRM","DIAL","DIOD","DISCA","DISCB",
 "DISCK","DISH","DITC","DJCO","DLGC","DLHC","DLIA","DLLR","DLTR","DMLP","DMND","DMRC","DNBF","DNDN","DNKN","DORM","DOVR",
 "DRAD","DRAM","DRCO","DRIV","DRRX","DRTX","DRWI","DRYS","DSCI","DSCO","DSGX","DSPG","DSTI","DSWL","DTLK","DTSI","DTV","DUSA",
 "DVAX","DVOX","DWA","DWCH","DWSN","DXCM","DXPE","DXYN","DYAX","DYNT","DYSL","EA","EAC","EAGL","EAGLU","EAGLW","EBAY","EBIX",
 "EBMT","EBSB","EBTC","ECOL","ECPG","ECTE","ECTY","ECYT","EDAC","EDAP","EDGW","EDMC","EDS","EDUC","EEFT","EEI","EEMA","EEME",
 "EEML","EFII","EFSC","EFUT","EGAN","EGBN","EGHT","EGLE","EGOV","EGRW","EHTH","ECHO","EIHI","ELGX","ELNK","ELON","ELOQ","ELOS",
 "ELRC","ELSE","ELTK","EMCB","EMCF","EMCI","EMDI","EMEY","EMFN","EMIF","EMITF","EMKR","EML","EMMS","EMMSP","EMMT","ENDP","ENG",
 "ENMD","ENOC","ENPH","ENSG","ENTG","ENTR","ENVI","ENZN","EONC","EOPN","EPAX","EPAY","EPHC","EPIQ","EPOC","EQIX","ERIC","ERIE",
 "ERII","EROC","ESBF","ESBK","ESCA","ESEA","ESGR","ESIO","ESLT","ESMC","ESRX","ESSA","ESSX","ESYS","ETFC","ETRM","EUFN","EVAC",
 "EVAL","EVBS","EVEP","EVOL","EWBC","EXA","EXAC","EXAR","EXAS","EXEL","EXFO","EXLP","EXLS","EXPD","EXPE","EXPO","EXTR","EXXI",
 "EZCH","EZPW","FABK","FACE","FALC","FANG","FARM","FARO","FAST","FB","FBIZ","FBMI","FBMS","FBNC","FBNK","FBRC","FBSS","FCAL","FCAP",
 "FCBC","FCCO","FCCY","FCEL","FCFC","FCFS","FCLF","FCNCA","FCTY","FCVA","FCZA","FDEF","FDML","FDUS","FEFN","FEIC","FEIM","FELE","FES",
 "FFBC","FFBCW","FFBH","FFCO","FFEX","FFHL","FFCH","FFIC","FFIN","FFIV","FFKT","FFKY","FFN","FFNM","FFNW","FHCO","FCHI","FIBK","FINL",
 "FIRE","FISI","FISV","FITB","FITBP","FIVE","FIZZ","FLDM","FLEX","FLIC","FLIR","FLML","FLOW","FLWS","FLXS","FMBI","FMCN","FMER","FMFC",
 "FMNB","FNBN","FNFG","FNGN","FNHC","FNLC","FNSR","FOLD","FONE","FONR","FORD","FORM","FORR","FORTY","FOSL","FRAN","FRBK","FRED","FREE",
 "FRGI","FRME","FRNK","FRP","FSBI","FSBK","FSBW","FSC","FSCI","FSFG","FSGI","FSIN","FSLR","FSRV","FSTR","FSYS","FTEK","FTNT","FTR",
 "FUBC","FULL","FULT","FUNC","FUND","FURX","FWLT","FWRD","FXCB","FXEN","GABC","GAGA","GAI","GAIA","GAIN","GAINP","GALE","GALT",
 "GALTU","GALTW","GAME","GASS","GBCI","GBDC","GBIM","GBLI","GBNK","GCBC","GCFB","GCOM","GCVRZ","GENC","GENE","GENT","GEOS","GEOY",
 "GERN","GEVA","GEVO","GFED","GFN","GFNCL","GFNCZ","GGAL","GHDX","GIFI","GIGA","GIGM","GIII","GILD","GILT","GIVN","GKNT","GKSR",
 "GLAD","GLADP","GLBS","GLBZ","GLDC","GLDD","GLCH","GLNG","GLPW","GLRE","GLUU","GMAN","GMCR","GMETP","GMLP","GNCMA","GNMA","GNMK",
 "GNOM","GNTX","GNVC","GOLD","GOOD","GOODN","GOODO","GOODP","GOOG","GPIC","GPOR","GPRC","GPRE","GRFS","GRID","GRIF","GRMH","GRMN",
 "GROW","GRPN","GRVY","GSAT","GSBC","GSIG","GSIT","GSJK","GSM","GSOL","GSVC","GTAT","GTIM","GTIV","GTLS","GTWN","GTXI","GUID","GULF",
 "GURE","GYRO","HA","HAFC","HAIN","HALL","HALO","HARL","HAS","HAST","HAUP","HAVNP","HAYN","HBAN","HBANP","HBCP","HBHC","HBIO","HBK",
 "HBMD","HBNC","HBNK","HBOS","HCBK","HCCI","HCII","HCIIP","HCIIW","HCKT","HCOM","HCSG","HDNG","HDSN","HEAT","HEES","HELE","HEOP","HERO",
 "HFBC","HFBL","HFFC","HFWA","HGSH","HIBB","HIFS","HIHO","HILL","HIMX"]

fids_part4 = ["HITK","HITT","HLIT","HLSS","HLYS","HMIN","HMNF","HMNY","HMPR",
 "HMST","HMSY","HNH","HNRG","HNSN","HOFT","HOGS","HOLI","HOLL","HOLX","HOMB","HOME","HOTR","HOTRW","HOTT","HOVNP","HPAC","HPCCP","HPJ",
 "HPOL","HPTX","HRZN","HSFT","HSIC","HSII","HSKA","HSNI","HSOL","HSON","HSTM","HTBI","HTBK","HTCO","HTHT","HTCH","HTLD","HTLF","HTWR",
 "HUBG","HURC","HURN","HWAY","HWBK","HWCC","HWKN","HYGS","HZNP","CHCI","CHCO","CHDN","CHDX","CHEF","CHEV","CHFC","CHFN","CHI","CHKE",
 "CHKP","CHLN","CHNR","CHOP","CHRM","CHRW","CHSCP","CHTP","CHTR","CHUY","CHW","CHXF","CHY","CHYR","IACI","IART","IBB","IBCA","IBCP",
 "IBCPO","IBKC","IBKR","IBOC","ICAD","ICCC","ICFI","ICGE","ICLN","ICLR","ICON","ICPT","ICUI","IDCC","IDIX","IDRA","IDSA","IDSY","IDTI",
 "IDXX","IEP","IESC","IFAS","IFEU","IFGL","IFNA","IFON","IFSIA","IFSM","IGLD","IGOI","IGOV","IGTE","III","IIIN","IIJI","IILG","IIN",
 "IIVI","IKAN","IKNX","ILMN","IMGN","IMI","IMKTA","IMMR","IMMU","IMOS","IMRS","INAP","INCY"]

fids_part5 = ["INDB","INDY","INFA","INFI",
 "INFN","INFY","ININ","INOC","INOD","INPH","INSM","INTC","INTG","INTL","INTT","INTU","INTX","INVE","INWK","INXB","INXBU",
 "INXBW","IOSP","IPAR","IPAS","IPCC","IPCI","IPCM","IPGP","IPHS","IPXL","IQNT","IRBT","IRDM","IRDMU","IRDMW","IRDMZ","IRET",
 "IRETP","IRG","IRIS","IRIX","IROQ","IRWD","ISBC","ISCA","ISHG","ISIG","ISIL","ISIS","ISLE","ISM","ISNS","ISRG","ISRL","ISSC",
 "ISSI","ITIC","ITMN","ITRI","ITRN","IVAC","IVAN","IXYS","JACK","JACQU","JADE","JAKK","JASO","JAX","JAXB","JAZZ","JBHT","JBLU",
 "JBSS","JCOM","JCS","JCTCF","JDAS","JDSU","JFBI","JIVE","JJSF","JKHY","JMBA","JOBS","JOEZ","JOSB","JOUT","JRCC","JRJC","JSM",
 "JST","JVA","JXSB","KALU","KBALB","KCAP","KCLI","KELYA","KELYB","KEQU","KERX","KEYN","KEYW","KFFB","KFFG","KFRC","KGJI","KINS",
 "KIOR","KIPO","KIPS","KIRK","KITD","KLAC","KLIC","KNDI","KONA","KONE","KONG","KOOL","KOPN","KOSS","KRFT","KRNY","KSWS","KTCC",
 "KTEC","KTOS","KUTV","KVHI","KYAK","KYTH","LABC","LABL","LACO","LAKE","LAMR","LANC","LARK","LAWS","LAYN","LBAI","LBIX","LBTYA",
 "LBTYB","LBTYK","LCAV","LCNB","LCUT","LEAP","LECO","LEDR","LEDS","LFUS","LFVN","LGCY","LGND","LHCG","LIFE","LIME","LINC","LINE",
 "LINTA","LINTB","LION","LIOX","LIVE","LIWA","LKFN","LKQ","LLEN","LLNW","LLTC","LMAT","LMCA","LMCB","LMIA","LMLP","LMNR","LMNX",
 "LMOS","LNBB","LNCE","LNCO","LNDC","LNET","LOAN","LOCM","LOGI","LOGM","LOJN","LONG","LOOK","LOPE","LORL","LPHI","LPLA","LPNT",
 "LPSBD","LPSN","LPTH","LPTN","LQDT","LRAD","LRCX","LSBI","LSBK","LSCC","LSTR","LTBR","LTON","LTRE","LTRX","LTXC","LUFK","LULU",
 "LUNA","LVNTA","LVNTB","LWAY","LXRX","LYTS","MACK","MAG","MAGS","MAKO","MALL","MANH","MANT","MAPP","MARK","MARPS","MASC","MASI",
 "MAT","MATR","MATW","MAXY","MAYS","MBFI","MBLX","MBND","MBRG","MBTF","MBVT","MBWM","MCBC","MCBI","MCBK","MCEP","MCGC","MCOX",
 "MCRI","MCRL","MCRS","MDAS","MDCA","MDCI","MDCO","MDH","MDIV","MDLZ","MDRX","MDSO","MDVN","MEAD","MEAS","MEDW","MEILU","MEIP",
 "MELA","MELI","MEMP","MEMS","MENT","MEOH","MERC","MERU","METR","MFI","MFLR","MFLX","MFNC","MFRI","MFRM","MFSF","MGAM","MGCD",
 "MGEE","MGIC","MGLN","MGPI","MGRC","MGYR","MHGC","MHLD","MCHP","MCHX","MIDD","MIND","MINI","MIPS","MITK","MITL","MKSI","MKTAY",
 "MKTG","MKTX","MLAB","MLHR","MLNK","MLNX","MLVFD","MMLP","MMSI","MMUS","MMYT","MNDO","MNGA","MNGL","MNGLU","MNGLW",
 "MNKD","MNOV","MNRK","MNRKP","MNRO","MNST","MNTA","MNTG","MNTX","MOBI","MOCO","MOFG","MOLX","MOLXA","MORN","MOSY","MOTR","MOVE",
 "MPAA","MPAC","MPB","MPEL","MPET","MPWR","MRCY","MRGE","MRLN","MRTN","MRVL","MSBF","MSCC","MSEX","MSFG","MSFT","MSG","MSLI","MSON",
 "MSPD","MSTR","MSW","MTEX","MTGE","MTRX","MTSC","MTSI","MTSL","MTSN","MU","MVIS","MVISW","MWIV","MXIM","MXWL","MYGN","MYL","MYRG",
 "MYRX","NABI","NAFC","NAII","NANO","NASB","NATH","NATI","NATL","NATR","NAUH","NAVG","NAVR","NBBC","NBIX","NBN","NBTB","NBTF","NCBC",
 "NCIT","NCMI","NCTY","NDAQ","NDSN","NEBS","NECB","NEOG","NEON","NEPT","NETC","NETE","NEWL","NEWP","NEWS","NEWT","NEXS","NFBK","NFEC",
 "NFLX","NFSB","NGPC","NHTB","NICE","NICK","NIHD","NILE","NINE","NKBP","NKSH","NKTR","NLNK","NLST","NMAR","NMARW","NMRX","NNBR","NOOF",
 "NOVB","NPBC","NPBCO","NPSP","NRCI","NRIM","NSEC","NSIT","NSPH","NSSC","NSYS","NTAP","NTCT","NTES","NTGR","NTIC","NTK","NTLS","NTRI",
 "NTRS","NTSC","NTSP","NTWK","NUAN","NUCL","NURO","NUTR","NUVA","NVAX","NVDA","NVDQ","NVEC","NVGN","NVMI","NVSL","NVTL","NWBI","NWFL",
 "NWLI","NWPX","NWS","NWSA","NXPI","NXST","NXTM","NYMT","NYMX","NYNY","OABC","OBAF","OBAS","OBCI","OCC","OCFC","OCLR","OCLS","OCZ",
 "ODFL","OFED","OFIX","OFLX","OGXI","OIIM","OINK","OKSB","OKSBP","OLBK","OMAB","OMCL","OMER","OMEX","OMPI","ONCY","ONEQ","ONFC",
 "ONNN","ONTY","ONVI","ONXX","OPAY","OPEN","OPHC","OPLK","OPNT","OPOF","OPTR","OPTT","OPXA","OPXAW","ORBC","ORBK","ORBT","ORCC",
 "ORCL","OREX","ORIG","ORIT","ORLY","ORRF","OSBC","OSBCP","OSH","OSHC","OSIR","OSIS","OSM","OSN","OSTK","OSUR","OTEX","OTIV","OTT",
 "OTTR","OUTD","OVBC","OVLY","OVRL","OVTI","OXBT","OXGN","OXLC","OZRK","PAAS","PACB","PACQ","PACQU","PACQW","PACR","PACW","PAGG",
 "PAMT","PANL","PATH","PATK","PATR","PAYX","PBCT","PBHC","PBIB","PBIP","PBMD","PBSK","PCAR","PCBC","PCBK","PCCC","PCLN","PCO",
 "PCOM","PCRX","PCTI","PCYC","PCYO","PDCE","PDCO","PDEX","PDFS","PDII","PDLI","PEBK","PEBO","PEET","PEGA","PEIX","PENN","PENX",
 "PEOP","PERF","PERI","PERY","PESI","PETM","PETS","PFBC","PFBI","PFBX","PFIN","PFLT","PFMT","PFPT","PFSW","PGC","PGNX","PGRX",
 "PGTI","PHII","PHIIK","PHMD","PCH","PICO","PKBK","PKOH","PKOL","PKT","PLAB","PLBC","PLCC","PLCE","PLCM","PLFE","PLMT","PLNR",
 "PLPC","PLTM","PLUG","PLUS","PLXS","PLXT","PMBC"]

fids_part6 = ["PMCS","PMD","PMFG","PMNA","PMTC","PMTI","PNBK","PNFP","PNNT","PNQI","PNRA",
 "PNRG","PNTR","PODD","POOL","POPE","POWI","POWL","POWR","POZN","PPBI","PPHM","PRAA","PRAN","PRCP","PRFT","PRFZ","PRGO","PRGS",
 "PRGX","PRIM","PRKR","PRLS","PRMW","PROV","PRPH","PRSC","PRSS","PRST","PRTS","PRWT","PRXI","PRXL","PSAU","PSBH","PSCC","PSCD",
 "PSCE","PSCF","PSCI","PSCM","PSCT","PSCU","PSDV","PSEC","PSEM","PSCH","PSMI","PSMT","PSOF","PSSI","PSTB","PSTI","PSTL","PSTR",
 "PSUN","PTEK","PTEN","PTIE","PTIX","PTNR","PTNT","PTRY","PTSI","PTSX","PULB","PURE","PVFC","PVSW","PVTB","PVTBP","PWAV","PWER",
 "PWND","PWOD","PWRD","PWX","PXLW","PZZA","PZZI","QABA","QADA","QADB","QBAK","QCCO","QCLN","QCOM","QCOR","QCRH","QDEL","QGEN",
 "QKLS","QLGC","QLIK","QLTI","QLTY","QLYS","QNST","QQEW","QQQ","QQQC","QQQX","QQXT","QSII","QTEC","QTWW","QUIK","RADA","RAIL",
 "RAND","RAVN","RBCAA","RBCN","RBNF","RBPAA","RCII","RCKB","RCKY","RCMT","RCON","RDA","RDCM","RDEN","RDI","RDIB","RDNT","RDWR",
 "RECN","RECV","REDF","REED","REFR","REGI","REGN","REIS","RELL","RELV","RENT","REXI","REXX","RFIL","RFMD","RGCO","RGDX","RGEN",
 "RGLD","RGLS","RICK","RIGL","RIMG","RIMM","RITT","RIVR","RJET","RLJE","RLOC","RLOG","RMBS","RMCF","RMKR","RMTI","RMTR","RNET",
 "RNIN","RNST","RNWK","ROCK","ROCM","ROIA","ROIAK","ROIC","ROICU","ROICW","ROIQ","ROIQU","ROIQW","ROLL","ROMA","ROSE","ROSG",
 "ROST","ROVI","ROYL","RP","RPRX","RPRXW","RPRXZ","RPTP","RPXC","RRD","RRGB","RRST","RSOL","RSTI","RSYS","RTEC","RTIX","RTLX",
 "RUE","RUSHA"]

fids_part7 = ["RUSHB","RUTH","RVBD","RVSB","RYAAY","SAAS","SABA","SAFM","SAFT","SAIA","SALM","SANM","SANW","SANWW","SANWZ",
 "SAPE","SASR","SATC","SATS","SAVB","SAVE","SBAC","SBBX","SBCF","SBGI","SBLK","SBNY","SBNYW","SBRA","SBSA","SBSI","SBUX",
 "SCBT","SCGQ","SCLN","SCLP","SCMFO","SCMP","SCMR","SCOG","SCOK","SCON","SCOR","SCSC","SCSS","SCVL","SDBT","SDIX","SEAC",
 "SEED","SEIC","SENEA","SENEB","SEV","SFBC","SFLY","SFNC","SFST","SGC","SGEN","SGGG","SGI","SGMA","SGMO","SGMS","SGNT",
 "SGOC","SGRP","SGYP","SGYPU","SGYPW","SHBI","SHEN","SHFL","SHIP","SHLD","SHLM","SHLO","SHOO","SHOR","SHOS","SHPG","SCHL",
 "SCHN","SCHS","SIAL","SIBC","SIEB","SIFI","SIFY","SIGA","SIGI","SIGM","SILC","SIMG","SIMO","SINA","SINO","SIRI","SIRO",
 "SIVB","SIVBO","SKBI","SKUL","SKYW","SKYY","SLAB","SLGN","SLM","SLMAP","SLMBP","SLP","SLRC","SLTC","SLTM","SLXP","SMBC",
 "SMBL","SMCI","SMED","SMIT","SMMF","SMRT","SMSI","SMT","SMTC","SMTX","SNAK","SNBC","SNCR","SNDK","SNFCA","SNHY","SNMX","SNPS",
 "SNSS","SNTA","SNTS","SOCB","SOCL","SODA","SOFO","SOHU","SOMH","SOMX","SONA","SONC","SONS","SORL","SOXX","SPAN","SPAR","SPBC",
 "SPEX","SPCHA","SPCHB","SPIL","SPIR","SPLK","SPLS","SPMD","SPNC","SPNS","SPPI","SPPR","SPPRO","SPPRP","SPRD","SPRO","SPRT",
 "SPSC","SPTN","SPU","SPWR","SQI","SQNM","SQQQ","SRCE","SRCL","SRDX","SREV","SRPT","SSBI","SSFN","SSH","SSNC","SSRI","SSRX",
 "SSYS","STAA","STAN","STB","STBA","STBZ","STEC","STEI","STEL","STEM","STFC","STKL","STLD","STLY","STMP","STND","STNR","STRA",
 "STRL","STRM","STRN","STRS","STRT","STSA","STSI","STX","STXS","SUBK","SUMR","SUNH","SUNS","SUPN","SUPX","SURG","SUSQ","SUSS",
 "SUTR","SVA","SVBI","SVNT","SVVC","SWHC","SWIR","SWKS","SWSH","SYBT","SYBTP","SYKE","SYMC","SYMM","SYMX","SYNA","SYNC","SYNL",
 "SYNM","SYNT","SYPR","SYUT","SZYM","TACT","TAIT","TASR","TAST","TATT","TAX","TAXI","TAYC","TAYCP","TAYD","TBAC","TBBK","TBNK",
 "TBOW","TCBI","TCBIL","TCBIW","TCBK","TCCO","TCPC","TCRD","TDIV","TEAR","TECD","TECUA","TECUB","TECH","TELK","TESO","TESS","TFCO",
 "TFM","TFSL","TGA","TGE","THER","THFF","THLD","THOR","THQI","THRD","THRM","THRX","THTI","TIBX","TICC","TIGR","TINY","TISA",
 "TITN","TIVO","TKMR","TLAB","TLF","TMNG","TNAV","TNGO","TOFC","TOPS","TORM","TOWN","TPCG","TPGI","TQNT","TQQQ","TRAK","TREE",
 "TRGT","TRIB","TRIO","TRIP","TRIT","TRLG","TRMB","TRMD","TRMK","TRNS","TRNX","TROV","TROVU","TROVW","TROW","TRS","TRST","TSBK",
 "TSCO","TSEM","TSLA","TSON","TSPT","TSRA","TSRI","TSRO","TSRX","TST","TSTC","TSYS","TTEC","TTEK","TTGT","TTHI","TTMI","TTS",
 "TTWO","TUES","TWER","TWGP","TWIN","TWMC","TWTC","TXCC","TXN","TXRH","TYPE","TZOO","TZYM","UACL","UBCP","UBFO","UBNK","UBNT",
 "UBOH","UBPS","UBPSU","UBPSW","UBSH","UBSI","UCBA","UCBI","UCFC","UCTT","UDRL","UEIC","UEPS","UFCS","UFPI","UFPT","UG",
 "UHAL","ULBI","ULGX","ULTA","ULTI","ULTR","UMBF","UMPQ","UNAM","UNB","UNFI","UNIS","UNTD","UNTK","UNTY","UNXL","UPI","UPIP",
 "URBN","URRE","USAK","USAP","USAT","USATP","USATZ","USBI","USCR","USEG","USHS","USLM","USMD","USMO","USTR","UTEK","UTHR",
 "UTIW","UTMD","UTSI","UVSP","VALU","VALV","VASC","VBFC","VCBI","VCIT","VCLK","VCLT","VCSH","VDSI","VECO","VELT","VGIT",
 "VGLT","VGSH","VIA","VIAB","VIAS","VICL","VICR","VIDE","VIFL","VIMC","VIRC","VISN","VITC","VIVO","VLCCF","VLGEA","VLTR",
 "VLYWW","VMBS","VMED","VNDA","VNET","VNQI","VOCS","VOD","VOLC","VONE","VONG","VONV","VOXX","VPFG","VPHM","VPRT","VRA","VRML",
 "VRNM","VRNT","VRSK","VRSN","VRTA","VRTB","VRTS","VRTU","VRTX","VSAT","VSBN"]

fids_part8 = ["VSCI","VSCP","VSEC","VSNT","VSTM","VTHR","VTIP",
 "VTNC","VTSS","VTUS","VTWG","VTWO","VTWV","VVTV","VVUS","VXUS","VYFC","WABC","WACLY","WAFD","WAFDW","WASH","WAVX","WAYN","WBCO",
 "WBKC","WBMD","WBSN","WCBO","WCRX","WDC","WDFC","WEBK","WEBM","WEN","WERN","WETF","WEYS","WFD","WFM","WIBC","WIFI","WILC","WILN",
 "WIN","WINA","WIRE","WLB","WLBPZ","WLDN","WLFC","WLFCP","WMAR","WMGI","WOOD","WOOF","WPCS","WPPGY","WPRT","WRES","WRLD","WRLS",
 "WSB","WSBC","WSBF","WSCI","WSFS","WSFSL","WSTG","WSTL","WTBA","WTFC","WTFCW","WTSLA","WVFC","WVVI","WWAY","WWD","WWIN",
 "WWVY","WWWW","WYNN","XBKS","XIDE","XLNX","XNPT","XOMA","XRAY","XRSC","XRTX","XTEX","XTXI","XWES","XXIA","YAVY","YDNT",
 "YHOO","YNDX","YOD","YONG","YORW","YRCW","YTEC","Z","ZAGG","ZAZA","ZBRA","ZEUS","ZGNX","ZHNE","ZIGO","ZINC","ZION",
 "ZIONW","ZIOP","ZIP","ZIPR","ZIXI","ZLCS","ZLTQ","ZN","ZNGA","ZNWAW","ZNWAZ","ZOLT","ZOOM","ZUMZ"]

fids_extras = ["GLD","FB"]