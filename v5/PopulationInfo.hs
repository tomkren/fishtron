module PopulationInfo 
( PopInfo(..)
, XOLType(..)
, initPopInfo, resetPopInfo
, popInfoToJSON
, inc_xo_ok, inc_xo_tooBig, inc_xo_fail, inc_rep, inc_elite, inc_nextRC
, popi_setBest, popi_setPop, popi_addXOL
) where

import Text.JSON (JSValue)
import JSONUtils (jsArr,jsObj,jsStr,jsNum)
import TTree     (CTT(..), TTree(..))
import TTerm     (Context)
import Utils     (JShow, jshow_popi, jshow_rc)

data PopInfo = PopInfo {
    popi_xo_ok     :: Int,
    popi_xo_tooBig :: Int,
    popi_xo_fail   :: Int,
    popi_rep       :: Int,
    popi_elite     :: Int,
    popi_best      :: JSValue,
    popi_pop       :: [JSValue],
    popi_xos       :: [XOLog],
    popi_nextRC    :: Int
  } deriving Show

initPopInfo :: PopInfo
initPopInfo = PopInfo {
  popi_xo_ok     = 0,
  popi_xo_tooBig = 0,
  popi_xo_fail   = 0,
  popi_rep       = 0,
  popi_elite     = 0,
  popi_best      = jsObj [],
  popi_pop       = [],
  popi_xos       = [],
  popi_nextRC    = 0
}

resetPopInfo :: PopInfo -> PopInfo
resetPopInfo popi = initPopInfo{ popi_nextRC = popi_nextRC popi }

-- staryý... obsahuje info o jednom jedinci
--data IndivRecord = XoverIR
--type XoverFailed = Bool

mkJsIndiv :: (JShow term) => (term,Double) -> JSValue
mkJsIndiv (term, fitVal) = jsObj [
  ("term"  , jshow_popi term ),
  ("fitval", jsNum fitVal    )]

mkJsFitvalLog :: (JShow term) => (term,Double) -> JSValue
mkJsFitvalLog (term, fitVal) = jsObj [
  ("id"    , jsNum $ jshow_rc term ),
  ("fitval", jsNum fitVal    )]

data XOLog = XOLog {
    xol_type  :: (XOLType,XOLType),
    xol_tata  :: JSValue,
    xol_mama  :: JSValue,
    xol_syn   :: JSValue,
    xol_dcera :: JSValue,
    xol_pos1  :: [Int],
    xol_pos2  :: [Int]
  } deriving Show

data XOLType = XOL_ok | XOL_fail | XOL_tooBig

instance Show XOLType where
  show x = case x of
   XOL_ok     -> "ok" 
   XOL_fail   -> "fail"
   XOL_tooBig -> "tooBig"

xolToJson :: XOLog -> JSValue
xolToJson xol = jsObj [
 ( "type"  , xoltToJson $ xol_type  xol ),
 ( "tata"  ,              xol_tata  xol ),
 ( "mama"  ,              xol_mama  xol ),
 ( "syn"   ,              xol_syn   xol ),
 ( "dcera" ,              xol_dcera xol ),
 ( "pos1"  ,  posToJson $ xol_pos1  xol ),
 ( "pos2"  ,  posToJson $ xol_pos2  xol )]

posToJson :: [Int] -> JSValue
posToJson = jsArr . map jsNum

xoltToJson :: (XOLType,XOLType) -> JSValue
xoltToJson (t1,t2) = jsArr $ map (jsStr . show) [t1,t2]

popi_addXOL :: (JShow term) => (XOLType,XOLType) -> [term] -> ([Int],[Int]) -> PopInfo -> PopInfo
popi_addXOL xolt [t,m,s,d] (pos1,pos2) popi = 
 popi{ popi_xos = xol:(popi_xos popi) }
  where xol = XOLog{
        xol_type  = xolt         ,
        xol_tata  = jshow_popi t ,
        xol_mama  = jshow_popi m ,
        xol_syn   = jshow_popi s ,
        xol_dcera = jshow_popi d ,
        xol_pos1  = pos1         ,
        xol_pos2  = pos2         }

popi_setPop :: (JShow term) => [(term,Double)] -> PopInfo -> PopInfo
popi_setPop xs popi = popi{ popi_pop = map mkJsFitvalLog xs } -- dříve tam bylo: mkJsIndiv --takle uspornejsi




popi_setBest :: (JShow term) => term -> Double -> PopInfo -> PopInfo
popi_setBest term fitVal popi = popi{ popi_best = jsObj [
 ("term",   jshow_popi term),
 ("fitval", jsNum fitVal)] } 


-- :: PopInfo -> PopInfo
inc_xo_ok     = popiInc popi_xo_ok     set_xo_ok
inc_xo_tooBig = popiInc popi_xo_tooBig set_xo_tooBig
inc_xo_fail   = popiInc popi_xo_fail   set_xo_fail
inc_rep       = popiInc popi_rep       set_rep
inc_elite     = popiInc popi_elite     set_elite
inc_nextRC    = popiInc popi_nextRC    set_nextRC

-- :: PopInfo -> Int -> PopInfo
set_xo_ok     popi i = popi { popi_xo_ok     = i }  
set_xo_tooBig popi i = popi { popi_xo_tooBig = i }  
set_xo_fail   popi i = popi { popi_xo_fail   = i }  
set_rep       popi i = popi { popi_rep       = i }
set_elite     popi i = popi { popi_elite     = i }
set_nextRC    popi i = popi { popi_nextRC    = i }


popiInc :: (PopInfo -> Int) -> (PopInfo -> Int -> PopInfo) -> PopInfo -> PopInfo
popiInc getter setter popi = setter popi (getter popi + 1)


popInfoToJSON :: PopInfo -> JSValue
popInfoToJSON popi = jsObj [
    ("type"     , jsStr "populationInfo"                ),
    ("xos"      , jsArr . map xolToJson $ popi_xos popi ),
    ("best"     , popi_best popi                        ),
    ("pop"      , jsArr $ popi_pop       popi           ),
    ("xo_ok"    , jsNum $ popi_xo_ok     popi           ),
    ("xo_tooBig", jsNum $ popi_xo_tooBig popi           ),
    ("xo_fail"  , jsNum $ popi_xo_fail   popi           ),
    ("rep"      , jsNum $ popi_rep       popi           ),
    ("elite"    , jsNum $ popi_elite     popi           )]




