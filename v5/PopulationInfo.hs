module PopulationInfo 
( PopInfo
, IndivRecord(..)
, initPopInfo
, updatePopInfo
, popInfoToJSON
, inc_xo_ok, inc_xo_tooBig, inc_xo_fail
, popi_setBest
) where

import Text.JSON (JSValue)
import JSONUtils (jsArr,jsObj,jsStr,jsNum)
import TTree     (CTT(..), TTree(..))
import TTerm     (Context)
import Utils     (JShow, jshow_popi)

data PopInfo = PopInfo {
    popi_irs       :: [IndivRecord],
    popi_xo_ok     :: Int,
    popi_xo_tooBig :: Int,
    popi_xo_fail   :: Int,
    popi_best      :: JSValue
  }

initPopInfo :: PopInfo
initPopInfo = PopInfo {
  popi_irs       = [],
  popi_xo_ok     = 0,
  popi_xo_tooBig = 0,
  popi_xo_fail   = 0,
  popi_best      = jsObj []
}

-- obsahuje info o jednom jedinci
data IndivRecord = XoverIR

type XoverFailed = Bool



popi_setBest :: (JShow term) => term -> Double -> PopInfo -> PopInfo
popi_setBest term fitVal popi = popi{ popi_best = jsObj [
 ("term",   jshow_popi term),
 ("fitval", jsNum fitVal)] } 

updatePopInfo :: PopInfo -> IndivRecord -> PopInfo
updatePopInfo popi ir = 
  let irs = popi_irs popi
   in popi { popi_irs = ir:irs }

-- :: PopInfo -> PopInfo
inc_xo_ok     = popiInc popi_xo_ok     set_xo_ok
inc_xo_tooBig = popiInc popi_xo_tooBig set_xo_tooBig
inc_xo_fail   = popiInc popi_xo_fail   set_xo_fail



-- :: PopInfo -> Int -> PopInfo
set_xo_ok     popi i = popi { popi_xo_ok     = i }  
set_xo_tooBig popi i = popi { popi_xo_tooBig = i }  
set_xo_fail   popi i = popi { popi_xo_fail   = i }  



popiInc :: (PopInfo -> Int) -> (PopInfo -> Int -> PopInfo) -> PopInfo -> PopInfo
popiInc getter setter popi = setter popi (getter popi + 1)


indivRecordToJSON :: IndivRecord -> JSValue
indivRecordToJSON ir = jsStr "FAKE"

popInfoToJSON :: PopInfo -> JSValue
popInfoToJSON popi = jsObj [
    ("type"     , jsStr "populationInfo"),
    ("best"     , popi_best popi),
    ("irs"      , jsArr . map indivRecordToJSON . popi_irs $ popi ),
    ("xo_ok"    , jsNum $ popi_xo_ok     popi ),
    ("xo_tooBig", jsNum $ popi_xo_tooBig popi ),
    ("xo_fail"  , jsNum $ popi_xo_fail   popi )]




