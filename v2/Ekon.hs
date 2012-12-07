module Ekon where

import Data.List


import qualified Data.Vector.Unboxed as U
import Statistics.LinearRegression

test :: Double -> IO ()
test k = do
  let n = 10000000
  let a = k*n + 1
  let b = (k+1)*n
  let xs = U.fromList [a..b]
  let ys = U.map (\x -> x*100 + 2000) xs
  -- thus 100 and 2000 are the alpha and beta we want
  putStrLn "linearRegression:"
  print $ linearRegression xs ys



type Qant     = Double
type Price    = Double
type Money    = Double

type Time     = Int

type Demand = Price -> Qant

data Thing = 
  Atom String | 
  Machine String [(Thing,Qant)] Money Thing

--data Firm_ = Firm_ { 
--  fInputs   :: [Thing] , 
--  fOutputs  :: [Thing] ,
--  fProperty :: [Thing] ,
--  fMoney    :: Money   }



data Firm = Firm {
 fMoney     :: Money  ,
 fProperty  :: [Qant] ,
 fMachs     :: [Mach] ,
 inMask     :: [Bool] ,
 outMask    :: [Bool] 
 }


data Mach = Mach {
 inMoney  :: Money  ,
 inQants  :: [Qant] , 
 outIndex :: Index ,
 outQant  :: Qant
 }


type Index   = Int

type History = ( [[Price]] , [[(Price,Qant)]] )


type Priority  = Double
type Power     = Double  
type MachOrder = (Power,Priority)

type FirmProgram = Firm -> History -> ( [Qant] , [MachOrder] , [Price]  )


-- my firm-program --------------------


myProg :: Firm -> History -> ( [Qant], [MachOrder] , [Price] )
myProg firm history = undefined

inputPricesEstimate :: [[Price]] -> [Price]
inputPricesEstimate pss = map mean pss

demandsEstimate :: [[(Price,Qant)]] -> [Demand]
demandsEstimate dds = map demandEstimate dds
 where
  demandEstimate dd = \ x -> alpha*x + beta
    where ( beta , alpha ) = linReg dd 




linReg :: [(Double,Double)] -> (Double,Double)
linReg datas = linearRegression (U.fromList xs) (U.fromList ys)
 where (xs,ys) = unzip datas



-- stepping simulation ------------

stepFirm :: Firm -> FirmProgram -> History -> [Price] -> [Demand] -> ( Firm , History )
stepFirm firm0 prog history0 inputPrices outputDemands =
 let history1           = updateInputPrices history0 inputPrices
     ( qantsToBuy , 
       machOrders , 
       sellPrices )     = prog firm0 history1
     firm1              = buyInputs firm0 qantsToBuy inputPrices
     firm2              = runMachs  firm1 machOrders
     (firm3,demandData) = setPrices firm2 sellPrices outputDemands 
     history2           = updateDemandData history1 demandData 
  in ( firm3 , history2 )


updateInputPrices :: History -> [Price] -> History
updateInputPrices ( pss , dds ) ps = ( zipWith (:) ps pss , dds )
                  
updateDemandData :: History -> [(Price,Qant)] -> History
updateDemandData ( pss , dds ) dd = ( pss , zipWith (:) dd dds )


buyInputs :: Firm -> [Qant] -> [Price] -> Firm
buyInputs firm qantsToBuy inputPrices = 
 let (money',deltaInputQants) = buyInputs' (fMoney firm) (zip qantsToBuy inputPrices)
     property' = plus (fProperty firm) (fillIt 0 deltaInputQants (inMask firm) )
  in firm { fMoney = money' , fProperty = property' }

buyInputs' :: Money -> [(Qant,Price)] -> ( Money , [Qant] )
buyInputs' money order = 
  let orderPrice = sum $ map (uncurry (*)) order
      qants = map fst order
   in if orderPrice <= money
       then ( money - orderPrice , qants )
       else let r = money / orderPrice
             in ( 0 , map (*r) qants )  



runMachs :: Firm -> [MachOrder] -> Firm
runMachs firm machOrders = foldl runMach firm (zip (fMachs firm) (map fst $ sortMachOrders machOrders ))

runMach :: Firm -> (Mach,Power)  -> Firm
runMach firm (mach,power)  =
 let (costMoney,realInputQants) = checkMachInput mach power firm
     property'                  = minus (fProperty firm) realInputQants
     property''                 = updateAt (outIndex mach) property' (+ (outQant mach))
     money'                     = (fMoney firm) - costMoney
  in firm { fMoney = money' , fProperty = property'' }

checkMachInput :: Mach -> Power -> Firm -> (Money , [Qant])
checkMachInput mach power firm = 
 let machIn     = map (*power) $ (inMoney mach) : (inQants   mach)
     firmHas    =                (fMoney  firm) : (fProperty firm)
     worstRatio = minimum $ zipWith (/) firmHas machIn
     machIn'    = if worstRatio < 1 then map (*worstRatio) machIn else machIn
  in ( head machIn' , tail machIn' )

sortMachOrders :: [MachOrder] -> [MachOrder]
sortMachOrders = sortBy (\(_,p1) (_,p2) -> compare p2 p1 )


setPrices :: Firm -> [Price] -> [Demand] -> ( Firm , [(Price,Qant)] )
setPrices firm sellPrices outputDemands = 
 let wantQants   = map (\(demand,sellPrice) -> demand sellPrice ) (zip outputDemands sellPrices)
     realQants   = fProperty firm
     sellQants   = map (\(want,real)-> if want > real then real else want ) (zip wantQants realQants)
     money'      = (fMoney firm) + (sum $ sellQants `krat` sellPrices)
     property'   = minus realQants sellQants 
  in ( firm { fMoney = money' , fProperty = property' }  ,  zip sellPrices wantQants )



plus :: Num a => [a] -> [a] -> [a]
plus = zipWith (+)

minus :: Num a => [a] -> [a] -> [a]
minus = zipWith (-)

krat :: Num a => [a] -> [a] -> [a]
krat = zipWith (*)


mean :: [Double] -> Double
mean xs = sum xs / (fromIntegral $ length xs)


subIt :: [a] -> [Bool] -> [a]
subIt [] [] = []
subIt (x:xs) (b:bs) = if b then x:rest else rest 
 where rest = subIt xs bs 
subIt _  _ = error "Mask do not match list."

fillIt :: a -> [a] -> [Bool] -> [a]
fillIt _    []     []     = []
fillIt defa []     bs     = if not (or bs) then replicate (length bs) defa else error "Mask do not match list."
fillIt defa xxs@(x:xs) (b:bs) = if b then x:(fillIt defa xs bs) else defa:(fillIt defa xxs bs)
fillIt _ _ _ = error "Mask do not match list."


updateAt :: Int -> [a] -> (a->a) -> [a]
updateAt i xs f = 
 let (as,b:bs) = splitAt i xs
  in as ++ ( f b : bs )

