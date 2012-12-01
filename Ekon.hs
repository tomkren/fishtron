module Ekon where

import Data.List

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


stepFirm :: Firm -> FirmProgram -> History -> [Price] -> [Demand] -> ( Firm , History )
stepFirm firm prog history inputPrices outputDemands =
 let history' = updateHistoryWithInputPrices history inputPrices
     ( qantsToBuy , machOrders , sellPrices ) = prog firm history'
     firm' = buyInputs firm qantsToBuy inputPrices
     -- run machines : 
     -- ..
     -- set prices for outputs
     sellQants = map (\(demand,sellPrice) -> demand sellPrice ) (zip outputDemands sellPrices)
     -- ...
  in undefined 


updateHistoryWithInputPrices :: History -> [Price] -> History
updateHistoryWithInputPrices ( pss , dss ) ps = ( ps:pss , dss )


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



--runMachs :: Firm -> [MachOrder] -> [Firm]
--runMachs firm machOrders = 

runMach :: Mach -> Power -> Firm -> Firm
runMach mach power firm =
 let (costMoney,realInputQants) = checkMachInput mach power firm
     property'                  = minus (fProperty firm) realInputQants
     property''                 = updateAt (outIndex mach) property' (+ (outQant mach))
     money'                     = (fMoney firm) - costMoney
  in firm { fMoney = money' , fProperty = property'' }

checkMachInput :: Mach -> Power -> Firm -> (Money , [Qant])
checkMachInput mach power firm = 
 let machIn     = map (*power) $ (inMoney mach) : (inQants   mach)
     firmHas    =                (fMoney  firm) : (fProperty firm)
     worstRatio = minimum $ perCompo (/) firmHas machIn
     machIn'    = if worstRatio < 1 then map (*worstRatio) machIn else machIn
  in ( head machIn' , tail machIn' )




plus :: Num a => [a] -> [a] -> [a]
plus = perCompo (+)

minus :: Num a => [a] -> [a] -> [a]
minus = perCompo (-)

perCompo :: (a->a->a) -> [a] -> [a] -> [a]
perCompo op xs ys = map (\(x,y)->x `op` y) (zip xs ys)


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

