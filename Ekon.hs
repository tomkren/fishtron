module Ekon where

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
 outQants :: [Qant]
 }


type History = ( [[Price]] , [[(Price,Qant)]] )

type Priority  = Double
type Power     = Double  
type MachOrder = (Power,Priority)

type FirmProgram = Firm -> History -> ( [Qant] , [MachOrder] , [Price]  )


stepFirm :: Firm -> FirmProgram -> History -> [Price] -> [Demand] -> ( Firm , History )
stepFirm firm prog history inputPrices outputDemands =
 let history' = updateHistoryWithInputPrices history inputPrices
     ( qantsToBuy , qantsToMake , sellPrices ) = prog firm history'
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



runMach :: Mach -> Power -> Firm -> Firm
runMach mach power firm = undefined

checkMachInput :: Mach -> [Qnat] -> [Qant]





plus :: Num a => [a] -> [a] -> [a]
plus xs ys = map (\(x,y)->x+y) (zip xs ys)

--perCompo :: (a->a->a) -> [a] -> [a] -> [a]
--perCompo op xs ys = map (\(x,y)->x `op` y) (zip xs ys)

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

