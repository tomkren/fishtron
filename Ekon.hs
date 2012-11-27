module Ekon where

type Qant  = Double
type Price = Double
type Money = Double
type Time  = Int

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

type FirmProgram = Firm -> History -> ( [Qant] , [Qant] , [Price]  )


stepFirm :: Firm -> FirmProgram -> History -> [Price] -> [Demand] -> Firm
stepFirm firm prog history inputPrices outputDemands =
 let ( qantsToBuy , qantsToMake , sellPrices ) = prog firm history
     (money',deltaInputQants) = buyInputs (fMoney firm) (zip qantsToMake inputPrices)
     property' = plus (fProperty firm) (fillIt 0 deltaInputQants (inMask firm))
     -- @TODO production
     sellQants = map (\(demand,sellPrice) -> demand sellPrice ) (zip outputDemands sellPrices)

     -- ...
  in undefined 


buyInputs :: Money -> [(Qant,Price)] -> ( Money , [Qant] )
buyInputs money order = 
  let orderPrice = sum $ map (uncurry (*)) order
      qants = map fst order
   in if orderPrice <= money
       then ( money - orderPrice , qants )
       else let r = money / orderPrice
             in ( 0 , map (*r) qants )  





plus :: Num a => [a] -> [a] -> [a]
plus xs ys = map (\(x,y)->x+y) (zip xs ys)

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

