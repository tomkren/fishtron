module Prover 
( dk2
, dk 
, dk'
, envii , en1
) where 

import Base
import Util
import Dist
import Evaluation
import Enviroments

import DecTree

import Text.Printf
import Data.List
import System.Random
import qualified Data.Map as Map
import qualified Data.PSQueue as Q -- Prioritní fronta


 -- NumSteps je počet kroků v důkazu nějakého termu.
 --        H je heuristický dolní odhad počtu zbývajících kroků k dokončení důkazu.  
 --   DeltaH je změna heuristického odhadu.

type NumSteps  = Int
type H         = Int
type DeltaH    = Int

 --      Law je z pohledu důkazů odvozovací pravidlo, z pohledu gramatiky termů přepisovací
 --          pravidlo. Pro neterminál vrací všechny termy, na které se vzhledem k tomuto
 --          pravidlu může tento neterminál přepsat. Z pohledu důkazů představuje jeden krok
 --          důkazu.
 --     LawH je Law obohacené o informaci o změně dolního odhadu pro potřebný počet kroků
 --          důkazu způsobený právě použitým krokem.
 -- TTermLaw je ekvivalent LawH nad celými termy. Vrací Nothing pro dokončený důkaz.

type Law       = Nete -> [TTerm]
type LawH      = Nete -> [(TTerm,DeltaH)]
type TTermLaw  = TTerm -> Maybe [(TTerm,DeltaH)]

 -- TTermCar je "obálka" pro TTerm využívaná během důkazu. Obsahuje pomocné informace
 --          o již uplynulém počtu kroků důkazu a dolním odhadu kroků potřebných
 --          pro dokončení důkazu.
 --   CarLaw je ekvivalent TTermLaw pro TTermCar.
 
type TTermCar = (TTerm , NumSteps , H )
type CarLaw   = TTermCar -> Maybe [TTermCar] --TODO asi dat pryc to Maybe

 
-- prover ----------------------------------------------------------------------

dk' :: (RandomGen g) => Env -> g -> Typ -> ([TTerm],g)
dk' env gen goal = ( dk' $ Q.singleton car numNete , gen )
  where
  law  = mkCarLaw . mkTTermLaw $ axiomH +++ mpH +++ vodH  +++ (envAxiomH env)
  car@(_,_,numNete) = mkTTermCar $ Temp (goal,mkContextCar env)

  dk' :: Q.PSQ TTermCar Int -> [TTerm]
  dk' q = case Q.minView q of
      Nothing -> [] 
      Just ( car@(t,_,_) Q.:-> _ , q' ) -> case law car of
        Nothing   -> let t' = makeVarsUnique t 
                      in if checkTyp t' then t' : dk' q' else error $ "checkType ERR : " ++ show t' 
        Just cars -> dk' $ insertToQ q' cars

dk :: Env -> Typ -> [TTerm]
dk env goal = dk' $ Q.singleton car numNete
  where
  law  = mkCarLaw . mkTTermLaw $ axiomH +++ mpH +++ vodH  +++ (envAxiomH env)
  car@(_,_,numNete) = mkTTermCar $ Temp (goal,mkContextCar env)

  dk' :: Q.PSQ TTermCar Int -> [TTerm]
  dk' q = case Q.minView q of
      Nothing -> [] 
      Just ( car@(t,_,_) Q.:-> _ , q' ) -> case law car of
        Nothing   -> let t' = makeVarsUnique t 
                      in if checkTyp t' then t' : dk' q' else error $ "checkType ERR : " ++ show t' 
        Just cars -> dk' $ insertToQ q' cars


insertToQ :: Q.PSQ TTermCar Int -> [TTermCar] -> Q.PSQ TTermCar Int
insertToQ q [] = q 
insertToQ q ((car@(_,numSteps,numNete)):cars) = insertToQ (Q.insert car (numSteps+numNete) q) cars 

-- makers ----------------------------------------------------------------------

mkLawH :: Law -> DeltaH -> LawH
mkLawH law i nete = map (\t->( t , i )) $ law nete

mkTTermLaw :: LawH -> TTerm -> Maybe [(TTerm,DeltaH)]
mkTTermLaw law term = do
  (Temp nete,ds) <- findNete term
  return [ (tzGoTop (t , ds), deltaH ) | (t,deltaH) <- law nete ]

mkTTermCar :: TTerm -> TTermCar
mkTTermCar t = (t , 0 , numNetes t)

mkCarLaw :: TTermLaw -> TTermCar -> Maybe [TTermCar]
mkCarLaw law (term,steps,h) = do
  xs <- law term
  return [ (term',steps+1,h+delta) |  (term',delta) <- xs ]

-- inference rules -------------------------------------------------------------

axiomH = mkLawH axiom (-1)
mpH    = mkLawH mp      1
vodH   = mkLawH vod     0

axiom :: Nete -> [TTerm]
axiom (typ,(b,_,dict) ) = [ term | (x,typ') <- b , typ == typ' ,
                            let term = symbolToTTerm dict x typ , 
                            (\t->case t of Var _ _ -> True ; _ -> False) term ]

mp :: Nete -> [TTerm]
mp (typ,base@(_,types,_) ) 
 = [ App (Temp ( typ' :-> typ , base )) (Temp ( typ' , base )) typ | typ' <- types ]
 
vod :: Nete -> [TTerm]
vod (Typ _         , _              ) = []
vod (typ@(a :-> b) , (ba,types,dict) ) = [ Lam x (Temp (b, base' ) ) typ ]
  where 
  x = newSymbol $ map fst ba
  base' = ( (x,a) : ba , nub $ a : types , dict ) -- TODO ? nemelo by bejt este něco v druhý části base' ?

envAxiomH :: Env -> Nete -> [(TTerm,DeltaH)]
envAxiomH env = foldr (+++) (\_->[]) (map bbLawH env) 

bbLawH :: (Symbol,Typ,Comb) -> Nete -> [(TTerm,DeltaH)]
bbLawH (fName , fTyp, _) (neteTyp,base@(_,_,dict) ) = case matchSufix neteTyp fTyp of
  Nothing -> []
  Just ts -> [ (foldl (\acc typ -> App acc (Temp (typ,base) ) (typFromAcc acc) ) 
               (symbolToTTerm dict fName fTyp) ts  , length ts - 1 )  ]
  where
  typFromAcc acc = let (t1:->t2) = ttermTyp acc in t2


-- in progress -----------------------------------------------------------------

envi  =  [
          ( "abs", real :-> real                    , absFunR          )
          ,("+"  , real :-> real :-> real           , plusFunR         )
          ,("-"  , real :-> real :-> real           , minusFunR        )
          ,("*"  , real :-> real :-> real           , kratFunR         )            
          ,("1"  , real                             , toDyn' (1::Double))
        ]

envii = ENod paraDec 
         [ (100 , ELaw axiomH ) 
         , (100 , ELaw mpH    )
         , (100 , ELaw vodH   )
         , (100 , EFun "abs" ( real :-> real          ) absFunR   )
         , (100 , EFun "+"   ( real :-> real :-> real ) plusFunR  )
         , (100 , EFun "-"   ( real :-> real :-> real ) minusFunR )
         , (100 , EFun "*"   ( real :-> real :-> real ) kratFunR  )            
         , (100 , EReal (0,1) )
         ]

en1 =  ENod paraDec 
          [
           (100 , ELaw axiomH ) 
          ,(100 , ELaw mpH    )
          ,(100 , ELaw vodH   )
       --   ,(100 , EFun "abs"  (int :-> int)                     absFun         )
       --   ,(100 , EFun "+"    (int :-> int :-> int)             plusFun        )
       --   ,(100 , EFun "-"    (int :-> int :-> int)             minusFun       )
          ,(100 , EFun "*"    (int :-> int :-> int)             kratFun        )            
       --   ,(100 , EFun "%"    (int :-> int :-> int)             divvFun        )
       --   ,(100 , EFun "<"    (int :-> int :-> bool)            ltFun          )
       --   ,(100 , EFun "if"   (bool :-> int :->  int :-> int)   ifFun          )
       --  , (100 , EInt (-5,5) )
        ]

enviii = ENod paraDec 
         [ (100 , ELaw axiomH ) 
         , (100 , ELaw mpH    )
         , (100 , ELaw vodH   )
         , (100 , EFun "+"   ( int :-> int :-> int ) plusFun  ) 
         , (100 , EInt (-5,5) )
         ]

type TTDT = DecTree TTermCar Double TTermCar

mkEmbryo :: CarLaw -> TTDT        
mkEmbryo law = Embryo $ \ car -> case law car of
  Nothing   -> Answs []
  Just cars -> Answs cars


realCarLaw :: Double -> TTermCar -> Maybe [TTermCar]
realCarLaw x = mkCarLaw . mkTTermLaw $ mkLawH (realLaw x) (-1)

intCarLaw :: Int -> TTermCar -> Maybe [TTermCar]
intCarLaw x = mkCarLaw . mkTTermLaw $ mkLawH (intLaw x) (-1)

realLaw :: Double -> (Typ,ContextCar) -> [TTerm]
realLaw x (typ,_) | typ == real = [Val (showNice x) (toDyn' x) typ]
                  | otherwise   = []
  where showNice = printf "%.2f"

intLaw :: Int -> (Typ, ContextCar) -> [TTerm]
intLaw x (typ,_) | typ == int = [Val (show x) (toDyn' x) typ]
                 | otherwise  = []


mkRandReal :: (Double,Double) -> TTDT
mkRandReal (a,b) = Embryo $ \ car -> case realCarLaw a car of
  Just [_] -> Answ2 (\x -> let num = (b-a)*x+a 
                            in case realCarLaw num car of
                                 Just [car'] -> car' )
  _ -> Answs []  

mkRandInt :: (Int,Int) -> TTDT
mkRandInt (a,b) = Embryo $ \ car -> case intCarLaw a car of
  Just [_] -> Answ2 (\x -> let a' = fromIntegral a
                               b' = fromIntegral b
                               num = round $ (b'-a')*x+a' 
                            in case intCarLaw num car of
                                 Just [car'] -> car' )
  _ -> Answs []  


data Env2 = EFun Symbol Typ Comb 
          | ELaw LawH
          | ENod (TTermCar->[Double]->[Double]) [(Double,Env2)]
          | EReal (Double,Double)
          | EInt  (Int,Int)
--          | ESame [Env2] 
mkTTDT :: Env2 -> TTDT
mkTTDT (EFun sym typ comb) = mkEmbryo . mkCarLaw . mkTTermLaw . bbLawH $ (sym,typ,comb)
mkTTDT (ELaw lawH        ) = mkEmbryo . mkCarLaw . mkTTermLaw $ lawH
mkTTDT (ENod f xs        ) = DecTree f $ map (\(d,env2)-> (d,mkTTDT env2) ) xs   
mkTTDT (EReal interval   ) = mkRandReal interval  
mkTTDT (EInt  interval   ) = mkRandInt  interval  

--mkTTDT (ESame f xs        ) =


envFromEnv2 :: Env2 -> [(Symbol,Typ,Comb)]
envFromEnv2 (EFun sym typ comb) = [(sym,typ,comb)]
envFromEnv2 (ELaw _   )         = []
envFromEnv2 (ENod _ xs)         = concatMap (\(_,env2)->envFromEnv2 env2) xs
envFromEnv2 (EReal _  )         = []
envFromEnv2 (EInt  _  )         = []



carka = mkTTermCar $ Temp ( (real) , mkContextCar $ envFromEnv2 envii )
testik = decide (mkTTDT envii) carka

test1   = putList $take 100 $fst $ dk2 envii  (1,1) (mkStdGen 42) real
test2   = putList $take 100 $fst $ dk2 enviii (1,1) (mkStdGen 42) int
testErr = putList $take 100 $fst $ dk2 enviii (1,1) (mkStdGen 42) bool

  
dk2 :: (RandomGen g) => Env2 -> (Double,Double) -> g -> Typ -> ([TTerm],g) -- TODO dat pryc Env
dk2 env2 (percent,cutP) gen goal = dk' gen 0 $ Q.singleton car numNete
  where
  counterLimit = 10000
  env     = envFromEnv2 env2
  ttdt    = mkTTDT env2
  car@(_,_,numNete) = mkTTermCar $ Temp (goal,mkContextCar env)

  dk' :: (RandomGen g) => g -> Int -> Q.PSQ TTermCar Int -> ([TTerm],g)
  dk' gen counter q = if counter > counterLimit
    then error "dk2 error : counter overflow"
    else case Q.minView q of
      Nothing -> ([],gen) 
      Just ( car@(t,_,_) Q.:-> _ , q' ) -> 
        if hasNete t 
        then let dist = decide ttdt car
                 ( cars , _ , gen' )  = distObtain gen (percent,cutP) dist           
              in dk' gen' (counter+1) $ insertToQ q' cars
        else let t' = makeVarsUnique t 
              in if checkTyp t' 
                 then let (ts,gen') = dk' gen 0 q' in (t' : ts ,gen') 
                 else error $ "checkType ERR : " ++ show t'
                 

