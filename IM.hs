-- Inhabitation Machines

module IM where
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Base
import Util

--data PreIM =  Map NodeId Node
--data Node = OrNode  Typ   Context [NodeId]
--          | AndNode Token Context [NodeId]
--data NodeId = TypId Typ | TokenId Int 


mkTaxi :: IM -> Taxi
mkTaxi (IM t ctx im) = Taxi [] [TTyp t] ctx

stepTaxi :: IMMap -> Taxi -> Either [Token] [Taxi]
stepTaxi im (Taxi ret []     _   ) = Left $ reverse ret
stepTaxi im (Taxi ret (x:xs) ctx ) = case x of
 TTyp t -> Right [ Taxi ret (toks++xs) ctx | toks <- succss im ctx t ]
 _      -> Right [ Taxi (x:ret) xs ctx ]

{-
proveLol :: Context -> Typ -> [String]
proveLol ctx t = 
 where
  iM@(IM _ _im) = mkIM ctx t
  taxi = mkTaxi iM
-}

succss :: IMMap -> Context -> Typ -> [[Token]]
succss im ctx typ = case Map.lookup typ im of
 Nothing -> error "unexpected nothing"
 Just edges -> map (\(tok,ts)->tok:(map TTyp ts)) edges

data Taxi = Taxi [Token] [Token]  Context  


type PreIMMap = Map Typ ( [(Token, [Typ] )] , Context )
type IMMap    = Map Typ [(Token, [Typ] )]

data PreIM    = PreIM Typ PreIMMap  
type Stack    = [ ( Typ , Context ) ]

data IM = IM Typ Context IMMap

instance Show IM where show = showIM

data Token = TLam [(Symbol,Typ)] 
           | TVar  Symbol Typ 
           | TParL
           | TParR
           | TTyp Typ
           | TVar2 Symbol Int Typ

instance Show Token where show = showToken


showToken :: Token -> String
showToken (TLam cxt) = (++) "\\ " $ concatMap (\(x,t)->x ++ ":" ++ show t ++" ") cxt
showToken (TVar x t) = x  


o = t0
a = Typ "a"
b = Typ "b"
c = Typ "c"


t0 = Typ "0"
t1 = t 1
t2 = t 2
t3 = t 3
t1_2 = tt 1 2

t :: Int -> Typ
t 0 = t0
t n = (t $ n-1) :-> t0

tt :: Int -> Int -> Typ  
tt n k = foldr (\_ acc -> t (n-1) :-> acc ) t0 [1..k]

contx1 = [("0",o),("a",o),("inc",t1),("+",t1_2 )]

showIM :: IM -> String
showIM (IM typ _ im) = (++) (hLine ++ show typ ++ "\n" ++ hLine) $ concatMap f $ Map.toDescList im
 where
  hLine = (replicate 60 '-') ++ "\n" 
  f :: ( Typ , [(Token, [Typ] )] ) -> String
  f ( typ , edges  ) 
   = fillStr 20 (show typ) ++ " :  " ++ 
     (g $ intercalate (replicate 24 ' ') $ map (\(tok,ts)-> (show tok) ++ " => " ++ show ts ++"\n") edges ) 
  g str = if null str then "\n" else str

mkIM :: Context -> Typ -> IM
mkIM ctx t 
 = let (PreIM _ mi) = mkPre ctx t  
    in IM t ctx $ Map.fromList $ map (\(t,(xs,_))->(t,xs)) $ Map.toList mi

mkPre :: Context -> Typ -> PreIM
mkPre contx typ = PreIM typ $ mkPre' [(typ,contx)] Map.empty
 where 
  
  mkPre' :: Stack -> PreIMMap -> PreIMMap
  mkPre' [] im = im
  mkPre' ((typ,contx):stack) im 
   = case Map.lookup typ im of
    Nothing -> 
     let (xs,stack') = f typ contx
      in mkPre' (stack'++stack) $ Map.insert typ xs im  
    Just ( ss , contx' ) -> case contx \\ contx' of
     [] -> mkPre' stack im
     cs -> 
      let ((xs,contx''),stack') = f typ cs
       in mkPre' (stack'++stack) $ Map.insert typ (xs++ss, union contx'' contx' ) im
  
  f :: Typ -> Context -> ( ( [(Token, [Typ] )] , Context )  ,  Stack )
  f typ contx
   = let xs@(ss , contx') = succesors typ contx 
         stack' = map (\t->(t,contx')) $ nub $ concatMap snd ss
      in ( xs , stack' )  
  
succesors :: Typ -> Context -> ( [(Token, [Typ] )] , Context ) 
succesors typ contx = case typ of
  (_ :-> _) -> let (ts,alpha) = typeArgs typ
                   tLamContx  = mkTLamContx contx ts
                in ( [ ( TLam tLamContx , [Typ alpha] ) ] , tLamContx ++ contx ) 
  Typ alpha -> ( map f $ getWithEnd alpha contx , contx ) 
 where
  f :: (Symbol,Typ) -> ( Token , [Typ] )
  f (varName , typ) 
   = let ( ts , _ ) = typeArgs typ
      in ( TVar varName typ , ts ) 

getWithEnd :: Symbol -> [(Symbol,Typ)] -> [(Symbol,Typ)]
getWithEnd alpha contx 
 = filter (\(_,typ)->let (_,beta)=typeArgs typ in alpha == beta ) contx 

mkTLamContx :: Context -> [Typ] -> [(Symbol,Typ)]
mkTLamContx contx ts = zip (mkNewVars contx (length ts)) ts

mkNewVars :: Context -> Int -> [Symbol]
mkNewVars contx num =  reverse $ mkNewVars' (map fst contx) [] num
 where
  mkNewVars' :: [Symbol] -> [Symbol] -> Int -> [Symbol]
  mkNewVars' occ acc 0 = acc
  mkNewVars' occ acc n 
   = let var = newSymbol occ  
      in mkNewVars' (var:occ) (var:acc) (n-1)

