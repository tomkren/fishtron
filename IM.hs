-- Inhabitation Machines --------------------------------------------

module IM where
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Maybe
import TTerm
import Util

generate :: Int -> Context -> Typ -> IO ()
generate limit ctx t = do 
  mapM putStrLn $ take limit $ proveStrs ctx t 
  return () 

-- types ------------------------------------------------------------

data IM = IM Typ Context IMMap

data PreIM    = PreIM Typ PreIMMap  
type Stack    = [ ( Typ , Context ) ]

type IMMap    = Map Typ [(Token, [Typ] )]
type PreIMMap = Map Typ ( [(Token, [Typ] )] , Context )

data Token = TokLam [(Symbol,Typ)] 
           | TokVar  Symbol Typ 

data Token2 = T2Lam [(Symbol,Int,Typ)] 
            | T2Var Symbol Int Typ 
            | T2ParL
            | T2ParR
            | T2ParR_lam [(Symbol,Int)]
            | T2Typ Typ

type Context2 = [(Symbol,Int)]

data Taxi = Taxi [Token2] [Token2] Context2  deriving (Show)

-- testing -------------------------------------------------------------

test = mapM putStrLn $ take 10 $ proveStrs [] (t1:->o:->o)

o = t0
a = Typ "a"
b = Typ "b"
c = Typ "c"
d = Typ "d"

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

contx1 = [("0",o),("succ",t1),("+",t1_2 ),("*",t1_2 )]

-- generating terms by IM --------------------------------------------

proveStrs :: Context -> Typ -> [String]
proveStrs ctx t 
  = (:) (show iM) $ map (\toks-> concatMap show toks) $ prove' $ singletonQueue taxi
 where
  iM@(IM _ _ im) = mkIM ctx t
  taxi = mkTaxi iM

  prove' :: Queue Taxi -> [[Token2]]
  prove' q = case popQueue q of
    Nothing -> []
    Just (taxi,q') -> case nextTaxis' im taxi of
     Left toks   -> toks : prove' q' 
     Right taxis -> prove' $ insertsQueue taxis q'


mkTaxi :: IM -> Taxi
mkTaxi (IM t ctx im) = Taxi [] [T2Typ t] $ map (\(x,_)->(x,1)) ctx

nextTaxis' :: IMMap -> Taxi -> Either [Token2] [Taxi]
nextTaxis' im taxi = case taxi of
  Taxi ret []     _    -> Left $ reverse ret
  Taxi ret (x:xs) ctx2 -> case x of
    T2Typ t         -> Right [ Taxi ret (toks++xs) ctx2' | (toks,ctx2') <- nextTaxis im ctx2 t ]
    T2ParR_lam vars -> Right [ Taxi (x:ret) xs (ctx2 \\ vars) ]
    _               -> Right [ Taxi (x:ret) xs ctx2 ]

nextTaxis :: IMMap -> Context2 -> Typ ->  [ ( [Token2] , Context2 ) ]
nextTaxis im ctx2 typ = case Map.lookup typ im of
  Nothing -> error "unexpected Nothing"
  Just edges -> concatMap next edges
 where
  next :: (Token,[Typ]) -> [ ( [Token2] , Context2 ) ]
  next (tok,ts) = case tok of
   TokLam ctx -> let (ctx2' , lamTok2 , rparTok2 ) = solveTokLam ctx ctx2  
                in [ ( T2ParL : lamTok2 : ( ( map T2Typ ts ) ++ [rparTok2]  ) , ctx2' ) ]
   TokVar x t -> let f t2Var = if null ts 
                              then ( [t2Var] , ctx2 )
                              else ( T2ParL : t2Var : ( ( map T2Typ ts ) ++ [T2ParR]  ) , ctx2 ) 
                in map f $ getT2Vars x t ctx2 

getT2Vars :: Symbol -> Typ -> Context2 -> [Token2]
getT2Vars x t ctx2 = map (\(_,n)-> T2Var x n t ) $ getThatVars ctx2 x

solveTokLam :: Context -> Context2 -> (Context2 , Token2 , Token2 )
solveTokLam ctx ctx2 = ( ctx'' ++ ctx2 , T2Lam ctx' , T2ParR_lam ctx'' )
 where
  ctx' = map f ctx
  ctx''= map (\(s,i,_)->(s,i)) ctx'
  f :: (Symbol,Typ) -> (Symbol,Int,Typ)
  f (x,t) = (x, 1 + (numOfThatVar ctx2 x) ,t)

numOfThatVar :: Context2 -> Symbol -> Int
numOfThatVar ctx2 x = length $ getThatVars ctx2 x 

getThatVars :: Context2 -> Symbol -> Context2
getThatVars ctx2 x = filter (\(x',_)->x'==x) ctx2 

-- creating inhabitation machines ---------------------------------------------

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
                   tLamContx  = mkTokLamContx contx ts
                in ( [ ( TokLam tLamContx , [Typ alpha] ) ] , tLamContx ++ contx ) 
  Typ alpha -> ( map f $ getWithEnd alpha contx , contx ) 
 where
  f :: (Symbol,Typ) -> ( Token , [Typ] )
  f (varName , typ) 
   = let ( ts , _ ) = typeArgs typ
      in ( TokVar varName typ , ts ) 

getWithEnd :: Symbol -> [(Symbol,Typ)] -> [(Symbol,Typ)]
getWithEnd alpha contx 
 = filter (\(_,typ)->let (_,beta)=typeArgs typ in alpha == beta ) contx 

mkTokLamContx :: Context -> [Typ] -> [(Symbol,Typ)]
mkTokLamContx contx ts = zip (mkNewVars contx (length ts)) ts

mkNewVars :: Context -> Int -> [Symbol]
mkNewVars contx num =  reverse $ mkNewVars' (map fst contx) [] num
 where
  varAlphabet = ['x'..'z'] ++ ['a'..'w']
  mkNewVars' :: [Symbol] -> [Symbol] -> Int -> [Symbol]
  mkNewVars' occ acc 0 = acc
  mkNewVars' occ acc n 
   = let var = newSymbol' varAlphabet occ  
      in mkNewVars' (var:occ) (var:acc) (n-1)

-- Shows ---------------------------------------------------------------------------

instance Show IM     where show = showIM
instance Show Token  where show = showToken
instance Show Token2 where show = showToken2

showIM :: IM -> String
showIM (IM typ _ im) = (++) (hLine ++ show typ ++ "\n" ++ hLine) $ concatMap f $ Map.toDescList im
 where
  hLine = (replicate 60 '-') ++ "\n" 
  f :: ( Typ , [(Token, [Typ] )] ) -> String
  f ( typ , edges  ) 
   = fillStr 20 (show typ) ++ " :  " ++ 
     (g $ intercalate (replicate 24 ' ') $ map (\(tok,ts)-> (show tok) ++ " => " ++ show ts ++"\n") edges ) 
  g str = if null str then "\n" else str

showToken :: Token -> String
showToken (TokLam cxt) = (++) "\\ " $ concatMap (\(x,t)->x ++ ":" ++ show t ++" ") cxt
showToken (TokVar x t) = x  

showToken2 :: Token2 -> String
showToken2 t = case t of
  T2Lam xs     -> "\\ " ++ (concatMap (\(x,n,t)-> showVar x n ++ ":" ++ show t ++" ") xs) ++ ". "
  T2Var x n _  -> showVar x n ++ " "
  T2ParL       -> "( "
  T2ParR       -> " )"
  T2ParR_lam _ -> " )"
  T2Typ t      -> show t
 where
  showVar x n = x ++ ( if n == 1 then "" else show n )
