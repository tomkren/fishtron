
--  Tento modul zajišťuje eavaluaci TTermů. V této chvíli je to
-- dočasná implementace, která bude nahrazena rychlejší a "čistší". 
-- Více o nedostatcích a budoucích plánech je v části o modulu Base.
-- 



module Evaluation 
( nf  
, beta
, subs
, varNames
, makeVarsUnique
, (@@)
, compute
) where 

import Data.List
import Data.Maybe
import Data.Dynamic

import Base
import Util

-- evaluation ------------------------------------------------------------------

-- nf : Pro zadanou redukční strategii, max. počet kroků a vstupní TTerm se pokusí tento
-- TTerm převést do normální formy. (Right pro TTerm v nf, Left jinak).
-- Redukční strategie je fce typu (TTerm -> Maybe TTerm) vracející Nothing pro TTerm
-- v normální formě.

nf :: (TTerm -> Maybe TTerm) -> Int -> TTerm -> Either TTerm TTerm
nf strat = nf'
    where
    nf' 0 t = Left t 
    nf' n t = case strat t of 
      Nothing -> Right t
      Just t' -> nf' (n-1) t'

-- beta : "hybridní" lýné vyhodnocení (s odchylkami kvůli tomu jak se řeší Comb)

beta ::  TTerm -> Maybe TTerm
beta ( App (Lam x m    _) n              _  ) = Just $ subs x m n
beta ( App (Val s1 (HaskComb dyn1) _) 
           (Val s2 (HaskComb dyn2) _)     typ) = Just $ Val ("["++s1++" "++s2++"]") (HaskComb $ dynApp dyn1 dyn2) typ 
beta ( App m@(Val _ (HaskComb _) _)  n   typ) = do n' <- beta n ; return $ App m n' typ 
beta ( App m@(Val _ (Comb comb) _)  n    typ) = Just $ App comb n typ 
beta ( App ( App ( App (Val _ IfComb _) (Val _ (HaskComb dynBool) _) _)  b _ ) c _ )  = case fromDynamic dynBool of
  Just True  -> Just b
  Just False -> Just c
  Nothing    -> error "ERR: in if in beta"
beta ( App ifC@(Val _ IfComb _) m typ) = do m' <- beta m ; return $ App ifC m' typ
beta ( App m              n              typ) = do m' <- beta m ; return $ App m' n typ
beta t                                        = Nothing



subs :: Symbol -> TTerm -> TTerm -> TTerm 
subs x m n = fst $ sub (varNames m) m x n   

sub :: [Symbol] -> TTerm -> Symbol -> TTerm -> (TTerm,[Symbol])
sub vns x@(Val _ _ _) _ _ = (x,vns)  
sub vns x@(Var v _) var vloz 
    | v == var  = (vloz,vns)
    | otherwise = (x,vns)  
sub vns (App p q typ) var vloz = (App t1 t2 typ , vns'')
    where 
    (t1,vns')  = sub vns  p var vloz
    (t2,vns'') = sub vns' q var vloz
sub vns x@(Lam v p typ) var vloz
    | v == var  = (x,vns)
    | not (elem v (fv vloz)) = (Lam v t typ , vns'') 
    | otherwise = sub vns' term' var vloz
    where
    (t,vns'') = (sub vns p var vloz)
    (term',vns') = rename vns x

varNames :: TTerm -> [Symbol]
varNames = nub . varNames'
    where
    varNames' (Var v   _)  = [v]
    varNames' (Val v _ _)  = [v]
    varNames' (App p q _)  = varNames' p ++ varNames' q
    varNames' (Lam v p _)  = v : varNames' p

fv :: TTerm -> [Symbol]
fv (Var v   _) = [v]
fv (Val v _ _) = [v] -- TODO neni to nejaka blbost davat je do FV, kouknout k cemu se to presne pouziva
fv (App p q _) = nub $ (fv p) ++ (fv q) -- neefektivni
fv (Lam v p _) = (fv p) \\ [v]

rename :: [Symbol] -> TTerm -> (TTerm,[Symbol])
rename vns (Lam v m typ@(varTyp :-> _) ) = ((Lam new m' typ),new:vns')
    where 
    new             = newSymbol vns
    (m',vns')       = sub vns m v (Var new varTyp)
rename vns t = (t,vns)

-- zunikátnění vázaných proměnných ---------------------------------------------

makeVarsUnique :: TTerm -> TTerm
makeVarsUnique tt = (\(a,_,_)->a) $ u (varNames tt) [] tt
  where  
  u :: [Symbol] -> [Symbol] -> TTerm -> ( TTerm , [Symbol] , [Symbol] )
  u allVars occupied t = case t of
    Lam x m typ@(a:->b) -> 
     if x `elem` occupied
     then let x' = newSymbol allVars
              (m',all',occ') = u (x':allVars) (x':occupied) (subs x m (Var x' a) )
           in ( Lam x' m' typ , all' , occ' )
     else let (m',all',occ') = u allVars (x:occupied) m
           in ( Lam x m' typ , all' , occ' )
    App m n typ -> 
     let (m',all' ,occ' ) = u allVars occupied m
         (n',all'',occ'') = u all'    occ'     n
      in ( App m' n' typ , all'' , occ'' )
    _           -> ( t , allVars , occupied )

-- tools for making fitness functions ------------------------------------------

 -- @@ je ekvivalent aplikace funkce pro TTerm a nějakou haskellovskou hodnotu
 -- compute se používá pro evaluaci termu a "přetypování" na haskellovskou hodnotu 

(@@) :: (Typeable a, Show a) => TTerm -> a -> TTerm
f @@ x = let (a:->b) = ttermTyp f in App f (toTTerm x a) b 

compute ::  (Typeable a) => Int -> TTerm -> Either TTerm a
compute steps t = case nf beta steps t of
  Left  t'                       -> Left t'
  Right (Val _ (HaskComb dyn) _) -> Right $ fromJust $ fromDynamic dyn
  Right t'                       -> error $ "\n\nERR: " ++ show t ++ " ---> " ++ show t' 

-- ladění ------------------------------------------------------------------------

nfErr :: (TTerm -> Either TTerm (Maybe TTerm)) -> Int -> TTerm -> Either TTerm TTerm
nfErr strat i tt = nf' i tt
    where
    nf' 0 t = Left t 
    nf' n t = case strat t of 
      Left errT       -> error $ "beta r. err: " ++ show tt ++ " ---> " ++ show errT   
      Right Nothing   -> Right t
      Right (Just t') -> nf' (n-1) t'

betaErr ::  TTerm -> Either TTerm (Maybe TTerm)
betaErr t = if checkTyp t then Right $ beta t else Left t 



