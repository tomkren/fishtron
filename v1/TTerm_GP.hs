{-# LANGUAGE MultiParamTypeClasses #-}

module TTerm_GP where

import TTerm
import IM_new (prove , randProveUnique , randProveOne ,   o,t1_2 )
import GP_Classes (Gene,Cros,Muta,FitFun2(..),FitVal, generateIt,mutateIt,crossIt, mkEOpt,Problem(..)
                 ,getFFType,putEvolveMaximas,putEvolve)

import Heval  (as)
import Util   (Rand, getRandomL, getRandomR)

import Data.List     (nub,groupBy)
import Data.Typeable (Typeable)

-- testing ------------------

treeType = (t1_2:->o:->o)
tree i = head . drop i $ prove treeType []

-- data FitFun2 term a = FF1 (term -> Rand FitVal)
--                     | FF2 (term->String) a (a->Rand FitVal)

dou, dou1, dou2 :: Typ
dou  = Typ "Double"
dou1 = dou :-> dou
dou2 = dou :-> dou :-> dou

tt_ssr = ttSolve ff_tt_ssr ssr_ctx dou1

ssr_ctx :: Context
ssr_ctx = ([("plus",dou2),("minus",dou2),("krat",dou2),("rdiv",dou2),("sin",dou1),("cos",dou1),("exp",dou1),("rlog",dou1)])

ff_tt_ssr :: FitFun2 TTerm (Double->Double)
ff_tt_ssr = FF2 show (as::Double->Double) (return . ff)
 where
  ff :: (Double->Double) -> FitVal
  ff f = (1/) . (1+) . sum . map (\x-> let dx = (f x) - ( x*x*x*x+x*x*x+x*x+x ) in dx*dx ) $ [-1,-0.9..1] 


-- GP instances ---------------------------

ttSolve :: (Typeable a) => FitFun2 TTerm a -> Context -> Typ -> IO ()
ttSolve ff ctx typ = 
 let eOpt    = mkEOpt (0,0,100)
     popSize = 10
     lim     = 100
     gOpt    = TTG_IM_rand typ ctx lim
     mOpt    = TTM_my ctx lim  
     cOpt    = TTC_my ctx 
  in putEvolve 25000 $ Problem popSize eOpt gOpt mOpt cOpt ff (getFFType ff)

instance Gene TTerm TTermGen where generateIt = ttermGen
instance Muta TTerm TTermMut where mutateIt   = ttermMut
instance Cros TTerm TTermCro where crossIt    = ttermCro


data TTermGen = TTG_IM_rand Typ Context Int
              | TTG_IM_syst Typ Context

data TTermMut = TTM_my Context Int

data TTermCro = TTC_my Context


ttermGen :: TTermGen -> Rand [TTerm] 
ttermGen opt = case opt of
 TTG_IM_rand typ ctx limit -> randProveUnique limit typ ctx
 TTG_IM_syst typ ctx       -> return $ prove typ ctx

ttermCro :: TTermCro -> TTerm -> TTerm -> Rand (TTerm,TTerm)
ttermCro (TTC_my ctx) tt1 tt2 = xover ctx tt1 tt2

-- mutation --------------------------------------

ttermMut :: TTermMut -> TTerm -> Rand TTerm
ttermMut (TTM_my ctx limit) tt = do
 (TTZ sub ds) <- getRandomL $ subterms tt
 newSub <- randProveOne limit (ttermTyp sub) ctx 
 return . makeVarsUnique . tzGoTop $ TTZ newSub ds


-- xover ------------------------------------------

xover :: Context -> TTerm -> TTerm -> Rand ( TTerm , TTerm )
xover ctx t1 t2 = 
  let candidates = compatibleSubterms t1 t2  --  <---------------- tady se voli smart/normal
      canSize    = length candidates
   in if null candidates 
      then do return $ (t1,t2) 
      else do
        i <- getRandomR (0, canSize - 1 )
        let (z1,z2) = candidates !! i 
        xover' ctx z1 z2


xover' :: Context -> TTermZipper -> TTermZipper -> Rand ( TTerm , TTerm )
xover' ctx tz1@(TTZ t1 ds1) tz2@(TTZ t2 ds2) = do 
 t1' <- treatFVs ctx (TTZ t1 ds2)
 t2' <- treatFVs ctx (TTZ t2 ds1)
 let  chis = ( makeVarsUnique $ tzGoTop (TTZ t2' ds1) , makeVarsUnique $ tzGoTop (TTZ t1' ds2) ) 
      pars = ( tzGoTop tz1 , tzGoTop tz2 )
  in  return $ checkXover pars chis

checkXover :: (TTerm,TTerm) -> (TTerm , TTerm) -> (TTerm , TTerm)
checkXover parents chs@(ch1,ch2) = 
 if checkTyp ch1 then
  if checkTyp ch2 then
   chs
  else err 
 else err
 where 
 err = error $ "xover typeCheck ERR: " ++
  show parents ++ " -> " ++ show chs

treatFVs :: Context -> TTermZipper -> Rand TTerm
treatFVs ctx (TTZ t ds) = f (fv' t) t
  where
  boundVarsInDs :: Context  
  boundVarsInDs = foldl (\acc x -> case x of 
    Lamb sym (typ:->_) -> if has acc sym
                          then           acc 
                          else (sym,typ):acc 
    _                  ->                acc ) [] ds
  has :: Context -> Symbol -> Bool
  has xs s = any (\(s',_)->s==s') xs
  f :: [(Symbol,Typ)] -> TTerm  -> Rand TTerm
  f [] t = return t
  f ((fv,fvTyp):fvs) t = 
    if null withSameTyp
    then f fvs $ subs fv t (defaultValue fvTyp ctx) 
    else do
      i <- getRandomR ( 0 , length withSameTyp - 1 )
      f fvs $ subs fv t $ TVar (withSameTyp !! i) fvTyp
    where
    withSameTyp = map fst $ filter (\(_,t) -> t == fvTyp ) boundVarsInDs 
  fv' :: TTerm -> [(Symbol,Typ)]
  fv' (TVar v   typ) = [(v,typ)]
  fv' (TVal v   _  ) = []
  fv' (TApp p q _  ) = nub $ (fv' p) ++ (fv' q) 
  fv' (TLam v p typ) = filter (\(s,_)->s/=v) (fv' p)   -- neefektivni

defaultValue :: Typ -> Context -> TTerm
defaultValue typ ctx = head $ prove typ ctx

compatibleSubterms :: TTerm -> TTerm ->  [ (TTermZipper,TTermZipper) ]
compatibleSubterms t1 t2 = concatMap (\(as,bs) -> [ (a,b) | a <- as , b <- bs ] ) 
  $ f (groupenSubterms t1) (groupenSubterms t2) 
  where
  f :: [(Typ,[TTermZipper])] -> [(Typ,[TTermZipper])] -> [([TTermZipper],[TTermZipper])]
  f []            _  = []
  f ((typ,ts):xs) ys = case lookup typ ys of
    Nothing  -> f xs ys
    Just ts' -> ( ts , ts' ) : f xs ys

groupenSubterms :: TTerm -> [(Typ,[TTermZipper])]
groupenSubterms t = map (\ts -> ( ttzTyp $ head ts ,ts) ) 
  $ groupBy (\t1 t2 -> ttzTyp t1 == ttzTyp t2 ) $ subterms t 

subterms :: TTerm -> [TTermZipper]
subterms term = subzips (TTZ term []) 
  where
  subzips :: TTermZipper -> [TTermZipper]
  subzips z@(TTZ t _) = case t of
    TVar _   _ -> [z]
    TVal _   _ -> [z]
    TLam _ _ _ -> z : ( subzips $ tzDown z )
    TApp _ _ _ -> z : ( subzips (tzLeft z) ++ subzips (tzRight z) )
