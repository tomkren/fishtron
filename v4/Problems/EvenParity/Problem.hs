module Problems.EvenParity.Problem where

import TTerm (Typ(..),Context)
import ProblemUtils 
import JSONUtils
import GP_Core (FitFun(FF6))
import GP_Data (CTTGen(..),CTTermGen(..))
import DrawIM( imGraphInJSON )

import Problems.EvenParity.Funs


prog_type = asType :: [Bool]  -> Bool  
prog_typ  =           l_bool :-> bool             
prog_ctx  = ctx_yu             


reg = PO_CTTP_ PO_CTTP {
  cttp_code        = "ep"                                     ,
  cttp_info        = "Even-parity problem." ,
  cttp_data        = jsObj [ ( "im" , imGraphInJSON prog_typ prog_ctx ) ] ,
  cttp_numRuns     = IntSlider "Runs"            1 100   50   1   ,
  cttp_numGene     = IntSlider "Generations"     0 500   50   10  ,
  cttp_popSize     = IntSlider "Population size" 0 5000  500  100 ,
  
  cttp_typ         = prog_typ                                     ,
  cttp_ctx         = prog_ctx                                     ,
  
  cttp_gOpt        = CTTG_Geom     prog_typ prog_ctx 0.75 , 
--cttp_gOpt        = CTTG_Koza2    prog_typ prog_ctx  , 
--cttp_gOpt        = CTTG_AllEdges prog_typ prog_ctx  , 

  cttp_ff          = FF6 prog_type ff "Problems.EvenParity.Funs", 
  
  cttp_saveBest    = True
}

rege = PO_CTTeP_ PO_CTTeP {
  cttep_code        = "ep2"                                     ,
  cttep_info        = "Even-parity problem. With @-Trees." ,
  cttep_data        = jsObj [ ( "im" , imGraphInJSON prog_typ prog_ctx ) ] ,
  cttep_numRuns     = IntSlider "Runs"            1 100   50   1   ,
  cttep_numGene     = IntSlider "Generations"     0 500   50   10  ,
  cttep_popSize     = IntSlider "Population size" 0 5000  500  100 ,
  
  cttep_typ         = prog_typ                                     ,
  cttep_ctx         = prog_ctx                                     ,
  
  cttep_gOpt        = CTTermG_Geom     prog_typ prog_ctx 0.75 , 
--cttp_gOpt        = CTTG_Koza2    prog_typ prog_ctx  , 
--cttp_gOpt        = CTTG_AllEdges prog_typ prog_ctx  , 

  cttep_ff          = FF6 prog_type ff "Problems.EvenParity.Funs", 
  
  cttep_saveBest    = True
}




bool   = Typ "Bool"
l_bool = Typ "[Bool]"

ctx = [
 ( "foldr" , (bool:->bool:->bool) :-> bool :-> l_bool :-> bool ),
 ( "(==)"  , bool :-> bool :-> bool                            ),
 ( "(/=)"  , bool :-> bool :-> bool                            ),
 ( "(&&)"  , bool :-> bool :-> bool                            ),
 ( "(||)"  , bool :-> bool :-> bool                            ),
 ( "not"   , bool :-> bool                                     ),
 ( "False" , bool                                              ),
 ( "True"  , bool                                              ),
 ( "head_" , l_bool :-> bool                                   ),
 ( "tail_" , l_bool :-> l_bool                                 )]

ctx_yu = [
 ( "(&&)"  , bool :-> bool :-> bool                            ),
 ( "(||)"  , bool :-> bool :-> bool                            ),
 ( "nand"  , bool :-> bool :-> bool                            ),
 ( "nor"   , bool :-> bool :-> bool                            ),
 ( "foldr" , (bool:->bool:->bool) :-> bool :-> l_bool :-> bool ),
 ( "head_" , l_bool :-> bool                                   ),
 ( "tail_" , l_bool :-> l_bool                                 )]


ff :: ([Bool]->Bool) -> (Double,Bool)
ff prog = 
  let res = [ ff_n 2 prog , ff_n 3 prog ]
   in ( (sum $ map fst res)/2 , and $ map snd res )

ff_n :: Int -> ([Bool]->Bool) -> (Double,Bool)
ff_n n prog = 
  let numOk = length $ filter (\bs->evenParity bs == prog bs) (allBools n)
      maxOk = 2 ^ n
   in ( (fromIntegral numOk) / (fromIntegral maxOk) , numOk == maxOk )


evenParity :: [Bool] -> Bool
evenParity xs = ( length (filter id xs) `mod` 2 ) == 0

evenParity' :: [Bool] -> Bool
evenParity' xs = foldr (/=) True xs



allBools :: Int -> [[Bool]]
allBools 0 = [[]]
allBools n = 
  let xs = allBools $ n-1
   in (map (False:) xs) ++ (map (True:) xs)




------------------------------------------------------------------------------------
-- solution GALLERY ----------------------------------------------------------------
------------------------------------------------------------------------------------


solution1 = \ x0 -> foldr (s (s (k s) (s (k (s (k (&&)))) (s (s (k s) (s (k k) 
  (s (k nand) i))) (s (s (k s) (s (k k) (s (k (&&)) i))) (k i))))) (||)) 
  (nand (foldr (s (k k) (s (s (k (&&)) (s (s (k (&&)) i) (s (s (k (&&)) i) i))) 
  (k (head_ x0)))) (head_ x0) x0) (head_ x0)) (tail_ x0)

solution2 =
 \ x0 -> foldr (s (s (k s) (s (k (s (k nor))) (s (s (k s) (s (k (s (k (&&))
 )) (s (s (k s) (s (k k) (s (k k) i))) (k i)))) (k i)))) (s (k nor) i)) (foldr 
 (s (s (k s) (s (k (s (s (k foldr) (s (k k) (s (k (s (s (k (&&)) i))) (
 s (k k) (s (s (k nand) i) (s (s (k nor) i) i)))))))) (s (s (k s) (s (k (s
 (k nand))) (s (s (k s) (s (k k) (s (k nor) i))) (k i)))) (k i)))) (k (k x0
 ))) (nor (nor (nor (head_ (tail_ x0)) (head_ x0)) (head_ x0)) ((||) (head_
  (tail_ x0)) (foldr (k (s (s (k (&&)) i) (s (s (k (||)) (s (s (s (k s) (s
 (k (s (k nor))) (s (k (&&)) i))) (s (k k) i)) (s (s (s (k s) (k nand)) (s
 (k (s (s (k nor) i))) (s (k k) i))) i))) i))) (foldr (s (s (k s) (k nand))
  (s (k k) i)) (head_ (tail_ x0)) x0) (tail_ x0)))) (tail_ x0)) x0

-- pop 1000
solution3 = 
 \ x0 -> foldr (s (s (k s) (s (k (s (k (&&)))) (s (k (s (s (k (||)) i))) (s
  (k k) i)))) (s (k nand) i)) (nand (head_ x0) (head_ x0)) (tail_ x0)



-- pokus 500 - 50 : 50% uspešnost (5 z 10 běhů) (Geom 0.75)

solution4 = 
 \ x0 -> foldr (s (s (k s) (s (k (s (k (&&)))) (s (k (||)) i))) (s (k (s (s (k nand) 
  (s (s (k (&&)) i) (s (s (k (&&)) i) i))))) (s (k k) i))) (nand (foldr (s (s
  (k s) (s (k (s (k nor))) (s (k nand) i))) (k i)) (head_ x0) (tail_ x0)) (nand (
  head_ (tail_ (tail_ x0))) (head_ (tail_ x0)))) x0

solution5 =  
 \ x0 -> foldr (s (s (k s) (s (k (s (k (||)))) (s (s (k s) (s (k (s (k (&&)))) nand)) 
  (s (k k) i)))) (s (s (k s) (s (k (s (k (&&)))) (s (s (k s) (s (k k) (s (k
  nand) i))) (k i)))) (s (k (s k)) (s (s (k k) (s (k nand) i)) (s (s (s (k foldr)
  (s (k k) (s (k k) i))) i) (k x0)))))) (foldr (s (s (k s) (s (s (k s) (s (k (s (k
  foldr))) (s (k (s (k k))) (s (s (k s) (s (k k) (k (s (s (k (||)) i))))) (s (k k
  ) (s (k nand) i)))))) (s (k k) i))) (k (k x0))) (head_ x0) x0) x0

solution6 =   
 \ x0 -> foldr (s (s (k s) (s (s (k s) (s (k (s (k foldr))) (s (k (s (k k))) (s
  (k (s (k k))) (s (s (k s) (s (k (s (k nor))) (s (s (k s) (s (k k) (s (k nor) i))
  ) (k i)))) (&&)))))) (s (k (s (s (k nor) i))) (s (s (k s) (s (k k) (s (k nand) i
  ))) (k (s (s (k (||)) i) i)))))) (k (k x0))) (foldr (k (s (s (s (k s) (s (k (s (
  k nand))) (s (k k) i))) (s (s (k s) (s (k (s (k nand))) (s (k (s (s (k (||)) i))
  ) (s (k k) i)))) (k i))) i)) (head_ x0) (tail_ x0)) x0

solution7 =   
 \ x0 -> foldr (s (s (k s) (s (k (s (k nor))) (s (k (s (s (k (&&)) i))) (s (s (k
  s) (s (k k) (s (k (&&)) i))) (s (s (k s) (s (k (s (k (&&)))) (s (s (k s) (s (k
  (s (k (&&)))) (s (s (k s) (s (k k) (s (k (&&)) i))) (k i)))) (k i)))) (k i))))))
  (s (s (k s) (s (k k) (s (k nor) i))) (s (s (k s) (s (k (s (k (||)))) (s (k k) i
  ))) (s (s (k s) (s (k (s (s (k foldr) (s (k (s (k k))) (s (k k) i))))) (s (k k)
  i))) (k (k x0)))))) (foldr (s (k (s (s (k nor) i))) (s (k (s (k (s (s (k (&&)) (
  s (s (s (k foldr) (s (k k) (s (k k) (s (s (k nand) i) i)))) (s (k (nor (head_ x0
  ))) i)) (k x0))))) (s (k k) (s (k ((&&) (head_ x0))) i)))) (s (s (k (foldr (k i)
  )) (s (s (k (&&)) i) i)) (k x0)))) (foldr (s (s (k s) (s (k (s (k nor))) (s (s (
  k s) (s (s (k s) (s (k (s (k foldr))) (s (k (s (k k))) (s (k (s (k k))) (s (k (&&)
  ) i))))) (k (s (s (k (&&)) i) i)))) (k (k (tail_ x0)))))) (s (s (k s) (k (&&))
  ) (s (s (k s) (s (k (s (s (k foldr) (s (k k) (s (k k) i))))) (s (k (s (s (k (&&)
  ) i))) (s (k k) i)))) (k (k x0))))) (foldr nand (head_ (tail_ x0)) x0) x0) x0) (
  tail_ x0)

solution8 =   
 \ x0 -> foldr (s (s (k s) (s (k (s (k (&&)))) nand)) (s (s (k s) (s (k k) (s (k
  (||)) i))) (k i))) (nand (head_ x0) (nor (head_ (tail_ x0)) ((&&) (foldr (s (k
  nand) i) (head_ (tail_ x0)) (tail_ x0)) (head_ x0)))) x0

-- end 'pokus'

solution9 =
  \ x0 -> foldr (s (s (k s) (s (k (s (k (&&)))) (s (s (k s) (s (k k) (s (k 
   nand) i))) (s (k (s (s (k (&&)) i))) (s (k k) i))))) (s (k (s (s (k (||)) i
   ))) (s (k k) i))) (foldr (k (s (s (k (||)) (s (s (k nor) i) i)) i)) (head_
    (tail_ x0)) (tail_ x0)) x0