module Problems.BigCtx.Problem where

import TTerm (Typ(..),Context)
import ProblemUtils 
import JSONUtils
import GP_Core (FitFun(FF6))
import GP_Data (CTTGen(..))

import DrawIM( imGraphInJSON )

 
reg = PO_CTTP_ PO_CTTP {
  cttp_code        = "bigctx"                                     ,
  cttp_info        = "Problem with basic funs for types generating elementary funs." ,
  cttp_data        = jsObj [ ( "im" , imGraphInJSON prog_typ prog_ctx ) ]      ,
  cttp_numRuns     = IntSlider "Runs"            1 10    1    1   ,
  cttp_numGene     = IntSlider "Generations"     0 100   10   10  ,
  cttp_popSize     = IntSlider "Population size" 0 5000  500  100 ,
  
  cttp_typ         = prog_typ                                     ,
  cttp_ctx         = prog_ctx                                     ,
  
  cttp_gOpt        = CTTG_Koza2 prog_typ prog_ctx                 , 

  cttp_ff          = FF6 prog_type ff_fst3 "Problems.BigCtx.Funs" 
  
}


prog_type = asType :: Fst3_Type  -- asType :: [Int] -> Maybe Int
prog_typ  = fst3_typ             -- l_int :-> m_int  
prog_ctx  = ctx_fst3             -- ctx_combo

--problem1 = go pr_head

go (ff,ctx,typ,as) = cttProblem5 "BigCtx" ff typ ctx as


bool   = Typ "Bool"
int    = Typ "Int"
m_int  = Typ "Maybe Int"
l_int  = Typ "[Int]"
ml_int = Typ "Maybe [Int]"




--(ff,ctx,typ,as) = pr_head


pr_head   = go ( ff_head   , ctx_combo , l_int :-> m_int                 , asType :: [Int] -> Maybe Int           )
pr_tail   = go ( ff_tail   , ctx_combo , l_int :-> ml_int                , asType :: [Int] -> Maybe [Int]         )
pr_map    = go ( ff_map    , ctx_combo , (int:->int) :-> l_int :-> l_int , asType :: (Int->Int) -> [Int] -> [Int] )
pr_filter = go ( ff_filter , ctx_combo , (int:->bool):-> l_int :-> l_int , asType :: (Int->Bool)-> [Int] -> [Int] )
pr_elem   = go ( ff_elem   , ctx_combo , int :-> l_int :-> bool          , asType :: Int -> [Int] -> Bool         )

ctx_combo = ctx_head ++ ctx_tail ++ ctx_map ++ ctx_filter' ++ ctx_elem

ctx_filter = ctx_filter' ++ ctx_map

ctx_fst3 =    [  ( "mkFst3", ( l_int :-> m_int                  ) :->
                             ( l_int :-> ml_int                 ) :->
                             ( (int:->int) :-> l_int :-> l_int  ) :-> fst3_typ ) ] ++ ctx_head ++ ctx_tail ++ ctx_map


ctx_mkAll =   [  ( "mkAll"  , ( l_int :-> m_int                  ) :->
                              ( l_int :-> ml_int                 ) :->
                              ( (int:->int) :-> l_int :-> l_int  ) :->
                              ( (int:->bool):-> l_int :-> l_int  ) :->
                              ( int :-> l_int :-> bool           ) :-> all_typ ) ]

ctx_head =    [  ( "listCase" , l_int :-> m_int  :-> (int:->l_int:->m_int ) :-> m_int  ),
                 ( "Nothing"  , m_int                                                  ),
                 ( "Just"     , int :-> m_int                                          )]

ctx_tail =    [  ( "listCase" , l_int :-> ml_int :-> (int:->l_int:->ml_int) :-> ml_int ),
                 ( "Nothing"  , ml_int                                                 ),
                 ( "Just"     , l_int :-> ml_int                                       )]

ctx_map  =    [  ( "foldr"    , (int:->l_int:->l_int) :-> l_int :-> l_int :-> l_int    ),
                 ( "(:)"      , int :-> l_int :-> l_int                                ),
                 ( "[]"       , l_int                                                  )]

ctx_filter' = [  ( "if'"      , bool :-> l_int :-> l_int :-> l_int                     )]

ctx_elem    = [  ( "foldr"    , (int:->bool:->bool) :-> bool :-> l_int :-> bool        ),
                 ( "if'"      , bool :-> bool :-> bool :-> bool                        ),
                 ( "(==)"     , int:->int:->bool                                       ),
                 ( "True"     , bool                                                   ),
                 ( "False"    , bool                                                   )]


all_typ = Typ "All"

type All_Type = 
 ( [Int] -> Maybe Int    ,
   [Int] -> Maybe [Int]  ,
   (Int->Int ) -> [Int] -> [Int] ,
   (Int->Bool) -> [Int] -> [Int] ,
   Int -> [Int] -> Bool )

fst3_typ = Typ "Fst3"

type Fst3_Type = 
 ( [Int] -> Maybe Int    ,
   [Int] -> Maybe [Int]  ,
   (Int->Int ) -> [Int] -> [Int]  )



ff :: All_Type -> (Double,Bool)
ff (h,t,m,f,e) = 
  let res = [ ff_head h , ff_tail t , ff_map m , ff_filter f , ff_elem e ]
   in ( sum $ map fst res , and $ map snd res )

ff_fst3 :: Fst3_Type -> (Double,Bool)
ff_fst3 (h,t,m) = 
  let res = [ ff_head h , ff_tail t , ff_map m ]
   in ( sum $ map fst res , and $ map snd res )



ff_head :: ( [Int] -> Maybe Int ) -> (Double,Bool)
ff_head prog = casesFF      
  [ prog []       == Nothing  
  , prog [1]      == Just 1   
  , prog [42,7,3] == Just 42  
  ]

ff_tail :: ( [Int] -> Maybe [Int] ) -> (Double,Bool)
ff_tail prog = casesFF
  [ prog []       == Nothing     
  , prog [1]      == Just []     
  , prog [42,7,3] == Just [7,3]  
  , prog [1..5]   == Just [2..5]  
  ]

ff_map :: ( (Int->Int) -> [Int] -> [Int] ) -> (Double,Bool)
ff_map prog = casesFF
  [ prog id      []      == []        
  , prog (+1234) [0]     == [1234]    
  , prog (*2)    [1,2,3] == [2,4,6]   
  ]

ff_filter :: ((Int->Bool)->[Int]->[Int]) -> (Double,Bool)
ff_filter prog = casesFF               
 [ prog odd           []     == []        
 , prog odd           [1..5] == [1,3,5]   
 , prog even          [1..5] == [2,4]     
 , prog (const True)  [1..4] == [1..4]    
 , prog (const False) [1..4] == []  
 ]

ff_elem :: ( Int -> [Int] -> Bool ) -> (Double,Bool)
ff_elem prog = casesFF          
 [ prog 42 []            == False  
 , prog 1  [1]           == True   
 , prog 1  [2]           == False  
 , prog 1  [1,2]         == True   
 , prog 2  [1,2]         == True   
 , prog 3  [1,2]         == False  
 , prog 5  [3,1,4,7,5,6] == True   
 , prog 2  [3,1,4,7,5,6] == False 
 ]








-- \ x0 x1 -> 
--  foldr 
--    (s 
--      (k k) 
--      (s 
--        (s 
--          (k (==)) 
--          i) 
--        (k x0))) 
--    False 
--    x1

-- \ x0 -> 
--  listCase 
--    x0 
--    Nothing 
--    (s 
--      (k k) 
--      (s 
--        (k Just) 
--        i))