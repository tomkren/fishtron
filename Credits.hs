module Credits where


type Credit = Double

data Cr a b = Cr { crFun :: (a->b) , crCost :: (a->Credit) }



cr_succ :: Cr Int Int
cr_succ = Cr succ (const 1) 

cr_sum :: Num a => Cr [a] a
cr_sum = Cr sum crLen 

cr_map :: Cr ( Cr a b , [a] ) [b]
cr_map = Cr fun cost
 where 
  fun :: ( Cr a b , [a] ) -> [b]
  fun ( cr_f , xs ) = map (crFun cr_f) xs
  cost :: ( Cr a b , [a] ) -> Credit
  cost ( cr_f , xs ) = sum . map (crCost cr_f) $ xs

dot :: Cr b c -> Cr a b -> Cr a c
dot f g = Cr fun cost
 where 
  fun    = (crFun f) . (crFun g) 
  cost x = (crCost g x) + (crCost f (crFun g x) ) 



crLen :: [a] -> Credit
crLen = fromIntegral . length

