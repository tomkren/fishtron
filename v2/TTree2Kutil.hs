module TTree2Kutil where

import Data.List

import TTerm
import TTree 



data XML = Tag String [(String,String)] [XML]
         | Text String

--ttree2kutil :: 

type Pre = ( ID , Val , Target , Pos )

type Target = [( ID , Port )]


type ID   = String
type Val  = String
type Port = Int
type Pos  = (Int,Int)


ttree2file :: TTree -> IO ()
ttree2file ttree = writeFile "../../kutil/from-fishtron-2.xml" (ttree2kutil ttree)

ttree2kutil :: TTree -> String
ttree2kutil ttree = 
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
  (show $ Tag "kutil" [] (ttree2xmls ttree) )

ttree2xmls :: TTree -> [XML]
ttree2xmls ttree = map (pre2xml ttree) . ttree2pre $ ttree 


pre2xml :: TTree -> Pre -> XML
pre2xml ttree ( iD , val , target , pos ) = 
  Tag "object" [( "type"   , "function"        ) , 
                ( "id"     , iD                ) , 
                ( "val"    , val               ) , 
                ( "target" , showTarget target ) ,
                ( "pos"    , showPos ttree pos ) ] [] 

showTarget :: Target -> String
showTarget target = intercalate " " $ map (\(iD,port)->iD ++ ":" ++ show port) target

showPos :: TTree -> Pos -> String
showPos ttree (x,y) = show ((x+1)*100) ++ " " ++ show (( ttreeDepth ttree - y + 1 )*100)

ttree2pre :: TTree -> [Pre]
ttree2pre tt = ttree2pre' ( "1" , [] , (0,0) , tt) 

ttree2pre' :: (ID , Target , Pos , TTree) -> [Pre]
ttree2pre' ( iD , target , pos@(x,y) , TTree sym _ ts ) =
  ( iD , sym2val sym , target  , pos ) : concatMap ttree2pre' (zip4 ids targs poses ts)
 where
  ids   = map (\i-> iD ++ "_" ++ show i ) [1..]
  targs = map (\i-> [(iD,i)] ) [0..] 
  poses = map (\dx->(x+dx,y+1) ) [0..] 

sym2val :: Symbol -> Val
sym2val = id



instance Show XML where
 show xml = showXml 0 xml
  where 
   showXml ods xml = case xml of
     Text str -> str
     Tag tag atts [] 
      -> odsStr++"<"++tag++(showAtts atts)++" />" 
     Tag tag atts inside 
      -> odsStr++"<"++tag++(showAtts atts)++">"++(showInside inside)++"\n</"++tag++">" 
    where
     odsStr     = replicate (ods*2) ' '
     showAtts   = concatMap (\(key,val)->" "++key++"="++"\""++val++"\"")
     showInside = concatMap (\xml-> '\n' : showXml (ods+1) xml  )

-- testovací.... 

CTT _ ttree1 = mkCTT2 [] tt1

i = Typ "I"

a = TVal "plus3" (i:->i:->i:->i)
b = TVal "+" (i:->i:->i)
c = TVal "id" i
d = TVal "id" i
e = TVal "inc" (i:->i)
f = TVal "id" i
g = TVal "id" i

tt1 = TApp (TApp (TApp a (TApp (TApp b c (i:->i)) d i) (i:->i:->i)) (TApp e f i) (i:->i)) g i



