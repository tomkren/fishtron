module TTree2Kutil where

import Data.List
import Data.Maybe

import TTerm
import TTree 



data XML = Tag String [(String,String)] [XML]
         | Text String

--ttree2kutil :: 

type Pre = ( ID , Val , Target , Pos , IsInput )

type Target = [( ID , Port )]


type ID   = String
type Val  = String
type Port = Int
type Pos  = (Int,Int)

type IsInput = Maybe (Int,ID)


ttree2file :: CTT -> IO ()
ttree2file ttree = writeFile "../../kutil/from-fishtron-2.xml" (ttree2kutil ttree)

ttree2kutil :: CTT -> String
ttree2kutil ttree = 
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
  (show $ Tag "kutil" [] (ttree2xmls ttree) )

ttree2xmls :: CTT -> [XML]
ttree2xmls ctt@(CTT ctx ttree) = 
  let (xmls,isInputs) = unzip $ ( map (pre2xml ttree) $ ttree2pre ctx ttree )
      idss = proccessIsIputs isInputs
      comment = Tag "object" [("type","comment"),("val", show ctt ),("pos","-33 106")] [] 
      out     = Tag "object" [("type","out"),("id","_out_"),
                              ("pos","100 "++(show $ 100 * (2+ttreeDepth ttree)))] []
      copyXmls = mkCopyss idss  
   in (copyXmls ++ xmls ++ [out,comment]) 

subsQuot :: String -> String
subsQuot str = map (\x->if x=='\"' then '\'' else x ) str

mkCopyss :: [[ID]] -> [XML]
mkCopyss idss = 
  let copyXmls = concat . map snd $ cols
      inXmls   = map mkIn (zip3 inIDs inTargets inPoses )
   in inXmls ++ copyXmls
 where
  cols       = map mkCopys (zip3 startIDs startPoses idss ) 
  startIDs   = map (\i-> "_c_" ++ show i) [1..]
  startPoses = map (\x-> (x*100,0) ) [1..]
  inTargets  = map fst cols
  inIDs      = map (\i-> "_in_" ++ show i) [1..]
  maxNumCopys= maximum $ map length idss 
  inPoses    = map (\x-> (x*100,-100*(maxNumCopys-1)) ) [1..]



mkIn :: (ID , Maybe ID , Pos ) -> XML
mkIn ( iD,targetID , (x,y) ) = 
 Tag "object" ((case targetID of Just tid -> [("target",tid++":0")] ; _ -> [] ) ++
  [("type","in"),("id",iD),("pos", show x ++ " " ++ show y),("physical","true"),("attached","true")]) []



mkCopys :: (ID , Pos , [ID]) -> ( Maybe ID , [XML])
mkCopys (_     , _    , []            ) = ( Nothing , [])
mkCopys (_     , _    , [iD]          ) = ( Just iD , [])
mkCopys (nextID, (x,y), (iD1:iD2:iDs) ) = 
 let xml = Tag "object" [( "type"   , "function"        ) , 
                         ( "id"     , nextID            ) , 
                         ( "val"    , "copy"            ) , 
                         ( "target" , iD1 ++ ":0 " ++ iD2 ++ ":0" ) ,
                         ( "pos"    , show x ++ " " ++ show y ) ] []
     (retID , xmls) = mkCopys ((nextID++"_") , (x,y-100) , (nextID:iDs) )
  in (retID , xml:xmls ) 

proccessIsIputs :: [IsInput] -> [[ID]]
proccessIsIputs = (proccessIsIputs2 0) . proccessIsIputs1

proccessIsIputs1 :: [IsInput] -> [(Int,ID)]
proccessIsIputs1 xs = sort . concatMap maybeToList $ xs

proccessIsIputs2 :: Int -> [(Int,ID)] -> [[ID]]
proccessIsIputs2 _ [] = []
proccessIsIputs2 i xs =
 let ( ys , zs ) = span (\(j,_)->j==i) xs
  in (map snd ys) : (proccessIsIputs2 (i+1) zs) 

pre2xml :: TTree -> Pre -> ( XML , IsInput )
pre2xml ttree ( iD , val , target , pos , isInput ) = 
  ( Tag "object" [( "type"   , "function"        ) , 
                  ( "id"     , iD                ) , 
                  ( "val"    , val               ) , 
                  ( "target" , showTarget target ) ,
                  ( "pos"    , showPos ttree pos ) ] [] , isInput )

showTarget :: Target -> String
showTarget target = intercalate " " $ map (\(iD,port)->iD ++ ":" ++ show port) target

showPos :: TTree -> Pos -> String
showPos ttree (x,y) = show ((x+1)*100) ++ " " ++ show (( ttreeDepth ttree - y + 1 )*100)


--findInputs :: Context -> TTree -> [[ID]]



ttree2pre :: Context -> TTree -> [Pre]
ttree2pre ctx tt = ttree2pre' ( "1" , [("_out_",0)] , (0,0) , 0 , tt) 
 where
  vars = map fst ctx
  ttree2pre' :: (ID , Target , Pos , Int , TTree) -> [Pre]
  ttree2pre' ( iD , target , pos@(x,y) , childX , TTree sym _ ts ) =
    ( iD , sym2val sym , target  , pos , isInput ) : concatMap ttree2pre' (zip5 ids targs poses childXs ts)
   where
    ids       = map (\i-> iD ++ "_" ++ show i ) [1..]
    targs     = map (\i-> [(iD,i)] ) [0..] 
    childNums = map (\(TTree _ _ tz)->length tz) ts
    childXs   = scanl (+) 0 childNums
    poses     = map (\dx->(childX+dx,y+1) ) [0..] 

    isInput = 
      if null ts then
        foo 0 sym vars
      else
        Nothing

    sym2val :: Symbol -> Val
    sym2val sym = 
      if null ts then
        if sym `elem` vars then
          "id"
        else 
          "const " ++ sym
      else
        sym


    foo :: Int -> String -> [String] -> IsInput
    foo _ _ [] = Just (0,iD)
    foo i sym (v:vars) 
      | v == sym = Just (i,iD) 
      |otherwise = foo (i+1) sym vars 


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

ctt1@(CTT _ ttree1) = mkCTT2 [("x0",i),("x1",i)] tt1

i = Typ "I"

a = TVal "plus3" (i:->i:->i:->i)
b = TVal "+" (i:->i:->i)
c = TVal "x0" i
d = TVal "3" i
e = TVal "inc" (i:->i)
f = TVal "x1" i
g = TVal "4" i

tt1 = TApp (TApp (TApp a (TApp (TApp b c (i:->i)) d i) (i:->i:->i)) (TApp e f i) (i:->i)) g i



