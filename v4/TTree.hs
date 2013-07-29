-- {-# LANGUAGE OverlappingInstances #-}

module TTree 
( CTT (..),
  TTree (..),
  TTPos,
  --mkCTT, -- <============== možná bug v něm!!!! 
  mkCTT2,
  ttreeSubtree,
  ttreeSubtrees,
  ttreeChangeSubtree,
  ttreeDepth,
  ttreePoses2,

  ttreePoses2WithTyps,
  ttreePoses2ByTyp,
  ttreePoses2WithTyps_onlyCompatible
) where

import Data.List
import Data.Either
import Data.Maybe

import Text.JSON (JSValue (..) , toJSObject , toJSString )
import Utils ( JShow , jshow_js, jss_size )

import JSONUtils

import TTerm


import qualified Data.Set as Set
--import qualified Data.Map as Map
import Data.Set (Set)
--import Data.Map (Map)


data CTT = CTT Context TTree 

data TTree = TTree Symbol Typ [TTree] 

type TTPos = [Int]




mkCTT2 :: Context -> TTerm -> CTT
mkCTT2 ctx tt = CTT ctx (fromTTerm tt)


instance Show CTT where
 show (CTT ctx ttree) = case ctx of
  [] -> show ttree
  _  -> let vars = intercalate " " . map fst $ ctx
         in "\\ "++ vars ++ " -> " ++ show ttree 

instance JShow CTT where
  jss_size (CTT _ ttree) = Just $ ttreeSize ttree 
  jshow_js ctt = Just . jsStr . jsShow $ ctt
--  jshow ctt = JSObject $ toJSObject [ 
--   ("type"     , JSString . toJSString $ "jsonout"     ) ,
--   ("haskell"  , JSString . toJSString $ show ctt      ) ,
--   ("js"       , JSString . toJSString $ jsShow ctt    ) 
--   --("kutil"    , JSString . toJSString $ ctt2kutil ctt ) 
--   ]


jsShow :: CTT -> String
jsShow (CTT ctx ttree) =
 let vars = intercalate "," . map fst $ ctx
  in "function("++ vars ++ "){return " ++ jsShowBody ttree ++ ";}"

jsShowBody :: TTree -> String
jsShowBody (TTree symbol typ ttrees ) =
 let symbol' = transformExceptions symbol 
 in case ttrees of
     [] -> symbol'
     _  ->
      if isBinop symbol' && length ttrees == 2
       then let [l,r] = ttrees
                op    = tail . init $ symbol'
             in "(" ++ jsShowBody l ++ op ++ jsShowBody r ++ ")"
       else let inside = intercalate "," . map jsShowBody $ ttrees 
             in symbol' ++ "(" ++ inside ++ ")"

isBinop :: Symbol -> Bool
isBinop ['(',x  ,')'] =  x    `elem` ['*','+','-','/','%']
isBinop ['(',x,y,')'] = [x,y] `elem` ["=="]
isBinop _ = False

transformExceptions :: Symbol -> Symbol
transformExceptions "(:)"  = "cons"
transformExceptions "if'"  = "if_"
transformExceptions "(==)" = "equals"
transformExceptions "(/=)" = "notEquals"
transformExceptions "(<=)" = "lte"
transformExceptions "(&&)" = "and"
transformExceptions "(||)" = "or"
transformExceptions "True" = "true"
transformExceptions "False"= "false"
transformExceptions x      = x


instance Show TTree where
 show ttree = case showPars ttree of
   '(' : str -> init str
   str       -> str  
  where  
   showPars (TTree x _ ts) = case ts of
    [] -> x
    _  -> "(" ++ x ++ " " ++ (intercalate " " (map showPars ts)) ++ ")"


ttreeDepth :: TTree -> Int
ttreeDepth (TTree _ _ ts) = case ts of
 [] -> 0
 _  -> (1+) . maximum $ map ttreeDepth ts

ttreeSize :: TTree -> Int
ttreeSize (TTree _ _ ts) = 1 + ( sum $ map ttreeSize ts )

ttreeSubtree :: TTree -> TTPos -> TTree
ttreeSubtree t [] = t
ttreeSubtree (TTree _ _ ts) (i:is) = 
 ttreeSubtree (ts !! (i-1)) is

ttreeSubtrees :: TTree -> [TTree]
ttreeSubtrees t@(TTree _ _ ts) = t : concatMap ttreeSubtrees ts

ttreeChangeSubtree :: TTree -> TTPos -> TTree -> (TTree,TTree)
ttreeChangeSubtree tree               []     newSub = (newSub,tree) 
ttreeChangeSubtree (TTree str typ ts) (i:is) newSub = 
 let (ts1,subt:ts2) = splitAt (i-1) ts
     (subt',oldSub) = ttreeChangeSubtree subt is newSub
  in (TTree str typ (ts1 ++ (subt':ts2) ) ,oldSub)  

ttreePoses2 :: TTree -> ([TTPos],[TTPos])
ttreePoses2 t = 
  let xs  = poses2 [] t 
      rev = map reverse 
   in ( rev . lefts $ xs , rev . rights $ xs )
 where
  poses2 :: [Int] -> TTree -> [ Either [Int] [Int] ]
  poses2 pos (TTree _ _ []) = [Left pos]
  poses2 pos (TTree _ _ ts) = 
   (Right pos) : (concatMap (\(i,t)-> poses2 (i:pos) t ) (zip [1..] ts) )

ttreePoses2WithTyps :: TTree -> ([(TTPos,Typ)],[(TTPos,Typ)])
ttreePoses2WithTyps t = 
  let xs  = poses2xx [] t 
      rev = map (\(pos,typ)->(reverse pos,typ))
   in ( rev . lefts $ xs , rev . rights $ xs )
 where
  poses2xx :: [Int] -> TTree -> [ Either ([Int],Typ) ([Int],Typ) ]
  poses2xx pos (TTree _ typ []) = [Left (pos,typ)]
  poses2xx pos (TTree _ typ ts) = 
   (Right (pos,typ)) : (concatMap (\(i,t)-> poses2xx (i:pos) t ) (zip [1..] ts) )

 

ttreePoses2WithTyps_onlyCompatible :: TTree -> TTree -> ([(TTPos,Typ)],[(TTPos,Typ)])
ttreePoses2WithTyps_onlyCompatible prvni druhej =
 let (ters,nonters) = ttreePoses2WithTyps prvni
     setTypuVDruhym = ttreeTypsSet druhej
     vyhodCoNemajTypVDruhym poziceSTypy 
       = filter ( \(_,typ) -> Set.member typ setTypuVDruhym ) poziceSTypy
  in ( vyhodCoNemajTypVDruhym ters , vyhodCoNemajTypVDruhym nonters  )


ttreeTypsSet :: TTree -> Set Typ
ttreeTypsSet (TTree _ typ ts) = Set.unions $ (Set.singleton typ) : (map ttreeTypsSet ts)    




ttreePoses2ByTyp :: Typ -> TTree -> ([TTPos],[TTPos])
ttreePoses2ByTyp typ t = 
  let xs  = poses2 [] t 
      rev = map reverse 
   in ( rev . lefts $ xs , rev . rights $ xs )
 where
  poses2 :: [Int] -> TTree -> [ Either [Int] [Int] ]
  poses2 pos (TTree _ typ' []) | typ' == typ = [Left pos]
                               | otherwise   = []
  poses2 pos (TTree _ typ' ts) = 
   (if typ' == typ then [Right pos] else []) ++ (concatMap (\(i,t)-> poses2 (i:pos) t ) (zip [1..] ts) )


mkCTT :: TTerm -> CTT
mkCTT tt = 
 let (ctx,ttree) = mkCTT' [] tt
  in CTT ctx ttree

mkCTT' :: [Symbol] -> TTerm -> ( Context , TTree ) 
mkCTT' xs tt = case tt of
 TLam x m (tx:->_) -> 
  let ( rest , ret ) = mkCTT' (x:xs) m
   in ( (x,tx) : rest , ret ) 
 _ -> ( [] , fromTTerm . toSki . changeVarsToVals xs $ tt )  

changeVarsToVals :: [Symbol] -> TTerm -> TTerm
changeVarsToVals xs tt = case tt of
 TVar x   t | x `elem` xs -> TVal x t
            | otherwise   -> TVar x t 
 TVal v   t -> TVar v t -- <=========================================================================== NENI TO BUG ???
 TLam x m t -> TLam x (changeVarsToVals (xs \\ [x]) m ) t
 TApp m n t -> TApp (changeVarsToVals xs m) (changeVarsToVals xs n) t
 

fromTTerm :: TTerm -> TTree
fromTTerm = reverseTs . fromTTerm'
 where reverseTs (TTree x t ts) = TTree x t ( reverse . map reverseTs $ ts ) 

fromTTerm' :: TTerm -> TTree
fromTTerm' tt = case tt of
 TLam _ _ _ -> error "tterm must be without lambdas"
 TVar x   t -> TTree x t []
 TVal v   t -> TTree v t []
 TApp m n t -> 
  let TTree f _ trees = fromTTerm' m 
   in TTree f t ( (fromTTerm' n) : trees  ) 



-- i = Typ "I"
-- 
-- a = TVal "a" (i:->i:->i:->i)
-- b = TVal "b" (i:->i:->i)
-- c = TVal "c" i
-- d = TVal "d" i
-- e = TVal "e" (i:->i)
-- f = TVal "f" i
-- g = TVal "g" i
-- 
-- tt1 = TApp (TApp (TApp a (TApp (TApp b c (i:->i)) d i) (i:->i:->i)) (TApp e f i) (i:->i)) g i



---  TTree2Kutil ----------------------------------------------------------------------------

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
ttree2file ctt = writeFile "../../kutil/from-fishtron-2.xml" (ctt2kutil ctt)

ctt2kutil :: CTT -> String
ctt2kutil ctt = 
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
  (show $ Tag "kutil" [] (ctt2xmls ctt) )

ctt2xmls :: CTT -> [XML]
ctt2xmls ctt@(CTT ctx ttree) = 
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
    ( iD , sym2val sym (length ts) , target  , pos , isInput ) : concatMap ttree2pre' (zip5 ids targs poses childXs ts)
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

    sym2val :: Symbol -> Int -> Val
    sym2val sym numArgs = 
      if null ts then
        if sym `elem` vars then
          "id"
        else 
          "const " ++ (transform sym numArgs)
      else
        transform sym numArgs

    transform :: Symbol -> Int -> Val 
    transform sym numArgs = case sym of
     "i" -> "id"
     "s" -> "s2"
     "k" | numArgs == 0 -> "const"
         | numArgs == 1 -> "k1" 
     "Nothing" -> "[Nothing]"
     "[]" -> "'()"
     "if'" -> "if_"
     "True" -> "[true]"
     "False" -> "[false]"
     "(:)"   -> ":"
     "(==)"  -> "=="   
     x -> x 


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



