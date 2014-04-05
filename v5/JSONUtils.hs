module JSONUtils where

import Text.JSON

jsObj :: [(String, JSValue)] -> JSValue
jsObj = JSObject . toJSObject 

jsProp :: JSValue -> String -> JSValue
jsProp (JSObject jso) prop = case lookup prop (fromJSObject jso) of
  Just x -> x



jsArr :: [JSValue] -> JSValue
jsArr = JSArray


jsStr :: String -> JSValue
jsStr = JSString . toJSString

fromJsStr :: JSValue -> String
fromJsStr (JSString jss) = fromJSString jss


jsNum :: Real a => a -> JSValue
jsNum = JSRational True . toRational

fromJsInt :: JSValue -> Int
fromJsInt (JSRational _ x) = round x


-- jsonová repre termů: (TODO jsVar)

jsVal :: String -> JSValue
jsVal sym = jsObj [
 ("c", jsStr "val"),
 ("x", jsStr sym  )]

jsApp :: JSValue -> JSValue -> JSValue
jsApp m n = jsObj [
 ("c", jsStr "app"),
 ("m", m),
 ("n", n)] 

jsLam :: String -> JSValue -> JSValue
jsLam x m = jsObj [
 ("c", jsStr "lam" ),
 ("x", jsStr x ),
 ("m", m )]