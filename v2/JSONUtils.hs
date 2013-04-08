module JSONUtils where

import Text.JSON

jsObj :: [(String, JSValue)] -> JSValue
jsObj = JSObject . toJSObject  

jsArr :: [JSValue] -> JSValue
jsArr = JSArray

jsStr :: String -> JSValue
jsStr = JSString . toJSString

jsNum :: Real a => a -> JSValue
jsNum = JSRational True . toRational

