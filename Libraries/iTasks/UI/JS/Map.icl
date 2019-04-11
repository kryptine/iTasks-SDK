implementation module iTasks.UI.JS.Map

import StdEnv
import StdMaybe
import iTasks.UI.JS.Interface

jsNewMap :: !*JSWorld -> *(!JSMap k JSVal, !*JSWorld) | toString k
jsNewMap world = jsEmptyObject world

jsPut :: !k !JSVal !(JSMap k JSVal) !*JSWorld -> *JSWorld | toString k
jsPut key val m world = (m .# toString key .= val) world
	
jsDel :: !k !(JSMap k JSVal) !*JSWorld -> *JSWorld | toString k
jsDel key m world = world // TODO
	
jsGet :: !k !(JSMap k JSVal) !*JSWorld -> *(!Maybe JSVal, !*JSWorld) | toString k
jsGet key m world
	# (val, world) = m .# toString key .? world
	| jsIsUndefined val
		= (Nothing, world)
		= (Just val, world)

jsToList :: !(JSMap k JSVal) !*JSWorld -> *(![(!String, !JSVal)], !*JSWorld) | toString k
jsToList m world
	= ([], world) /* TODO
	# (keys, world) = (jsGlobal "Object.keys" .$ m) world

	# keys = case fromJSValUnsafe keys of
					(keys :: [String]) 	= keys
										= abort "List of strings was expected but something else came"
									
	= foldl readprop ([], world) (reverse keys)
where
	readprop (ps, world) k = let (val, w) = jsGetObjectAttr k m world in ([(k,val):ps], w)*/
