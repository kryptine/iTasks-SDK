definition module iTasks.API.Core.Client.Map

import iTasks.API.Core.Client.Interface
	
:: JSMap k v 

jsNewMap :: 									!*JSWorld -> *(!JSVal (JSMap k v), !*JSWorld)
jsPut 	 :: !k !(JSVal v) !(JSVal (JSMap k v)) 	!*JSWorld -> *JSWorld | toString k
jsDel 	 :: !k !(JSVal (JSMap k v)) 			!*JSWorld -> *JSWorld | toString k
jsGet 	 :: !k !(JSVal (JSMap k v)) 			!*JSWorld -> *(!Maybe (JSVal v), !*JSWorld) | toString k

jsToList :: !(JSVal (JSMap k v)) 				!*JSWorld -> *(![(!String, !JSVal v)], !*JSWorld)