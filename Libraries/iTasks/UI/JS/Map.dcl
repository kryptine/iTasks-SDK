definition module iTasks.UI.JS.Map

import iTasks.UI.JS.Interface

:: JSMap k v :== JSVal

jsNewMap ::                            !*JSWorld -> *(!JSMap k JSVal, !*JSWorld) | toString k
jsPut    :: !k !JSVal !(JSMap k JSVal) !*JSWorld -> *JSWorld | toString k
jsDel    :: !k !(JSMap k JSVal)        !*JSWorld -> *JSWorld | toString k
jsGet    :: !k !(JSMap k JSVal)        !*JSWorld -> *(!Maybe JSVal, !*JSWorld) | toString k

jsToList :: !(JSMap k JSVal)           !*JSWorld -> *(![(!String, !JSVal)], !*JSWorld) | toString k
