definition module iTasks.API.Extensions.Graphlet.D3

from Text.HTML import :: HtmlTag
from iTasks.API.Core.Client.Interface import :: JSWorld, :: JSVal, :: JSFun, :: JSObj, :: JSFunction, :: JSObject

:: D3W

:: D3 :== JSObj D3W

selectElem :: String *JSWorld -> *(D3, *JSWorld)

selectAllElems :: String *JSWorld -> *(D3, *JSWorld)

selectChildElem :: D3 String *JSWorld -> *(D3, *JSWorld)

selectAllChildElems :: D3 String *JSWorld -> *(D3, *JSWorld)

setAttr :: String (JSVal a) D3 *JSWorld -> *(D3, *JSWorld)

setAttrs :: [(String, (JSVal a))] D3 *JSWorld -> *(D3, *JSWorld)

getAttr :: String D3 *JSWorld -> *(JSVal a, *JSWorld)

append :: String D3 *JSWorld -> *(D3, *JSWorld)

setHtml :: String D3 *JSWorld -> *(D3, *JSWorld)

setHtmlWith :: (JSFun c) D3 *JSWorld -> *(D3, *JSWorld)

setText :: String D3 *JSWorld -> *(D3, *JSWorld)

setTextWith :: (JSFun c) D3 *JSWorld -> *(D3, *JSWorld)

firstNode :: D3 *JSWorld -> *(JSVal v, *JSWorld)

appendHtml :: HtmlTag D3 *JSWorld -> *(D3, *JSWorld)

removeElems :: D3 *JSWorld -> *(D3, *JSWorld)
