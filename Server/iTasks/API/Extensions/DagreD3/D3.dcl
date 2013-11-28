definition module iTasks.API.Extensions.DagreD3.D3

from iTasks.API.Core.Client.Interface import :: JSWorld, :: JSVal, :: JSFunction

:: D3W

:: D3 :== JSVal D3W

select :: String *JSWorld -> *(D3, *JSWorld)

selectAll :: String *JSWorld -> *(D3, *JSWorld)

setAttr :: String a D3 *JSWorld -> *(D3, *JSWorld)

getAttr :: String D3 *JSWorld -> *(JSVal a, *JSWorld)

append :: String D3 *JSWorld -> *(D3, *JSWorld)

setHtml :: String D3 *JSWorld -> *(D3, *JSWorld)

setHtmlWith :: (JSVal (JSFunction c)) D3 *JSWorld -> *(D3, *JSWorld)

setText :: String D3 *JSWorld -> *(D3, *JSWorld)

setTextWith :: (JSVal (JSFunction c)) D3 *JSWorld -> *(D3, *JSWorld)
