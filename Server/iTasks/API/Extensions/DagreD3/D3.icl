implementation module iTasks.API.Extensions.DagreD3.D3

import iTasks.API.Core.Client.Interface

:: D3W = D3W

:: D3 :== JSVal D3W

select :: String *JSWorld -> *(D3, *JSWorld)
select sel world = callFunction "d3.select" [toJSArg sel] world

selectAll :: String *JSWorld -> *(D3, *JSWorld)
selectAll sel world = callFunction "d3.selectAll" [toJSArg sel] world

setAttr :: String a D3 *JSWorld -> *(D3, *JSWorld)
setAttr attr val d3 world = callObjectMethod "attr" [toJSArg attr, toJSArg val] d3 world

getAttr :: String D3 *JSWorld -> *(JSVal a, *JSWorld)
getAttr attr d3 world = callObjectMethod "attr" [toJSArg attr] d3 world

append :: String D3 *JSWorld -> *(D3, *JSWorld)
append attr d3 world = callObjectMethod "append" [toJSArg attr] d3 world

setHtml :: String D3 *JSWorld -> *(D3, *JSWorld)
setHtml str d3 world = callObjectMethod "html" [toJSArg str] d3 world

setHtmlWith :: (JSVal (JSFunction c)) D3 *JSWorld -> *(D3, *JSWorld)
setHtmlWith f d3 world = callObjectMethod "html" [toJSArg f] d3 world

setText :: String D3 *JSWorld -> *(D3, *JSWorld)
setText str d3 world = callObjectMethod "text" [toJSArg str] d3 world

setTextWith :: (JSVal (JSFunction c)) D3 *JSWorld -> *(D3, *JSWorld)
setTextWith f d3 world = callObjectMethod "text" [toJSArg f] d3 world
