implementation module iTasks.API.Extensions.Graphlet.D3

import iTasks.API.Core.Client.Interface
import Data.List, Text.HTML

:: D3W = D3W

:: D3 :== JSVal D3W

selectElem :: String *JSWorld -> *(D3, *JSWorld)
selectElem sel world = callFunction "d3.select" [toJSArg sel] world

selectAllElems :: String *JSWorld -> *(D3, *JSWorld)
selectAllElems sel world = callFunction "d3.selectAll" [toJSArg sel] world

selectChildElem :: D3 String *JSWorld -> *(D3, *JSWorld)
selectChildElem d3 sel world = callObjectMethod "select" [toJSArg sel] d3 world

selectAllChildElems :: D3 String *JSWorld -> *(D3, *JSWorld)
selectAllChildElems d3 sel world = callObjectMethod "selectAll" [toJSArg sel] d3 world

setAttr :: String (JSVal a) D3 *JSWorld -> *(D3, *JSWorld)
setAttr attr val d3 world = callObjectMethod "attr" [toJSArg attr, toJSArg val] d3 world

setAttrs :: [(String, (JSVal a))] D3 *JSWorld -> *(D3, *JSWorld)
setAttrs xs d3 world = foldr (\(attr, val) (d3, world) -> setAttr attr val d3 world) (d3, world) xs

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

firstNode :: D3 *JSWorld -> *(JSVal v, *JSWorld)
firstNode d3 world = callObjectMethod "node" [] d3 world

appendHtml :: HtmlTag D3 *JSWorld -> *(D3, *JSWorld)
appendHtml html root world
  # world = jsTrace (toString html) world
  # (g, world) = append "g" root world
  = setHtml (toString html) g world
