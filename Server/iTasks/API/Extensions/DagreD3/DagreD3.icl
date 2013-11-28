implementation module iTasks.API.Extensions.DagreD3.DagreD3

import StdTuple
import iTasks.API.Core.Client.Interface
import iTasks.API.Extensions.DagreD3.Graphlib
import iTasks.API.Extensions.DagreD3.Dagre
import iTasks.API.Extensions.DagreD3.D3

:: DagreRenderW = DagreRenderW

:: DagreRender :== JSVal DagreRenderW

mkRenderer :: *JSWorld -> *(DagreRender, *JSWorld)
mkRenderer world = jsNewObject "dagreD3.Renderer" [] world

setLayout :: DagreRender DagreLayout *JSWorld -> *JSWorld
setLayout renderer layout world = snd (callObjectMethod "layout" [toJSArg layout] renderer world)

runRenderer :: DagreRender GLGraph D3 *JSWorld -> *JSWorld
runRenderer renderer graph d3 world = snd (callObjectMethod "run" [toJSArg graph, toJSArg d3] renderer world)

setDrawNode :: DagreRender (JSVal (JSFunction a)) *JSWorld -> *JSWorld
setDrawNode renderer cb world = snd (callObjectMethod "drawNode" [toJSArg cb] renderer world)

setDrawEdgeLabel :: DagreRender (JSVal (JSFunction a)) *JSWorld -> *JSWorld
setDrawEdgeLabel renderer cb world = snd (callObjectMethod "drawEdgeLabel" [toJSArg cb] renderer world)
