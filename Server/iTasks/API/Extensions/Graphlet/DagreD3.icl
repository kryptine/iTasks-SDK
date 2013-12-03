implementation module iTasks.API.Extensions.Graphlet.DagreD3

import StdTuple
import iTasks.API.Core.Client.Interface
import iTasks.API.Extensions.Graphlet.Graphlib
import iTasks.API.Extensions.Graphlet.Dagre
import iTasks.API.Extensions.Graphlet.D3

:: DagreRenderW = DagreRenderW

:: DagreRender :== JSVal DagreRenderW

mkDigraph :: *JSWorld -> *(GLGraph, *JSWorld)
mkDigraph world = jsNewObject "dagreD3.Digraph" [] world

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
