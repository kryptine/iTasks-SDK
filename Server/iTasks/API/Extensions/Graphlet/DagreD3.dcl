definition module iTasks.API.Extensions.Graphlet.DagreD3

from iTasks.API.Core.Client.Interface import :: JSWorld, :: JSVal, :: JSFunction, :: JSObj, :: JSObject, :: JSFun
from iTasks.API.Core.Client.Editlet import :: EditletEventHandlerFunc, :: ComponentEventHandlerFunc, :: ComponentId, :: JSEvent
from iTasks.API.Extensions.Graphlet.Graphlib import :: GLGraph, :: GLGraphW, addNode, addEdge, addEdgeSimple, getNodeValue, setNodeValue, getEdgeValue, setNodeValue, isDirected, hasNode, hasEdge
from iTasks.API.Extensions.Graphlet.Dagre import :: DagreLayout, :: DagreLW
from iTasks.API.Extensions.Graphlet.D3 import :: D3, :: D3W

:: DagreRenderW

:: DagreRender :== JSObj DagreRenderW

mkDagreD3Digraph :: *JSWorld -> *(GLGraph, *JSWorld)

mkRenderer :: *JSWorld -> *(DagreRender, *JSWorld)

setLayout :: DagreRender DagreLayout *JSWorld -> *JSWorld

runRenderer :: DagreRender GLGraph D3 *JSWorld -> *JSWorld

setDrawNode :: DagreRender (JSFun a) *JSWorld -> *JSWorld

setDrawEdgeLabel :: DagreRender (JSFun a) *JSWorld -> *JSWorld
