definition module iTasks.API.Extensions.Graphlet.DagreD3

from iTasks.API.Core.Client.Interface import :: JSWorld, :: JSVal, :: JSFunction
from iTasks.API.Core.Client.Editlet import :: EditletEventHandlerFunc, :: ComponentEventHandlerFunc, :: ComponentId, :: JSEvent
from iTasks.API.Extensions.Graphlet.Graphlib import :: GLGraph, :: GLGraphW, addNode, addEdge, addEdgeSimple, getNodeValue, setNodeValue, getEdgeValue, setNodeValue, isDirected, hasNode, hasEdge
from iTasks.API.Extensions.Graphlet.Dagre import :: DagreLayout, :: DagreLW
from iTasks.API.Extensions.Graphlet.D3 import :: D3, :: D3W

:: DagreRenderW

:: DagreRender :== JSVal DagreRenderW

mkDigraph :: *JSWorld -> *(GLGraph, *JSWorld)

mkRenderer :: *JSWorld -> *(DagreRender, *JSWorld)

setLayout :: DagreRender DagreLayout *JSWorld -> *JSWorld

runRenderer :: DagreRender GLGraph D3 *JSWorld -> *JSWorld

setDrawNode :: DagreRender (JSVal (JSFunction a)) *JSWorld -> *JSWorld

setDrawEdgeLabel :: DagreRender (JSVal (JSFunction a)) *JSWorld -> *JSWorld
