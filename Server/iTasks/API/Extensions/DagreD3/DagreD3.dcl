definition module iTasks.API.Extensions.DagreD3.DagreD3

from iTasks.API.Core.Client.Interface import :: JSWorld, :: JSVal
from iTasks.API.Extensions.DagreD3.Graphlib import :: GLGraph, :: GLGraphW
from iTasks.API.Extensions.DagreD3.Dagre import :: DagreLayout, :: DagreLW
from iTasks.API.Extensions.DagreD3.D3 import :: D3, :: D3W

:: DagreRenderW

:: DagreRender :== JSVal DagreRenderW

mkRenderer :: *JSWorld -> *(DagreRender, *JSWorld)

setLayout :: DagreRender DagreLayout *JSWorld -> *JSWorld

runRenderer :: DagreRender GLGraph D3 *JSWorld -> *JSWorld
