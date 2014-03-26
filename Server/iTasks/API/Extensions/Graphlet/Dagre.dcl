definition module iTasks.API.Extensions.Graphlet.Dagre

from iTasks.API.Core.Client.Interface import :: JSWorld, :: JSVal, :: JSObj, :: JSObject
from iTasks.API.Extensions.Graphlet.Graphlib import :: GLGraph, :: GLGraphW

:: DagreLW

:: DagreLayout :== JSObj DagreLW

:: DagreRankDir = TB | LR

mkLayout :: *JSWorld -> *(DagreLayout, *JSWorld)

debugLevel :: DagreLayout Int *JSWorld -> *JSWorld

nodeSep :: DagreLayout Int *JSWorld -> *JSWorld

edgeSep :: DagreLayout Int *JSWorld -> *JSWorld

rankSep :: DagreLayout Int *JSWorld -> *JSWorld

rankDir :: DagreLayout DagreRankDir *JSWorld -> *JSWorld

runLayout :: DagreLayout GLGraph *JSWorld -> *JSWorld
