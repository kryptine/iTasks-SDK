definition module iTasks.API.Extensions.DagreD3.Dagre

from iTasks.API.Core.Client.Interface import :: JSWorld, :: JSVal
from iTasks.API.Extensions.DagreD3.Graphlib import :: GLGraph, :: GLGraphW

:: DagreLW

:: DagreLayout :== JSVal DagreLW

:: DagreRankDir = TB | LR

mkLayout :: *JSWorld -> *(DagreLayout, *JSWorld)

debugLevel :: DagreLayout Int *JSWorld -> *JSWorld

nodeSep :: DagreLayout Int *JSWorld -> *JSWorld

edgeSep :: DagreLayout Int *JSWorld -> *JSWorld

rankSep :: DagreLayout Int *JSWorld -> *JSWorld

rankDir :: DagreLayout DagreRankDir *JSWorld -> *JSWorld

runLayout :: DagreLayout GLGraph *JSWorld -> *JSWorld
