implementation module iTasks.API.Extensions.Graphlet.Dagre

import StdTuple
import iTasks.API.Core.Client.Interface
import iTasks.API.Extensions.Graphlet.Graphlib

:: DagreLW = DagreLW

:: DagreLayout :== JSVal DagreLW

:: DagreRankDir = TB | LR

mkLayout :: *JSWorld -> *(DagreLayout, *JSWorld)
mkLayout world = callFunction "dagre.layout" [] world

debugLevel :: DagreLayout Int *JSWorld -> *JSWorld
debugLevel layout n world = snd (callObjectMethod "debugLevel" [toJSArg n] layout world)

nodeSep :: DagreLayout Int *JSWorld -> *JSWorld
nodeSep layout n world = snd (callObjectMethod "nodeSep" [toJSArg n] layout world)

edgeSep :: DagreLayout Int *JSWorld -> *JSWorld
edgeSep layout n world = snd (callObjectMethod "edgeSep" [toJSArg n] layout world)

rankSep :: DagreLayout Int *JSWorld -> *JSWorld
rankSep layout n world = snd (callObjectMethod "rankSep" [toJSArg n] layout world)

rankDir :: DagreLayout DagreRankDir *JSWorld -> *JSWorld
rankDir layout dir world = snd (callObjectMethod "rankDir" [toJSArg (d2s dir)] layout world)
  where d2s TB = "TB"
        d2s LR = "LR"

runLayout :: DagreLayout GLGraph *JSWorld -> *JSWorld
runLayout layout graph world = snd (callObjectMethod "run" [toJSArg graph] layout world)
