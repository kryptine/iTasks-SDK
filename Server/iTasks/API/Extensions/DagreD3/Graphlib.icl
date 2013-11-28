implementation module iTasks.API.Extensions.DagreD3.Graphlib

import StdTuple
import iTasks.API.Core.Client.Interface

:: GLGraphW = GLGraphW

:: GLGraph :== JSVal GLGraphW

mkGraph :: *JSWorld -> *(GLGraph, *JSWorld)
mkGraph world = jsNewObject "Graph" [] world

mkDigraph :: *JSWorld -> *(GLGraph, *JSWorld)
mkDigraph world = jsNewObject "Digraph" [] world

mkCGraph :: *JSWorld -> *(GLGraph, *JSWorld)
mkCGraph world = jsNewObject "CGraph" [] world

mkCDGraph :: *JSWorld -> *(GLGraph, *JSWorld)
mkCDGraph world = jsNewObject "CDGraph" [] world

addNode :: GLGraph (JSVal i) (JSVal m) *JSWorld -> *JSWorld
addNode graph nid meta world = snd (callObjectMethod "addNode" [toJSArg nid, toJSArg meta] graph world)

addEdge :: GLGraph (JSVal i) (JSVal l) (JSVal r) (JSVal a) *JSWorld -> *JSWorld
addEdge graph eid lid rid attrs world = snd (callObjectMethod "addEdge" [toJSArg eid, toJSArg lid, toJSArg rid, toJSArg attrs] graph world)

isDirected :: GLGraph *JSWorld -> *(Bool, *JSWorld)
isDirected graph world
  # (jb, world) = callObjectMethod "isDirected" [] graph world
  = (jsValToBool jb, world)

hasNode :: GLGraph (JSVal i) *JSWorld -> *(Bool, *JSWorld)
hasNode graph ev world
  # (jb, world) = callObjectMethod "hasNode" [] graph world
  = (jsValToBool jb, world)

hasEdge :: GLGraph (JSVal i) *JSWorld -> *(Bool, *JSWorld)
hasEdge graph ev world
  # (jb, world) = callObjectMethod "hasEdge" [] graph world
  = (jsValToBool jb, world)
