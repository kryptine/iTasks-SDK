implementation module iTasks.API.Extensions.Graphlet.Graphlib

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

delNode :: GLGraph (JSVal i) *JSWorld -> *JSWorld
delNode graph nid world = snd (callObjectMethod "delNode" [toJSArg nid] graph world)

addEdge :: GLGraph (JSVal i) (JSVal l) (JSVal r) (JSVal a) *JSWorld -> *JSWorld
addEdge graph eid lid rid attrs world = snd (callObjectMethod "addEdge" [toJSArg eid, toJSArg lid, toJSArg rid, toJSArg attrs] graph world)

delEdge :: GLGraph (JSVal i) *JSWorld -> *JSWorld
delEdge graph eid world = snd (callObjectMethod "delEdge" [toJSArg eid] graph world)

addEdgeSimple :: GLGraph (JSVal l) (JSVal r) *JSWorld -> *JSWorld
addEdgeSimple graph lid rid world = addEdge graph jsNull lid rid jsNull world

setEdgeValue :: GLGraph (JSVal e) (JSVal v) *JSWorld -> *JSWorld
setEdgeValue graph edge value world = snd (callObjectMethod "edge" [toJSArg edge, toJSArg value] graph world)

getEdgeValue :: GLGraph (JSVal e) *JSWorld -> *(JSVal v, *JSWorld)
getEdgeValue graph edge world = callObjectMethod "edge" [toJSArg edge] graph world

setNodeValue :: GLGraph (JSVal e) (JSVal v) *JSWorld -> *JSWorld
setNodeValue graph edge value world = snd (callObjectMethod "node" [toJSArg edge, toJSArg value] graph world)

getNodeValue :: GLGraph (JSVal e) *JSWorld -> *(JSVal v, *JSWorld)
getNodeValue graph node world = callObjectMethod "node" [toJSArg node] graph world

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

eachNode :: GLGraph (JSVal (JSFunction f)) *JSWorld -> *JSWorld
eachNode graph jsf world = snd (callObjectMethod "eachNode" [toJSArg jsf] graph world)
