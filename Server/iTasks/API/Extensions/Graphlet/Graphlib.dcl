definition module iTasks.API.Extensions.Graphlet.Graphlib

import iTasks.API.Core.Client.Interface

:: GLGraphW

:: GLGraph :== JSVal GLGraphW

mkGraph :: *JSWorld -> *(GLGraph, *JSWorld)

mkDigraph :: *JSWorld -> *(GLGraph, *JSWorld)

mkCGraph :: *JSWorld -> *(GLGraph, *JSWorld)

mkCDGraph :: *JSWorld -> *(GLGraph, *JSWorld)

addNode :: GLGraph (JSVal i) (JSVal m) *JSWorld -> *JSWorld

addEdge :: GLGraph (JSVal i) (JSVal l) (JSVal r) (JSVal a) *JSWorld -> *JSWorld

addEdgeSimple :: GLGraph (JSVal l) (JSVal r) *JSWorld -> *JSWorld

setEdgeValue :: GLGraph (JSVal e) (JSVal v) *JSWorld -> *JSWorld

getEdgeValue :: GLGraph (JSVal e) *JSWorld -> *(JSVal v, *JSWorld)

setNodeValue :: GLGraph (JSVal e) (JSVal v) *JSWorld -> *JSWorld

getNodeValue :: GLGraph (JSVal e) *JSWorld -> *(JSVal v, *JSWorld)

isDirected :: GLGraph *JSWorld -> *(Bool, *JSWorld)

hasNode :: GLGraph (JSVal i) *JSWorld -> *(Bool, *JSWorld)

hasEdge :: GLGraph (JSVal i) *JSWorld -> *(Bool, *JSWorld)
