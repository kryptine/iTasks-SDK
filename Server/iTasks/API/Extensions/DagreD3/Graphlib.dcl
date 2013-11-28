definition module iTasks.API.Extensions.DagreD3.Graphlib

import iTasks.API.Core.Client.Interface

:: GLGraphW

:: GLGraph :== JSVal GLGraphW

mkGraph :: *JSWorld -> *(GLGraph, *JSWorld)

mkDigraph :: *JSWorld -> *(GLGraph, *JSWorld)

mkCGraph :: *JSWorld -> *(GLGraph, *JSWorld)

mkCDGraph :: *JSWorld -> *(GLGraph, *JSWorld)

addNode :: GLGraph (JSVal i) (JSVal m) *JSWorld -> *JSWorld

addEdge :: GLGraph (JSVal i) (JSVal l) (JSVal r) (JSVal a) *JSWorld -> *JSWorld

isDirected :: GLGraph *JSWorld -> *(Bool, *JSWorld)

hasNode :: GLGraph (JSVal i) *JSWorld -> *(Bool, *JSWorld)

hasEdge :: GLGraph (JSVal i) *JSWorld -> *(Bool, *JSWorld)
