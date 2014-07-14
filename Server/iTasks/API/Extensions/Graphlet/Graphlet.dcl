definition module iTasks.API.Extensions.Graphlet.Graphlet

import iTasks
import iTasks.API.Core.Client.Editlet
from Data.Graph import :: Graph, :: Node, :: NodeIndex, :: EdgeIndex
from iTasks.API.Extensions.Graphlet.D3 import :: D3, :: D3W
from iTasks.API.Extensions.Graphlet.Graphlib import :: GLGraph, :: GLGraphW

derive gEditor Graph, Node
derive gEditMeta Graph, Node
derive gText Graph, Node
derive gDefault Graph, Node
derive gUpdate Graph, Node
derive gVerify Graph, Node

:: GraphletDiff s n e
  =  RemoveNodes [NodeIndex]
  |  RemoveEdges [EdgeIndex]
  |  AddNodes    [(n, NodeIndex)]
  |  AddEdges    [(e, EdgeIndex)]
  |  UpdateNodes [(n, NodeIndex)]
  |  SetState    s

:: GraphletRenderer s n e =
  { drawNodeCallback      :: s n GLGraph NodeIndex D3 *JSWorld -> *JSWorld
  , drawEdgeLabelCallback :: s e GLGraph EdgeIndex D3 *JSWorld -> *JSWorld
  , styleSheets           :: [String]
  }

:: Graphlet s n e =
  { graph       :: Graph n e
  , customState :: s
  }

:: GraphletClientData s n e =
  { mbClientState :: Maybe GraphletClientState
  , graphlet      :: Graphlet s n e
  }

:: GraphletClientState =
  { graphObj  :: GLGraph
  , svgTarget :: D3
  }

derive class iTask GraphletDiff, Graphlet

graphlet :: (GraphletRenderer s n e) (Graphlet s n e)
         -> Editlet (Graphlet s n e) [GraphletDiff s n e] | iTask s & iTask n & iTask e
