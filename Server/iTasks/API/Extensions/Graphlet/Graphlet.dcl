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

:: GraphletDiff n e
  =  RemoveNodes [NodeIndex]
  |  RemoveEdges [EdgeIndex]
  |  AddNodes    [(n, NodeIndex)]
  |  AddEdges    [(e, EdgeIndex)]
  |  UpdateNodes [(n, NodeIndex)]

:: GraphletRenderer n e =
  { drawNodeCallback      :: n GLGraph NodeIndex D3 *JSWorld -> *JSWorld
  , drawEdgeLabelCallback :: e GLGraph EdgeIndex D3 *JSWorld -> *JSWorld
  , styleSheets           :: [String]
  }

:: Graphlet n e =
  { graph       :: Graph n e
  }

:: GraphletClientData n e =
  { mbClientState :: Maybe GraphletClientState
  , graphlet      :: Graphlet n e
  }

:: GraphletClientState =
  { graphObj  :: GLGraph
  , svgTarget :: D3
  }

derive class iTask GraphletDiff, Graphlet

graphlet :: (GraphletRenderer n e) (Graphlet n e)
         -> Editlet (Graphlet n e) [GraphletDiff n e] | iTask n & iTask e
