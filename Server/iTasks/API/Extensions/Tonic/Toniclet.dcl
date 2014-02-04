definition module iTasks.API.Extensions.Tonic.Toniclet

import iTasks
import iTasks.API.Core.Client.Editlet
from Data.Graph import :: Graph, :: Node, :: NodeIndex, :: EdgeIndex
from iTasks.API.Extensions.Graphlet.D3 import :: D3, :: D3W
from iTasks.API.Extensions.Graphlet.Graphlib import :: GLGraph, :: GLGraphW
from iTasks.API.Extensions.Tonic.TonicRenderer import :: TonicState
from iTasks.Framework.Tonic import :: TonicTrace

derive gEditor Graph, Node
derive gEditMeta Graph, Node
derive gVisualizeText Graph, Node
derive gDefault Graph, Node
derive gUpdate Graph, Node
derive gVerify Graph, Node

:: GraphletDiff n e
  =  RemoveNodes [NodeIndex]
  |  RemoveEdges [EdgeIndex]
  |  AddNodes    [(n, NodeIndex)]
  |  AddEdges    [(e, EdgeIndex)]
  |  UpdateNodes [(n, NodeIndex)]
  |  AddTraces   [TonicTrace]

:: GraphletRenderer n e =
  { drawNodeCallback      :: TonicState n GLGraph NodeIndex D3 *JSWorld -> *JSWorld
  , drawEdgeLabelCallback :: TonicState e GLGraph EdgeIndex D3 *JSWorld -> *JSWorld
  , styleSheets           :: [String]
  }

:: Graphlet n e =
  { graph      :: Graph n e
  , tonicState :: TonicState
  }

:: GraphletClientData n e =
  { mbClientState :: Maybe GraphletClientState
  , graphlet      :: Graphlet n e
  }

:: GraphletClientState =
  { graphObj  :: GLGraph
  , svgTarget :: D3
  //, activeNodeIds :: [(User, [NodeIndex])]
  }

derive class iTask GraphletDiff, Graphlet

//graphlet :: s (s s -> cd) (cd s -> s) (GraphletRenderer s n e) (Graph n e)
         //-> Editlet (GraphletState s n e) (GraphletDiff cd n e) | iTask n & iTask e & iTask s & iTask cd
graphlet :: (GraphletRenderer n e) (Graphlet n e)
         -> Editlet (Graphlet n e) [GraphletDiff n e] | iTask n & iTask e
