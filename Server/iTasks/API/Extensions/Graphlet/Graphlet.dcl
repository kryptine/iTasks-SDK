definition module iTasks.API.Extensions.Graphlet.Graphlet

import iTasks
import iTasks.API.Core.Client.Editlet
from Data.Graph import :: Graph, :: Node, :: NodeIndex, :: EdgeIndex
from iTasks.API.Extensions.Graphlet.D3 import :: D3, :: D3W
from iTasks.API.Extensions.Graphlet.Graphlib import :: GLGraph, :: GLGraphW

derive gEditor Graph, Node
derive gEditMeta Graph, Node
derive gVisualizeText Graph, Node
derive gDefault Graph, Node
derive gUpdate Graph, Node
derive gVerify Graph, Node

:: GraphletDiff n e =
  { newGraph :: Graph n e
  , newLoc   :: GraphletLoc
  }

:: GraphletRenderer n e =
  { drawNodeCallback      :: GraphletLoc n GLGraph NodeIndex D3 *JSWorld -> *JSWorld
  , drawEdgeLabelCallback :: GraphletLoc e GLGraph EdgeIndex D3 *JSWorld -> *JSWorld
  , styleSheets           :: [String]
  }

:: GraphletState n e =
  { currGraph :: Graph n e
  , currLoc   :: GraphletLoc
  }

:: GraphletLoc
  =  NodeLoc NodeIndex
  |  EdgeLoc EdgeIndex Int

derive class iTask GraphletDiff, GraphletState, GraphletLoc

graphlet :: (Graph n e) (GraphletRenderer n e) -> Editlet (GraphletState n e) (GraphletDiff n e) | iTask n & iTask e
