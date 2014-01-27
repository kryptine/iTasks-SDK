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

:: GraphletDiff sd n e =
  { newGraph  :: Graph n e
  , stateDiff :: sd
  }

:: GraphletRenderer s n e =
  { drawNodeCallback      :: s n GLGraph NodeIndex D3 *JSWorld -> *JSWorld
  , drawEdgeLabelCallback :: s e GLGraph EdgeIndex D3 *JSWorld -> *JSWorld
  , styleSheets           :: [String]
  }

:: GraphletState s n e =
  { currGraph   :: Graph n e
  , customState :: s
  }

derive class iTask GraphletDiff, GraphletState

graphlet :: s (s s -> cd) (cd s -> s) (GraphletRenderer s n e) (Graph n e)
         -> Editlet (GraphletState s n e) (GraphletDiff cd n e) | iTask n & iTask e & iTask s & iTask cd
