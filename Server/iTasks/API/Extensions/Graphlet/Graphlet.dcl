definition module iTasks.API.Extensions.Graphlet.Graphlet

import iTasks
import iTasks.API.Core.Client.Editlet
from Data.Graph import :: Graph, :: Node
from iTasks.API.Extensions.Graphlet.D3 import :: D3, :: D3W
from iTasks.API.Extensions.Graphlet.Graphlib import :: GLGraph, :: GLGraphW

derive gEditor Graph, Node
derive gEditMeta Graph, Node
derive gVisualizeText Graph, Node
derive gDefault Graph, Node
derive gUpdate Graph, Node
derive gVerify Graph, Node

:: GraphletDiff a = GraphletDiff a

derive class iTask GraphletDiff

:: GraphletRenderer n e =
  { drawNodeCallback      :: n GLGraph Int D3 *JSWorld -> *JSWorld
  , drawEdgeLabelCallback :: e GLGraph (Int, Int) D3 *JSWorld -> *JSWorld
  , styleSheets           :: [String]
  }

:: GraphletState n e =
  { graph         :: Graph n e
  , activeNodeIdx :: Int
  }

graphlet :: (Graph n e) (GraphletRenderer n e) -> Editlet (Graph n e) (GraphletDiff (Graph n e)) | iTask n & iTask e
