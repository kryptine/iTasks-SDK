definition module iTasks.API.Extensions.Tonic.Toniclet

import iTasks
import iTasks.API.Core.Client.Editlet
from Data.Graph import :: Graph, :: Node, :: NodeIndex, :: EdgeIndex
from iTasks.API.Extensions.Graphlet.D3 import :: D3, :: D3W
from iTasks.API.Extensions.Graphlet.Graphlib import :: GLGraph, :: GLGraphW
from iTasks.Framework.Tonic.AbsSyn import :: GNode, :: GEdge, :: TonicTask

derive gEditor Graph, Node
derive gEditMeta Graph, Node
derive gDefault Graph, Node
derive gUpdate Graph, Node
derive gVerify Graph, Node
derive gText Graph, Node

:: TonicletDiff
  =  RemoveNodes    [NodeIndex]
  |  RemoveEdges    [EdgeIndex]
  |  AddNodes       [(GNode, NodeIndex)]
  |  AddEdges       [(GEdge, EdgeIndex)]
  |  UpdateNodes    [(GNode, NodeIndex)]

:: TonicletRenderer =
  { drawNodeCallback      :: Bool GNode GLGraph NodeIndex D3 *JSWorld -> *JSWorld
  , drawEdgeLabelCallback :: GEdge GLGraph EdgeIndex D3 *JSWorld -> *JSWorld
  , styleSheets           :: [String]
  }

:: TonicletClientData =
  { mbClientState :: Maybe TonicletClientState
  , tonicTask     :: Maybe TonicTask
  }

:: TonicletClientState =
  { graphObj  :: GLGraph
  , svgTarget :: D3
  }

derive class iTask TonicletDiff

toniclet :: TonicletRenderer (Maybe TonicTask) (Maybe Int) -> Editlet (Maybe TonicTask) [TonicletDiff]
