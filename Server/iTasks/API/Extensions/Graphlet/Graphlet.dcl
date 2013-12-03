definition module iTasks.API.Extensions.Graphlet.Graphlet

import iTasks
import iTasks.API.Core.Client.Editlet
from Data.Graph import :: Graph, :: Node
from iTasks.API.Extensions.Graphlet.D3 import :: D3, :: D3W
from iTasks.API.Extensions.Graphlet.Graphlib import :: GLGraph, :: GLGraphW
from Text.HTML import :: HtmlTag

derive gEditor Graph, Node
derive gEditMeta Graph, Node
derive gVisualizeText Graph, Node
derive gDefault Graph, Node
derive gUpdate Graph, Node
derive gVerify Graph, Node

:: GraphletRenderer n e =
  { drawNodeCallback      :: n GLGraph String D3 *JSWorld -> *JSWorld
  , drawEdgeLabelCallback :: e GLGraph String D3 *JSWorld -> *JSWorld
  , styleSheets           :: [String]
  }

graphlet :: (Graph n e) (GraphletRenderer n e) -> Editlet (Graph n e) (Graph n e) | iTask n & iTask e

appendHtml :: HtmlTag D3 *JSWorld -> *(D3, *JSWorld)
