implementation module iTasks.API.Extensions.Graphlet.Graphlet

import iTasks
import iTasks.API.Extensions.Graphlet.D3
import iTasks.API.Extensions.Graphlet.DagreD3
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import StdMisc
import StdDebug
import StdArray
from Data.Graph import :: Graph, :: Node
import qualified Data.Graph as DG
import dynamic_string

derive gEditor Graph, Node
derive gEditMeta Graph, Node
derive gVisualizeText Graph, Node
derive gDefault Graph, Node
derive gUpdate Graph, Node
derive gVerify Graph, Node

derive class iTask GraphletDiff, GraphletState, GraphletLoc

mkSVGId :: String -> String
mkSVGId x = "svg" +++ x

graphlet :: (Graph n e) (GraphletRenderer n e) -> Editlet (GraphletState n e) (GraphletDiff n e) | iTask n & iTask e
graphlet graph renderer
  = toEditlet simpl
  where
  simpl = EditletSimpl {currGraph = graph, currLoc = NodeLoc 0}
            { EditletSimplDef
            | genUI    = \cid world -> (uiDef cid, world)
            , updateUI = onUpdate
            , genDiff  = genDiff
            , appDiff  = appDiff
            }

  uiDef cid
    = { html          = SvgTag [IdAttr (mkSVGId cid), ClassAttr "graphletGraph"]
                               [GTag [TransformAttr "translate(20, 20)"] []]
      , eventHandlers = []
      , width         = WrapSize
      , height        = WrapSize
      }

  onUpdate :: ComponentId (Maybe (GraphletDiff n e)) (GraphletState n e) *JSWorld -> *(GraphletState n e, *JSWorld) | iTask n & iTask e
  onUpdate cid _ st world
    # (dagre, world) = findObject "dagreD3" world
    | jsIsUndefined dagre
        # world = foldr addCSSFromUrl world renderer.styleSheets
        # world = addJSFromUrl "/d3.v3.min.js" Nothing world
        # world = addJSFromUrl "/dagre.js" Nothing world
        # world = addJSFromUrl "/dagre-d3.js" (Just (createEditletEventHandler onLibLoaded cid)) world
        = (st, world)
    | otherwise
        = onLibLoaded cid undef st world

  onLibLoaded :: ComponentId {JSVal JSEvent} (GraphletState n e) *JSWorld -> *(GraphletState n e, *JSWorld) // | iTask n & iTask e
  onLibLoaded cid _ st=:{currGraph, currLoc} world
    # (graph, world)   = mkDigraph world
    # world            = addNodesEdges currGraph graph world
    # (drend, world)   = mkRenderer world
    # renderNodeFun    = createEditletEventHandler (drawNodeCb currLoc) cid
    # renderEdgeLblFun = createEditletEventHandler (drawEdgeLabelCb currLoc) cid
    # world            = setDrawNode drend renderNodeFun world
    # world            = setDrawEdgeLabel drend renderEdgeLblFun world
    # (svgg, world)    = selectElem ("#" +++ mkSVGId cid) world
    # world            = runRenderer drend graph svgg world
    = (st, world)

  drawNodeCb :: GraphletLoc ComponentId {JSVal JSEvent} (Graph n e) *JSWorld -> *(Graph n e, *JSWorld) // | iTask n & iTask e
  drawNodeCb currLoc cid {[0] = jsgraph, [1] = u, [2] = root} cgraph world
    # graphValue = jsUnsafeCoerce jsgraph
    # nodeId     = jsValToInt (jsUnsafeCoerce u)
    # rootElem   = jsUnsafeCoerce root
    # world      = case 'DG'.getNodeData nodeId graph of
                     Just nodeVal -> renderer.drawNodeCallback currLoc nodeVal graphValue nodeId rootElem world
                     _            -> world
    = (cgraph, world)

  drawEdgeLabelCb :: GraphletLoc ComponentId {JSVal JSEvent} (Graph n e) *JSWorld -> *(Graph n e, *JSWorld) // | iTask n & iTask e
  drawEdgeLabelCb currLoc cid {[0] = jsgraph, [1] = e, [2] = root} cgraph world
    # graphValue    = jsUnsafeCoerce jsgraph
    # edgeIdLst     = jsUnsafeCoerce e // We really need the #! here, because jsGetObjectEl will try to get elements from a thunk otherwise
    # (fEId, world) = jsGetObjectEl 0 edgeIdLst world
    # (tEId, world) = jsGetObjectEl 1 edgeIdLst world
    # edgeId        = (jsValToInt fEId, jsValToInt tEId)
    # rootElem      = jsUnsafeCoerce root
    # world         = case 'DG'.getEdgeData edgeId graph of
                        Just edgeVal -> renderer.drawEdgeLabelCallback currLoc edgeVal graphValue edgeId rootElem world
                        _            -> world
    = (cgraph, world)

  genDiff _ {currGraph = cgr, currLoc = clr} = Just {newGraph = cgr, newLoc = clr} // TODO Better diffing between graphs
  appDiff {newGraph, newLoc} _               = {currGraph = newGraph, currLoc = newLoc}

addNodesEdges :: (Graph n e) GLGraph *JSWorld -> *JSWorld // | iTask n & iTask e
addNodesEdges g jsgraph world
  # (_, world) = 'DG'.foldrNodes addNode` (jsgraph, world) g
  # (_, world) = 'DG'.foldrEdges addEdge` (jsgraph, world) g
  = world
  where
  addNode` :: Int n *(GLGraph, *JSWorld) -> *(GLGraph, *JSWorld) // | iTask n
  addNode` ni node (jsgraph, world)
    # (obj, world) = jsEmptyObject world
    # world        = jsSetObjectAttr "node" (toJSVal ni) obj world
    # world        = addNode jsgraph (toJSVal ni) (toJSVal obj) world
    = (jsgraph, world)

  addEdge` :: (Int, Int) e *(GLGraph, *JSWorld) -> *(GLGraph, *JSWorld) // | iTask e
  addEdge` (fromNode, toNode) edge (jsgraph, world)
    # (obj, world) = jsEmptyObject world
    # world        = addEdge jsgraph (toJSVal [fromNode, toNode]) (toJSVal fromNode) (toJSVal toNode) obj world
    = (jsgraph, world)
