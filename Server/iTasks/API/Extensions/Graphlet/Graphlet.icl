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
import Text.HTML
import dynamic_string

derive gEditor Graph, Node
derive gEditMeta Graph, Node
derive gVisualizeText Graph, Node
derive gDefault Graph, Node
derive gUpdate Graph, Node
derive gVerify Graph, Node

mkSVGId :: String -> String
mkSVGId x = "svg" +++ x

graphlet :: (Graph n e) (GraphletRenderer n e) -> Editlet (Graph n e) (Graph n e) | iTask n & iTask e
graphlet gg renderer
  = toEditlet simpl
  where
  simpl = EditletSimpl gg
            { EditletSimplDef
            | genUI    = \cid world -> (uiDef cid, world)
            , updateUI = onUpdate
            , genDiff  = genDiff
            , appDiff  = appDiff
            }

  uiDef cid
    = { html          = SvgTag [IdAttr (mkSVGId cid), ClassAttr "graphletGraph", WidthAttr "800px", HeightAttr "600px"] // TODO Dynamic resizing
                               [GTag [TransformAttr "translate(20, 20)"] []]
      , eventHandlers = []
      , width         = ExactSize 800 // TODO Dynamic resizing
      , height        = ExactSize 600
      }

  loadLibs pid world
    # world = foldr addCSSFromUrl world renderer.styleSheets
    # world = addJSFromUrl "/d3.v3.min.js" Nothing world
    # world = addJSFromUrl "/dagre.js" Nothing world
    # world = addJSFromUrl "/dagre-d3.js" (Just (createEditletEventHandler onLibLoaded pid)) world
    = world

  onLibLoaded pid evt val world
    # (graph, world)    = mkDigraph world
    # world             = addNodesEdges val graph world
    # (renderer, world) = mkRenderer world
    # renderNodeFun     = createEditletEventHandler drawNodeCb pid
    # renderEdgeLblFun  = createEditletEventHandler drawEdgeLabelCb pid
    # world             = setDrawNode renderer renderNodeFun world
    # world             = setDrawEdgeLabel renderer renderEdgeLblFun world
    # (svgg, world)     = selectElem ("#" +++ mkSVGId pid) world
    # world             = runRenderer renderer graph svgg world
    = (val, world)

  drawNodeCb pid {[0] = graph, [1] = u, [2] = root} gg world
    # graphValue    = jsUnsafeCoerce graph
    # nodeId        = jsValToInt (jsUnsafeCoerce u)
    # rootElem      = jsUnsafeCoerce root
    # (node, world) = getNodeValue graphValue (toJSVal nodeId) world
    # (str, world)  = jsGetObjectAttr "node" node world
    # (nodeVal, _)  = copy_from_string {c \\ c <-: jsValToString str}
    # world         = renderer.drawNodeCallback nodeVal graphValue nodeId rootElem world
    = (gg, world)

  drawEdgeLabelCb pid {[0] = graph, [1] = e, [2] = root} gg world
    # graphValue    = jsUnsafeCoerce graph
    # edgeId        = jsValToInt (jsUnsafeCoerce e)
    # rootElem      = jsUnsafeCoerce root
    # (edge, world) = getEdgeValue graphValue (toJSVal edgeId) world
    # (str, world)  = jsGetObjectAttr "edge" edge world
    # (edgeVal, _)  = copy_from_string {c \\ c <-: jsValToString str}
    # world         = renderer.drawEdgeCallback edgeVal graphValue edgeId rootElem world
    = (gg, world)

  onUpdate pid _ val world
    # (joint, world) = findObject "dagreD3" world
    | jsIsUndefined joint
        # world = loadLibs pid world
        = (val, world)
    | otherwise
        = onLibLoaded pid Nothing val world

  genDiff _ g = Just g // TODO Better diffing
  appDiff g _ = g

addNodesEdges g jsgraph world
  # (_, world) = 'DG'.foldrNodes addNode` (jsgraph, world) g
  # (_, world) = 'DG'.foldrEdges addEdge` (jsgraph, world) g
  = world
  where
  addNode` ni node (jsgraph, world)
    # (obj, world) = jsEmptyObject world
    # world        = jsSetObjectAttr "node" (toJSVal (copy_to_string node)) obj world
    # world        = addNode jsgraph (toJSVal ni) (toJSVal obj) world
    = (jsgraph, world)
  addEdge` (fromNode, toNode) edge (jsgraph, world)
    # (obj, world) = jsEmptyObject world
    # world        = jsSetObjectAttr "edge" (toJSVal (copy_to_string edge)) obj world
    # world        = addEdge jsgraph jsNull (toJSVal fromNode) (toJSVal toNode) obj world
    = (jsgraph, world)

appendHtml :: HtmlTag D3 *JSWorld -> *(D3, *JSWorld)
appendHtml html root world
  # (g, world) = append "g" root world
  = setHtml (toString html) g world
