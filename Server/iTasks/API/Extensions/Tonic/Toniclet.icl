implementation module iTasks.API.Extensions.Tonic.Toniclet

import iTasks
import iTasks.API.Extensions.Graphlet.D3
import iTasks.API.Extensions.Graphlet.Dagre
import iTasks.API.Extensions.Graphlet.DagreD3
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import iTasks.API.Extensions.Tonic.TonicRenderer
import iTasks.Framework.Tonic
import iTasks.Framework.Tonic.AbsSyn
from iTasks.API.Extensions.Graphlet.Graphlib import delNode, delEdge
import StdMisc
import StdArray
from Data.Graph import :: Graph, :: Node{..}, :: NodeIndex, :: EdgeIndex, emptyGraph
import qualified Data.Graph as DG
import qualified Data.Map as DM
import dynamic_string

derive gEditor Graph, Node
derive gEditMeta Graph, Node
derive gDefault Graph, Node
derive gUpdate Graph, Node
derive gVerify Graph, Node
derive gText Graph, Node

derive class iTask TonicletDiff

mkSVGId :: String -> String
mkSVGId x = "svg" +++ x

toniclet :: (Maybe TonicTask) (Maybe Int) -> Editlet (Maybe TonicTask) [TonicletDiff]
toniclet mtt manid =
  Editlet mtt
    { EditletServerDef
    | genUI   = \cid world -> (uiDef cid, world)
    , defVal  = Nothing
    , genDiff = genServerDiff
    , appDiff = appServerDiff
    }
    { EditletClientDef
    | updateUI = updateUI
    , defVal   = { mbClientState = Nothing
                 , tonicTask     = mtt
                 }
    , genDiff  = genClientDiff
    , appDiff  = appClientDiff
    }
  where
  uiDef cid
    = { html          = SvgTag [IdAttr (mkSVGId cid), ClassAttr "graphletGraph"]
                               []
                               [GElt [] [TransformAttr [TranslateTransform "20" "20"]] []]
      , eventHandlers = []
      , width         = ExactSize 1024
      , height        = ExactSize 512
      }

  updateUI cid diffs clval=:{mbClientState=Nothing} world
    # (dagre, world) = findObject "dagreD3" world
    | jsIsUndefined dagre
        # world = foldr addCSSFromUrl world tonicRenderer.styleSheets
        # world = addJSFromUrl "/d3.v3.min.js" Nothing world
        # world = addJSFromUrl "/dagre.js" Nothing world
        # world = addJSFromUrl "/dagre-d3.js" (Just (createEditletEventHandler (onLibLoaded diffs) cid)) world
        = (clval, world)
    | otherwise
        = onLibLoaded diffs cid undef clval world

  updateUI cid (Just xs) clval=:{mbClientState=Just {graphObj}} world
    = updateUI` cid (Just xs) clval world
    where
    updateUI` cid (Just [RemoveNodes rmnds:diffs]) clval=:{mbClientState=Just {graphObj}} world
      # world = snd (foldr removeNode` (graphObj, world) rmnds)
      = updateUI` cid (Just diffs) clval world
      where
      removeNode` :: NodeIndex *(GLGraph, *JSWorld) -> *(GLGraph, *JSWorld)
      removeNode` ni (jsgraph, world)
        # world = delNode jsgraph (toJSVal ni) world
        = (jsgraph, world)

    updateUI` cid (Just [RemoveEdges rmes:diffs]) clval=:{mbClientState=Just {graphObj}} world
      # world = snd (foldr removeEdge` (graphObj, world) rmes)
      = updateUI` cid (Just diffs) clval world
      where
      removeEdge` :: EdgeIndex *(GLGraph, *JSWorld) -> *(GLGraph, *JSWorld)
      removeEdge` (fromNode, toNode) (jsgraph, world)
        # world = delEdge jsgraph (toJSVal [fromNode, toNode]) world
        = (jsgraph, world)

    updateUI` cid (Just [AddNodes ans:diffs]) clval=:{mbClientState=Just {graphObj}} world
      # world = snd (foldr addNode` (graphObj, world) ans)
      = updateUI` cid (Just diffs) clval world
      where
      addNode` :: (n, NodeIndex) *(GLGraph, *JSWorld) -> *(GLGraph, *JSWorld)
      addNode` (node, ni) (jsgraph, world)
        # (obj, world) = jsEmptyObject world
        # world        = jsSetObjectAttr "node" (toJSVal ni) obj world
        # world        = addNode jsgraph (toJSVal ni) (toJSVal obj) world
        = (jsgraph, world)

    updateUI` cid (Just [AddEdges aes:diffs]) clval=:{mbClientState=Just {graphObj}} world
      # world = snd (foldr addEdge` (graphObj, world) aes)
      = updateUI` cid (Just diffs) clval world
      where
      addEdge` :: (e, EdgeIndex) *(GLGraph, *JSWorld) -> *(GLGraph, *JSWorld)
      addEdge` (edge, (fromNode, toNode)) (jsgraph, world)
        # (obj, world) = jsEmptyObject world
        # world        = addEdge jsgraph (toJSVal [fromNode, toNode]) (toJSVal fromNode) (toJSVal toNode) obj world
        = (jsgraph, world)

    updateUI` cid (Just [UpdateNodes uns:diffs]) clval=:{mbClientState=Just {graphObj}} world
      # world = snd (foldr updateNode` (graphObj, world) uns)
      = updateUI` cid (Just diffs) clval world
      where
      updateNode` (node, ni) (jsgraph, world)
        # world = setNodeValue graphObj (toJSVal ni) (toJSVal node) world
        = (jsgraph, world)

    updateUI` cid (Just []) clval=:{tonicTask, mbClientState=Just {graphObj, svgTarget}} world
      // Start hackish solution to prevent double rerendering
      # (subs, world)    = selectAllChildElems svgTarget "g" world
      # (_, world)       = removeElems subs world
      // End hackish solution to prevent double rerendering
      # (drend, world)   = mkRenderer world
      # renderNodeFun    = createEditletEventHandler drawNodeCb cid
      # renderEdgeLblFun = createEditletEventHandler drawEdgeLabelCb cid
      # world            = setDrawNode drend renderNodeFun world
      # world            = setDrawEdgeLabel drend renderEdgeLblFun world
      # (layout, world)  = mkLayout world
      # world            = rankDir layout LR world
      # world            = setLayout drend layout world
      # world            = runRenderer drend graphObj svgTarget world
      = (clval, world)

    updateUI` _ _ clval world
      = (clval, world)

  updateUI _ _ clval world
    = (clval, world)

  onLibLoaded diffs cid _ clval=:{mbClientState=Nothing} world
    # (svgg, world)    = selectElem ("#" +++ mkSVGId cid) world
    # (jsgraph, world) = mkDigraph world
    # clval            = {clval & mbClientState = Just {graphObj = jsgraph, svgTarget = svgg}}
    = updateUI cid diffs clval world

  onLibLoaded diffs cid _ clval world
    = updateUI cid diffs clval world

  drawNodeCb cid {[0] = jsgraph, [1] = u, [2] = root} clval world
    # graphValue = jsUnsafeCoerce jsgraph
    # nodeId     = jsValToInt (jsUnsafeCoerce u)
    # rootElem   = jsUnsafeCoerce root
    # world      = case fmap (\tt -> 'DG'.getNodeData nodeId tt.tt_graph) clval.tonicTask of
                     (Just (Just nodeVal)) -> tonicRenderer.drawNodeCallback (manid === Just nodeId) nodeVal graphValue nodeId rootElem world
                     _                     -> world
    = (clval, world)

  drawEdgeLabelCb cid {[0] = jsgraph, [1] = e, [2] = root} clval world
    # graphValue    = jsUnsafeCoerce jsgraph
    # edgeIdLst     = jsUnsafeCoerce e
    # (fEId, world) = jsGetObjectEl 0 edgeIdLst world
    # (tEId, world) = jsGetObjectEl 1 edgeIdLst world
    # edgeId        = (jsValToInt fEId, jsValToInt tEId)
    # rootElem      = jsUnsafeCoerce root
    # world         = case fmap (\tt -> 'DG'.getEdgeData edgeId tt.tt_graph) clval.tonicTask of
                        (Just (Just edgeVal)) -> tonicRenderer.drawEdgeLabelCallback edgeVal graphValue edgeId rootElem world
                        _                     -> world
    = (clval, world)

  genServerDiff (Nothing) (Just tt)  = genGraphDiff emptyGraph tt.tt_graph
  genServerDiff (Just tt) (Nothing)  = genGraphDiff tt.tt_graph emptyGraph
  genServerDiff (Just tt) (Just tt`) = genGraphDiff tt.tt_graph tt`.tt_graph
  genServerDiff _         _          = Nothing

  genGraphDiff oldGraph newGraph = case rmNodes ++ rmEdges ++ addNodes ++ addEdges ++ updateNodes of
                                     []    -> Nothing
                                     diffs -> Just diffs
    where
    rmNodes     = case 'DG'.foldrNodes (\ni _ xs -> if ('DG'.nodeExists ni newGraph) xs [ni:xs]) [] oldGraph of
                    [] -> []
                    xs -> [RemoveNodes xs]
    rmEdges     = case 'DG'.foldrEdges (\ei _ xs -> if ('DG'.edgeExists ei newGraph) xs [ei:xs]) [] oldGraph of
                    [] -> []
                    xs -> [RemoveEdges xs]
    addNodes    = case 'DG'.foldlNodes (\xs ni {data} -> if ('DG'.nodeExists ni oldGraph) xs [(data, ni):xs]) [] newGraph of // TODO use foldlNodes ?
                    [] ->  []
                    xs ->  [AddNodes xs]
    addEdges    = case 'DG'.foldrEdges (\ei edge xs -> if ('DG'.edgeExists ei oldGraph) xs [(edge, ei):xs]) [] newGraph of // TODO user foldlEdges ?
                    [] -> []
                    xs -> [AddEdges xs]
    updateNodes = case 'DG'.foldrNodes (\ni {data=newData} xs -> case 'DG'.getNodeData ni oldGraph of
                                                                   Just oldData -> if (newData =!= oldData) [(newData, ni):xs] xs
                                                                   _            -> xs) [] newGraph of
                    [] -> []
                    xs -> [UpdateNodes xs]

  appServerDiff diffs serverState = fmap (\st -> {st & tt_graph = appGraphDiff diffs st.tt_graph}) serverState

  appGraphDiff diffs graph = foldl f graph diffs
    where
    f graph (RemoveNodes ns) = foldr 'DG'.removeNode graph ns
    f graph (RemoveEdges es) = foldr 'DG'.removeEdge graph es
    f graph (AddNodes ns)    = foldr (\(n, ni) g -> snd ('DG'.addNode n g)) graph ns
    f graph (AddEdges es)    = foldr (\(e, ei) g -> 'DG'.addEdge e ei g) graph es
    f graph (UpdateNodes ns) = foldr (\(n, ni) g -> 'DG'.setNodeData ni n g) graph ns
    f graph _                = graph

  genClientDiff {tonicTask=Nothing} {tonicTask=Just tt}  = genGraphDiff emptyGraph tt.tt_graph
  genClientDiff {tonicTask=Just tt} {tonicTask=Nothing}  = genGraphDiff tt.tt_graph emptyGraph
  genClientDiff {tonicTask=Just tt} {tonicTask=Just tt`} = genGraphDiff tt.tt_graph tt`.tt_graph
  genClientDiff _                   _                    = Nothing

  appClientDiff diffs cs=:{tonicTask=(Just tt=:{tt_graph})} = {cs & tonicTask=Just {tt & tt_graph = appGraphDiff diffs tt_graph}}
  appClientDiff diffs cs                                    = cs
