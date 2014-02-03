implementation module iTasks.API.Extensions.Tonic.Toniclet

import iTasks
import iTasks.API.Extensions.Graphlet.D3
import iTasks.API.Extensions.Graphlet.DagreD3
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import StdMisc
import StdDebug
import StdArray
from Data.Graph import :: Graph, :: Node{..}, :: NodeIndex, :: EdgeIndex
import qualified Data.Graph as DG
import dynamic_string

derive gEditor Graph, Node
derive gEditMeta Graph, Node
derive gVisualizeText Graph, Node
derive gDefault Graph, Node
derive gUpdate Graph, Node
derive gVerify Graph, Node

derive class iTask GraphletDiff, Graphlet

mkSVGId :: String -> String
mkSVGId x = "svg" +++ x

graphlet :: (GraphletRenderer n e) (Graphlet n e)
         -> Editlet (Graphlet n e) [GraphletDiff n e] | iTask n & iTask e
graphlet renderer graphlet =
  Editlet graphlet
    { EditletServerDef
    | genUI   = \cid world -> (uiDef cid, world)
    , defVal  = defGraphlet
    , genDiff = genServerDiff
    , appDiff = appServerDiff
    }
    { EditletClientDef
    | updateUI = updateUI
    , defVal   = { mbClientState = Nothing
                 , graphlet      = defGraphlet
                 }
    , genDiff  = genClientDiff
    , appDiff  = appClientDiff
    }
  where
  defGraphlet = graphlet // { graph = 'DG'.emptyGraph
                //, tonicState = []
                //}

  uiDef cid
    = { html          = SvgTag [IdAttr (mkSVGId cid), ClassAttr "graphletGraph"]
                               [GTag [TransformAttr "translate(20, 20)"] []]
      , eventHandlers = []
      , width         = WrapSize
      , height        = WrapSize
      }

  updateUI cid _ clval=:{mbClientState=Nothing} world
    # (dagre, world) = findObject "dagreD3" world
    | jsIsUndefined dagre
        # world = foldr addCSSFromUrl world renderer.styleSheets
        # world = addJSFromUrl "/d3.v3.min.js" Nothing world
        # world = addJSFromUrl "/dagre.js" Nothing world
        # world = addJSFromUrl "/dagre-d3.js" (Just (createEditletEventHandler onLibLoaded cid)) world
        = (clval, world)
    | otherwise
        = onLibLoaded cid undef clval world

  updateUI cid (Just [RemoveNodes rmnds:diffs]) clval=:{mbClientState=Just {graphObj}} world
    = updateUI cid (Just diffs) clval world

  updateUI cid (Just [RemoveEdges rmes:diffs]) clval=:{mbClientState=Just {graphObj}} world
    = updateUI cid (Just diffs) clval world

  updateUI cid (Just [AddNodes ans:diffs]) clval=:{mbClientState=Just {graphObj}} world
    = updateUI cid (Just diffs) clval world

  updateUI cid (Just [AddEdges aes:diffs]) clval=:{mbClientState=Just {graphObj}} world
    = updateUI cid (Just diffs) clval world

  updateUI cid (Just [UpdateNodes uns:diffs]) clval=:{mbClientState=Just {graphObj}} world
    = updateUI cid (Just diffs) clval world

  updateUI _ _ clval world
    = (clval, world)

  onLibLoaded cid _ clval=:{graphlet} world
    # (svgg, world)    = selectElem ("#" +++ mkSVGId cid) world
// Start hackish solution to rerendering
    # (subs, world)    = selectAllChildElems svgg "g" world
    # (_, world)       = removeElems subs world
// End hackish solution to rerendering
    # (jsgraph, world) = mkDigraph world
    # world            = addNodesEdges graphlet.graph jsgraph world
    # (drend, world)   = mkRenderer world
    # renderNodeFun    = createEditletEventHandler (drawNodeCb graphlet.tonicState) cid
    # renderEdgeLblFun = createEditletEventHandler (drawEdgeLabelCb graphlet.tonicState) cid
    # world            = setDrawNode drend renderNodeFun world
    # world            = setDrawEdgeLabel drend renderEdgeLblFun world
    # world            = runRenderer drend jsgraph svgg world
    = (clval, world)

  //drawNodeCb :: s ComponentId {JSVal JSEvent} (Graph n e) *JSWorld
             //-> *(Graph n e, *JSWorld) // | iTask n & iTask e
  drawNodeCb st cid {[0] = jsgraph, [1] = u, [2] = root} cgraph world
    # graphValue = jsUnsafeCoerce jsgraph
    # nodeId     = jsValToInt (jsUnsafeCoerce u)
    # rootElem   = jsUnsafeCoerce root
    # world      = case 'DG'.getNodeData nodeId graphlet.graph of
                     Just nodeVal -> renderer.drawNodeCallback st nodeVal graphValue nodeId rootElem world
                     _            -> world
    = (cgraph, world)

  //drawEdgeLabelCb :: s ComponentId {JSVal JSEvent} (Graph n e) *JSWorld
                  //-> *(Graph n e, *JSWorld) // | iTask n & iTask e
  drawEdgeLabelCb st cid {[0] = jsgraph, [1] = e, [2] = root} cgraph world
    # graphValue    = jsUnsafeCoerce jsgraph
    # edgeIdLst     = jsUnsafeCoerce e // We really need the #! here, because jsGetObjectEl will try to get elements from a thunk otherwise
    # (fEId, world) = jsGetObjectEl 0 edgeIdLst world
    # (tEId, world) = jsGetObjectEl 1 edgeIdLst world
    # edgeId        = (jsValToInt fEId, jsValToInt tEId)
    # rootElem      = jsUnsafeCoerce root
    # world         = case 'DG'.getEdgeData edgeId graphlet.graph of
                        Just edgeVal -> renderer.drawEdgeLabelCallback st edgeVal graphValue edgeId rootElem world
                        _            -> world
    = (cgraph, world)

  //genServerDiff :: (GraphletServerState s n e) (GraphletServerState s n e) -> Maybe [GraphletDiff n e]
  genServerDiff oldSt newSt = genDiff oldSt.graph newSt.graph

  genDiff oldGraph newGraph = case rmNodes ++ rmEdges ++ addNodes ++ addEdges ++ updateNodes of
                                []    -> Nothing
                                diffs -> Just diffs
    where
    rmNodes     = case 'DG'.foldrNodes (\ni _ xs -> if ('DG'.nodeExists ni newGraph) xs [ni:xs]) [] oldGraph of
                    [] -> []
                    xs -> [RemoveNodes xs]
    rmEdges     = case 'DG'.foldrEdges (\ei _ xs -> if ('DG'.edgeExists ei newGraph) xs [ei:xs]) [] oldGraph of
                    [] -> []
                    xs -> [RemoveEdges xs]
    addNodes    = case 'DG'.foldrNodes (\ni {data} xs -> if ('DG'.nodeExists ni oldGraph) xs [data:xs]) [] newGraph of // TODO use foldlNodes ?
                    [] -> []
                    xs -> [AddNodes xs]
    addEdges    = case 'DG'.foldrEdges (\ei edge xs -> if ('DG'.edgeExists ei oldGraph) xs [(edge, ei):xs]) [] newGraph of // TODO user foldlEdges ?
                    [] -> []
                    xs -> [AddEdges xs]
    updateNodes = case 'DG'.foldrNodes (\ni {data=newData} xs -> case 'DG'.getNodeData ni oldGraph of
                                                                   Just oldData -> if (newData =!= oldData) [(ni, newData):xs] xs
                                                                   _            -> xs) [] newGraph of
                    [] -> []
                    xs -> [UpdateNodes xs]

  //appServerDiff :: [GraphletDiff n e] (GraphletServerState s n e) -> GraphletServerState s n e
  appServerDiff diffs serverState = {serverState & graph = appDiff diffs serverState.graph}

  appDiff diffs graph = foldl f graph diffs
    where
    f graph (RemoveNodes ns) = foldr 'DG'.removeNode graph ns
    f graph (RemoveEdges es) = foldr 'DG'.removeEdge graph es
    f graph (AddNodes ns)    = foldr (\n g -> snd ('DG'.addNode n g)) graph ns
    f graph (AddEdges es)    = foldr (\(e, ei) g -> 'DG'.addEdge e ei g) graph es
    f graph (UpdateNodes ns) = foldr (\(ni, n) g -> 'DG'.setNodeData ni n g) graph ns

  //genClientDiff :: (GraphletClientData s) (GraphletClientData s) -> Maybe [GraphletDiff n e]
  genClientDiff oldSt newSt = genDiff oldSt.graphlet.graph newSt.graphlet.graph

  //appClientDiff :: [GraphletDiff n e] (GraphletClientData s) -> GraphletClientData s
  appClientDiff diffs cs=:{graphlet={graph, tonicState}} = {cs & graphlet = {graph = appDiff diffs graph, tonicState=tonicState}}

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
