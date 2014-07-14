implementation module iTasks.API.Extensions.Graphlet.Graphlet

import iTasks
import iTasks.API.Extensions.Graphlet.D3
import iTasks.API.Extensions.Graphlet.DagreD3
import iTasks.API.Extensions.Graphlet.Graphlib
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Tasklet
import iTasks.API.Core.Client.Interface
import StdMisc
import StdDebug
import StdArray
from Data.Graph import :: Graph, :: Node{..}, :: NodeIndex, :: EdgeIndex
import qualified Data.Graph as DG
import dynamic_string

derive gEditor Graph, Node
derive gEditMeta Graph, Node
derive gText Graph, Node
derive gDefault Graph, Node
derive gUpdate Graph, Node
derive gVerify Graph, Node

derive class iTask GraphletDiff, Graphlet

mkSVGId :: String -> String
mkSVGId x = "svg" +++ x

graphletTasklet :: (GraphletRenderer n e) (Graphlet n e)
         -> Tasklet (GraphletClientData n e) Void | iTask n & iTask e
graphletTasklet renderer graphlet =
			{ Tasklet
			| genUI      = \tid _ world -> (TaskletHTML (graphletUIDef (toString tid) eventHandlers), {graphlet = graphlet, mbClientState=Nothing}, world)
			, resultFunc = const (Value Void True)
			, tweakUI    = id
			}
where
	eventHandlers = [ComponentEvent "tasklet" "init" onInit]
	
	diffs = genServerDiff defGraphlet graphlet
	
	onInit taskId _ clval world 
		= updateUI foo renderer (toString taskId) diffs clval world
	where
		foo hnd taskId = createTaskletEventHandler (\taskId obj val st -> hnd (toString taskId) obj val st) (fromString taskId)

graphletEditlet :: (GraphletRenderer n e) (Graphlet n e)
         -> Editlet (Graphlet n e) [GraphletDiff n e] | iTask n & iTask e
graphletEditlet renderer graphlet =
  Editlet graphlet
    { EditletServerDef
    | genUI   = \cid world -> (graphletUIDef cid [], world)
    , defVal  = defGraphlet
    , genDiff = genServerDiff
    , appDiff = appServerDiff
    }
    { EditletClientDef
    | updateUI = updateUI createEditletEventHandler renderer
    , defVal   = { mbClientState = Nothing
                 , graphlet      = defGraphlet
                 }
    , genDiff  = genClientDiff
    , appDiff  = appClientDiff
    }

updateUI hndCreater renderer cid diffs clval=:{mbClientState=Nothing} world
    # (dagre, world) = findObject "dagreD3" world
    | jsIsUndefined dagre
        # world = foldr addCSSFromUrl world renderer.styleSheets
        # world = addJSFromUrl "/d3.v3.min.js" Nothing world
        # world = addJSFromUrl "/dagre.js" Nothing world
        # world = addJSFromUrl "/dagre-d3.js" (Just (hndCreater (onLibLoaded hndCreater renderer diffs) cid)) world
        = (clval, world)
    | otherwise
        = onLibLoaded hndCreater renderer diffs cid undef clval world

updateUI hndCreater renderer cid (Just xs) clval=:{mbClientState=Just {graphObj}} world
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

    updateUI` cid (Just []) clval=:{graphlet, mbClientState=Just {graphObj, svgTarget}} world
      // Start hackish solution to prevent double rerendering
      # (subs, world)    = selectAllChildElems svgTarget "g" world
      # (_, world)       = removeElems subs world
      // End hackish solution to prevent double rerendering
      # (drend, world)   = mkRenderer world
      # renderNodeFun    = hndCreater (drawNodeCb renderer) cid
      # renderEdgeLblFun = hndCreater (drawEdgeLabelCb renderer) cid
      # world            = setDrawNode drend renderNodeFun world
      # world            = setDrawEdgeLabel drend renderEdgeLblFun world
      # world            = runRenderer drend graphObj svgTarget world
      = (clval, world)

    updateUI` _ _ clval world
      = (clval, world)

updateUI _ _ _ _ clval world
    = (clval, world)

onLibLoaded hndCreater renderer diffs cid _ clval=:{mbClientState=Nothing} world
    # (svgg, world)    = selectElem ("#" +++ mkSVGId cid) world
    # (jsgraph, world) = mkDagreD3Digraph world
    # clval            = {clval & mbClientState = Just {graphObj = jsgraph, svgTarget = svgg}}
    = updateUI hndCreater renderer cid diffs clval world

onLibLoaded hndCreater renderer diffs cid _ clval world
    = updateUI hndCreater renderer cid diffs clval world

drawNodeCb renderer cid {[0] = jsgraph, [1] = u, [2] = root} clval world
    # graphValue = jsUnsafeCoerce jsgraph
    # nodeId     = jsValToInt (jsUnsafeCoerce u)
    # rootElem   = jsUnsafeCoerce root
    # world      = case 'DG'.getNodeData nodeId clval.graphlet.graph of
                     Just nodeVal -> renderer.drawNodeCallback nodeVal graphValue nodeId rootElem world
                     _            -> world
    = (clval, world)

drawEdgeLabelCb renderer cid {[0] = jsgraph, [1] = e, [2] = root} clval world
    # graphValue    = jsUnsafeCoerce jsgraph
    # edgeIdLst     = jsUnsafeCoerce e
    # (fEId, world) = jsGetObjectEl 0 edgeIdLst world
    # (tEId, world) = jsGetObjectEl 1 edgeIdLst world
    # edgeId        = (jsValToInt fEId, jsValToInt tEId)
    # rootElem      = jsUnsafeCoerce root
    # world         = case 'DG'.getEdgeData edgeId clval.graphlet.graph of
                        Just edgeVal -> renderer.drawEdgeLabelCallback edgeVal graphValue edgeId rootElem world
                        _            -> world
    = (clval, world)

defGraphlet = { graph       = 'DG'.emptyGraph
              }

appServerDiff diffs serverState = {serverState & graph = appGraphDiff diffs serverState.graph}

appGraphDiff diffs graph = foldl f graph diffs
    where
    f graph (RemoveNodes ns) = foldr 'DG'.removeNode graph ns
    f graph (RemoveEdges es) = foldr 'DG'.removeEdge graph es
    f graph (AddNodes ns)    = foldr (\(n, ni) g -> snd ('DG'.addNode n g)) graph ns
    f graph (AddEdges es)    = foldr (\(e, ei) g -> 'DG'.addEdge e ei g) graph es
    f graph (UpdateNodes ns) = foldr (\(n, ni) g -> 'DG'.setNodeData ni n g) graph ns
    f graph _                = graph

genClientDiff oldSt newSt = genGraphDiff oldSt.graphlet.graph newSt.graphlet.graph

appClientDiff diffs cs=:{graphlet={graph}} = {cs & graphlet = {graph = appGraphDiff diffs graph}}

genServerDiff oldSt newSt =  genGraphDiff oldSt.graph newSt.graph

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

graphletUIDef cid eventHandlers
    = { html          = SvgTag [IdAttr (mkSVGId cid), ClassAttr "graphletGraph"] []
                               [GElt [] [TransformAttr [TranslateTransform "20" "20"]] []]
      , eventHandlers = eventHandlers
      , width         = ExactSize 1920
      , height        = ExactSize 1080
      }

mappendMaybeList (Just xs) (Just ys) = Just (xs ++ ys)
mappendMaybeList (Just xs) _         = Just xs
mappendMaybeList _         (Just ys) = Just ys
mappendMaybeList _         _         = Nothing
