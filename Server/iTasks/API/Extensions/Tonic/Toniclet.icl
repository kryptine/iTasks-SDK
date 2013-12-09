implementation module iTasks.API.Extensions.Tonic.Toniclet

import iTasks
import iTasks.API.Extensions.Graphlet.D3
import iTasks.API.Extensions.Graphlet.DagreD3
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import iTasks.Framework.Tonic.AbsSyn
import StdMisc
import StdDebug
from StdArray import class Array (select, uselect), instance Array {} a
from Data.Graph import :: Graph
import qualified Data.Graph as DG
import dynamic_string

derive gEditor
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, Graph, Node

derive gEditMeta
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, Graph, Node

derive gVisualizeText
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, Graph, Node

derive gDefault
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, Graph, Node

derive gUpdate
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, Graph, Node

derive gVerify
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, Graph, Node

mkSVGId :: String -> String
mkSVGId x = "svg" +++ x

toniclet :: GinGraph -> Editlet GinGraph GinGraph
toniclet g
  = toEditlet simpl
  where
  simpl = EditletSimpl g
            { EditletSimplDef
            | genUI    = \cid world -> (uiDef cid, world)
            , updateUI = onUpdate
            , genDiff  = genDiff
            , appDiff  = appDiff
            }

  uiDef cid
    = { html          = SvgTag [IdAttr (mkSVGId cid), ClassAttr "tonicletGraph", WidthAttr "800px", HeightAttr "600px"]
                               [GTag [TransformAttr "translate(20, 20)"] []]
      , eventHandlers = []
      , width         = ExactSize 800
      , height        = ExactSize 600
      }

  loadLibs pid world
    # world = addCSSFromUrl "/Toniclet.css" world
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
    # (d3, world)  = append "g" (jsUnsafeCoerce root) world
    # (d3, world)  = setAttr "class" (toJSVal "label") d3 world
    # (nv, world)  = getNodeValue (jsUnsafeCoerce graph) (jsUnsafeCoerce u) world
    # (str, world) = jsGetObjectAttr "node" nv world
    # (node, _)    = copy_from_string (jsValToString str)
    # world        = addLabel (toJSVal "Node label") d3 10 10 world
    = (gg, world)

  drawEdgeLabelCb pid {[0] = graph, [1] = e, [2] = root} gg world
    # (d3, world)  = append "g" (jsUnsafeCoerce root) world
    # (d3, world)  = setAttr "class" (toJSVal "edge-label") d3 world
    # (ev, world)  = getEdgeValue (jsUnsafeCoerce graph) (jsUnsafeCoerce e) world
    # (lbl, world) = jsGetObjectAttr "label" ev world
    # world        = addLabel lbl d3 0 0 world
    = (gg, world)

  addLabel label root mx my world
    # (rect, world)   = append "rect" root world
    # (lblSvg, world) = append "g" root world
    # (d3, world)     = append "text" root world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # (d3, world)     = append "tspan" d3 world
    # (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    # (d3, world)     = setText (jsValToString label) d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("rx", toJSVal 5)
                                 , ("ry", toJSVal 5)
                                 , ("x", toJSVal (0 - ((bbw / 2) + mx)))
                                 , ("y", toJSVal (0 - ((bbh / 2) + my)))
                                 , ("width", toJSVal (bbw + (2 * mx)))
                                 , ("height", toJSVal (bbh + (2 * my)))
                                 ] rect world
    = world

  onUpdate pid _ val world
    # (joint, world) = findObject "dagreD3" world
    | jsIsUndefined joint
        # world = loadLibs pid world
        = (val, world)
    | otherwise
        = onLibLoaded pid Nothing val world

  genDiff _ _ = Just g
  appDiff g _ = g

addNodesEdges g jgrph world
  # (_, world) = 'DG'.foldrNodes addNode` (jgrph, world) g
  # (_, world) = 'DG'.foldrEdges addEdge` (jgrph, world) g
  = world
  where
  addNode` ni node (jgrph, world)
    # (obj, world) = jsEmptyObject world
    # world        = jsSetObjectAttr "node" (toJSVal (copy_to_string node)) obj world
    # world        = addNode jgrph (toJSVal ni) (toJSVal obj) world
    = (jgrph, world)
  addEdge` (fromNode, toNode) {edge_pattern} (jgrph, world)
    # (obj, world) = jsEmptyObject world
    # world        = jsSetObjectAttr "label" (toJSVal ("Edge" +++ toString fromNode +++ "-" +++ toString toNode)) obj world
    # world        = addEdge jgrph jsNull (toJSVal fromNode) (toJSVal toNode) obj world
    = (jgrph, world)

    //= case node.data.nodeType of
        //GAssign expr
          //# (anon, world) = jsEmptyObject world
          //# world         = jsSetObjectAttr "id" (toJSVal ni) anon world
          //# (dec, world)  = jsNewObject "joint.shapes.tonic.AssignFigure" [toJSArg anon] world
          //# world         = addCell dec jgrph world
          //= (jgrph, world)

        //GDecision dt expr
          //# args = { ReturnStateArgs
                   //| id = ni
                   //, size  = {Size | width = 100, height = 100}
                   //, attrs = { Attrs
                             //| text = { TextAttrs
                                      //| text = expr} }
                   //}
          //# (dec, world)  = jsNewObject "joint.shapes.tonic.DecisionState" [toJSArg args] world
          //# world         = addCell dec jgrph world
          //= (jgrph, world)

        //GLet lt
          //# binds = foldr (\(l, r) xs -> l +++ " = " +++ r +++ xs) "" lt.glet_binds
          //# args = { LetArgs
                   //| id = ni
                   //, size  = {Size | width = 100, height = 100}
                   //, attrs = { Attrs
                             //| text = { TextAttrs
                                      //| text = binds} }
                   //}
          //# (ltst, world) = jsNewObject "joint.shapes.tonic.LetState" [toJSArg args] world
          //# world         = addCell ltst jgrph world
          //= (jgrph, world)

        //GInit
          //# (anon, world)  = jsEmptyObject world
          //# world          = jsSetObjectAttr "id" (toJSVal ni) anon world
          //# (start, world) = jsNewObject "joint.shapes.tonic.StartState" [toJSArg anon] world
          //# world          = addCell start jgrph world
          //= (jgrph, world)

        //GListComprehension lc
          //# ident        = case lc.input of
                             //GCleanExpression ce -> ce
                             //_                   -> "a list"
          //# args         = { ListComprehensionArgs
                           //| id   = ni
                           //, size = {Size | width = 100, height = 100}
                           //, name = ident
                           //}
          //# (dec, world)   = jsNewObject "joint.shapes.tonic.ListComprehension" [toJSArg args] world
          //# world          = addCell dec jgrph world
          //= (jgrph, world)

        //GParallelSplit
          //# args = { ParSplitArgs
                   //| id = ni
                   //, size  = {Size | width = 100, height = 100}
                   //, attrs = { Attrs
                             //| text = { TextAttrs
                                      //| text = "Start parallel tasks"} }
                   //}
          //# (dec, world)   = jsNewObject "joint.shapes.tonic.ParallelSplit" [toJSArg args] world
          //# world          = addCell dec jgrph world
          //= (jgrph, world)

        //GParallelJoin jt
          //# args = { ParJoinArgs
                   //| id = ni
                   //, size  = {Size | width = 100, height = 100}
                   //, attrs = { Attrs
                             //| text = { TextAttrs
                                      //| text = ppJT jt} }
                   //}
          //# (dec, world)  = jsNewObject "joint.shapes.tonic.ParallelJoin" [toJSArg args] world
          //# world         = addCell dec jgrph world
          //= (jgrph, world)
          //where ppJT DisFirstBin  = "First finished task"
                //ppJT DisFirstList = "First finished task"
                //ppJT DisLeft      = "Left task result"
                //ppJT DisRight     = "Right task result"
                //ppJT ConAll       = "All task results"
                //ppJT ConPair      = "Pair of task results"

        //GReturn (GCleanExpression expr)
          //# args = { ReturnStateArgs
                   //| id    = ni
                   //, size  = {Size | width = 50, height = 50}
                   //, attrs = { Attrs
                             //| text = { TextAttrs
                                      //| text = expr } }
                   //}
          //# (ret, world) = jsNewObject "joint.shapes.tonic.Return" [toJSArg args] world
          //# world        = addCell ret jgrph world
          //= (jgrph, world)

        //GReturn expr
          //# args = { ReturnStateArgs
                   //| id = ni
                   //, size  = {Size | width = 50, height = 50}
                   //, attrs = { Attrs
                             //| text = { TextAttrs
                                      //| text = "TODO: expr :: GExpression" } }
                   //}
          //# (ret, world) = jsNewObject "joint.shapes.tonic.Return" [toJSArg args] world
          //# world        = addCell ret jgrph world
          //= (jgrph, world)

        //GStep
          //# (anon, world) = jsEmptyObject world
          //# world         = jsSetObjectAttr "id" (toJSVal ni) anon world
          //# (dec, world)  = jsNewObject "joint.shapes.tonic.Step" [toJSArg anon] world
          //# world         = addCell dec jgrph world
          //= (jgrph, world)

        //GStop
          //# (anon, world) = jsEmptyObject world
          //# world         = jsSetObjectAttr "id" (toJSVal ni) anon world
          //# (stop, world) = jsNewObject "joint.shapes.tonic.StopState" [toJSArg anon] world
          //# world         = addCell stop jgrph world
          //= (jgrph, world)

        //GTaskApp ident exprs
          //# args         = { TaskAppArgs
                           //| id   = ni
                           //, size = {Size | width = 100, height = 100}
                           //, name = ident
                           //}
                           //// TODO: Add exprs
          //# (app, world) = jsNewObject "joint.shapes.tonic.TaskApp" [toJSArg args] world
          //# world        = addCell app jgrph world
          //= (jgrph, world)
        //_ = (jgrph, world)

:: GraphMeta =
  { label :: String
  }

drawNode :: GLGraph (JSVal Int) D3 *JSWorld -> *JSWorld
drawNode graph u root world
  # (g, world)       = append "g" root world
  # (lbl, world)     = setAttr "class" (toJSVal "label") g world
  # (node, world)    = getNodeValue graph u world
  # (nodeLbl, world) = jsGetObjectAttr "label" node world
  // TODO: reimplement addLabel here
  # (_, world)       = callFunction "addLabel" [toJSArg nodeLbl, toJSArg lbl, toJSArg 0, toJSArg 0] world
  = world

drawEdgeLabel :: GLGraph (JSVal Int) D3 *JSWorld -> *JSWorld
drawEdgeLabel graph e root world
  # (g, world)       = append "g" root world
  # (lbl, world)     = setAttr "class" (toJSVal "edge-label") g world
  # (node, world)    = getEdgeValue graph e world
  # (nodeLbl, world) = jsGetObjectAttr "label" node world
  // TODO: reimplement addLabel here
  # (_, world)       = callFunction "addLabel" [toJSArg nodeLbl, toJSArg lbl, toJSArg 0, toJSArg 0] world
  = world

addCell :: (JSVal o) (JSVal g) *JSWorld -> *JSWorld
addCell cell jgrph world = snd (callObjectMethod "addCell" [toJSArg cell] jgrph world)

//mkBind :: Int Int (Maybe String) (JSVal g) *JSWorld -> *JSWorld
//mkBind sid tid mtxt jgrph world
  //# args          = {ArrowArgs | source = {Identifier | id = toString sid}
                               //, target = {Identifier | id = toString tid}
                               //, labels = mkLabels mtxt}
  //# world = jsTrace (toJSVal args) world
  //# (bind, world) = jsNewObject "joint.shapes.tonic.Bind" [toJSArg args] world
  //= addCell bind jgrph world
  //where
  //mkLabels Nothing    = []
  //mkLabels (Just lbl) = [ {LabelArgs
                        //| position = 0.5
                        //, attrs    = { Attrs
                                     //| text = { TextAttrs
                                     //| text = lbl } } }
                        //]

layoutGraph :: (JSVal g) *JSWorld -> *JSWorld
layoutGraph g world
  # (obj, world) = jsEmptyObject world
  # world        = jsSetObjectAttr "setLinkVertices" (toJSVal False) obj world
  # world        = jsSetObjectAttr "rankDir" (toJSVal "LR") obj world
  # (_, world)   = callFunction "joint.layout.DirectedGraph.layout" [toJSArg g, toJSArg obj] world
  = world

:: ArrowArgs =
  { source :: Identifier
  , target :: Identifier
  , labels :: [LabelArgs]
  }

:: LabelArgs =
  { position :: Real
  , attrs    :: Attrs
  }

:: Attrs =
  { text :: TextAttrs
  }

:: TextAttrs =
  { text :: String
  }

:: Identifier =
  { id :: String
  }

:: PaperArgs = E.e m:
  { el       :: JSVal e
  , width    :: Int
  , height   :: Int
  , gridSize :: Int
  , model    :: JSVal m
  }

:: Point =
  { x :: Int
  , y :: Int
  }

:: Size =
  { width  :: Int
  , height :: Int
  }

:: DecisionArgs =
  { id   :: Int
  , size :: Size
  , name :: String
  }

:: TaskAppArgs =
  { id   :: Int
  , size :: Size
  , name :: String
  }

:: ReturnStateArgs =
  { id    :: Int
  , size  :: Size
  , attrs :: Attrs
  }

:: ListComprehensionArgs =
  { id   :: Int
  , size :: Size
  , name :: String
  }

:: LetArgs =
  { id   :: Int
  , size :: Size
  , attrs :: Attrs
  }

:: ParSplitArgs =
  { id   :: Int
  , size :: Size
  , attrs :: Attrs
  }

:: ParJoinArgs =
  { id   :: Int
  , size :: Size
  , attrs :: Attrs
  }
