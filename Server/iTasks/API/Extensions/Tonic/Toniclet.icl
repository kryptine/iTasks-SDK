implementation module iTasks.API.Extensions.Tonic.Toniclet

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import iTasks.Framework.Tonic.AbsSyn
import StdMisc
import Data.Graph

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

//jointDotJS :== "/joint/dist/joint.all.js"
//jointDotCSS :== "/joint/dist/joint.all.css"
//tonicShapes :== "/joint/plugins/joint.shapes.tonic.js"

mkSVGId :: String -> String
mkSVGId x = "svg" +++ x

import StdDebug, StdMisc

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
    = { html          = DivTag [IdAttr (mkSVGId cid), ClassAttr (mkSVGId cid)] []
      , eventHandlers = []
      , width         = ExactSize 1024
      , height        = ExactSize 768
      }

  loadLibs pid world
    # world = addCSSFromUrl "/Toniclet.css" world
    # world = addJSFromUrl "/d3.v3.min.js" Nothing world
    # world = addJSFromUrl "/dagre.js" Nothing world
    # world = addJSFromUrl "/dagre-d3.js" (Just (createEditletEventHandler onLibLoaded pid)) world
    = world

  onLibLoaded pid evt val world
    # (graph, world)    = jsNewObject "dagreD3.Digraph" [] world
    # (renderer, world) = jsNewObject "dagreD3.Renderer" [] world
    # renderNodeFun     = createEditletEventHandler drawNodeCb pid
    # renderEdgeLblFun  = createEditletEventHandler drawEdgeLabelCb pid
    # (_, world)        = callObjectMethod "drawNode" [toJSArg renderNodeFun] renderer world
    # (_, world)        = callObjectMethod "drawEdgeLabel" [toJSArg renderEdgeLblFun] renderer world
    # (d3, world)       = findObject "d3" world
    # (g, world)        = callObjectMethod "select" [toJSArg ("svg g#" +++ mkSVGId pid)] d3 world
    # (_, world)        = callObjectMethod "run" [toJSArg graph, toJSArg g] renderer world
    //# world          = drawTonicGraph val jgrph world
    = (val, world)

  drawNodeCb pid args gg world
    # world = jsTrace (toJSArg "drawNodeCb") world
    # world = jsTrace (toJSArg args) world
    = (gg, world)

  drawEdgeLabelCb pid args gg world
    # world = jsTrace (toJSArg "drawEdgeLabelCb") world
    # world = jsTrace (toJSArg args) world
    = (gg, world)

  onUpdate pid _ val world
    # (joint, world) = findObject "dagreD3" world
    | jsIsUndefined joint
        # world = loadLibs pid world
        = (val, world)
    | otherwise
        = onLibLoaded pid Nothing val world

  genDiff _ _ = Nothing
  appDiff g _ = g

drawTonicGraph g jgrph world
  # (_, world) = foldrNodes addNode (jgrph, world) g
  # (_, world) = foldrEdges addEdge (jgrph, world) g
  = layoutGraph jgrph world
  where
  addNode ni node (jgrph, world)
    = case node.data.nodeType of
        GAssign expr
          # (anon, world) = jsEmptyObject world
          # world         = jsSetObjectAttr "id" (toJSVal ni) anon world
          # (dec, world)  = jsNewObject "joint.shapes.tonic.AssignFigure" [toJSArg anon] world
          # world         = addCell dec jgrph world
          = (jgrph, world)

        GDecision dt expr
          # args = { ReturnStateArgs
                   | id = ni
                   , size  = {Size | width = 100, height = 100}
                   , attrs = { Attrs
                             | text = { TextAttrs
                                      | text = expr} }
                   }
          # (dec, world)  = jsNewObject "joint.shapes.tonic.DecisionState" [toJSArg args] world
          # world         = addCell dec jgrph world
          = (jgrph, world)

        GLet lt
          # binds = foldr (\(l, r) xs -> l +++ " = " +++ r +++ xs) "" lt.glet_binds
          # args = { LetArgs
                   | id = ni
                   , size  = {Size | width = 100, height = 100}
                   , attrs = { Attrs
                             | text = { TextAttrs
                                      | text = binds} }
                   }
          # (ltst, world) = jsNewObject "joint.shapes.tonic.LetState" [toJSArg args] world
          # world         = addCell ltst jgrph world
          = (jgrph, world)

        GInit
          # (anon, world)  = jsEmptyObject world
          # world          = jsSetObjectAttr "id" (toJSVal ni) anon world
          # (start, world) = jsNewObject "joint.shapes.tonic.StartState" [toJSArg anon] world
          # world          = addCell start jgrph world
          = (jgrph, world)

        GListComprehension lc
          # ident        = case lc.input of
                             GCleanExpression ce -> ce
                             _                   -> "a list"
          # args         = { ListComprehensionArgs
                           | id   = ni
                           , size = {Size | width = 100, height = 100}
                           , name = ident
                           }
          # (dec, world)   = jsNewObject "joint.shapes.tonic.ListComprehension" [toJSArg args] world
          # world          = addCell dec jgrph world
          = (jgrph, world)

        GParallelSplit
          # args = { ParSplitArgs
                   | id = ni
                   , size  = {Size | width = 100, height = 100}
                   , attrs = { Attrs
                             | text = { TextAttrs
                                      | text = "Start parallel tasks"} }
                   }
          # (dec, world)   = jsNewObject "joint.shapes.tonic.ParallelSplit" [toJSArg args] world
          # world          = addCell dec jgrph world
          = (jgrph, world)

        GParallelJoin jt
          # args = { ParJoinArgs
                   | id = ni
                   , size  = {Size | width = 100, height = 100}
                   , attrs = { Attrs
                             | text = { TextAttrs
                                      | text = ppJT jt} }
                   }
          # (dec, world)  = jsNewObject "joint.shapes.tonic.ParallelJoin" [toJSArg args] world
          # world         = addCell dec jgrph world
          = (jgrph, world)
          where ppJT DisFirstBin  = "First finished task"
                ppJT DisFirstList = "First finished task"
                ppJT DisLeft      = "Left task result"
                ppJT DisRight     = "Right task result"
                ppJT ConAll       = "All task results"
                ppJT ConPair      = "Pair of task results"

        GReturn (GCleanExpression expr)
          # args = { ReturnStateArgs
                   | id    = ni
                   , size  = {Size | width = 50, height = 50}
                   , attrs = { Attrs
                             | text = { TextAttrs
                                      | text = expr } }
                   }
          # (ret, world) = jsNewObject "joint.shapes.tonic.Return" [toJSArg args] world
          # world        = addCell ret jgrph world
          = (jgrph, world)

        GReturn expr
          # args = { ReturnStateArgs
                   | id = ni
                   , size  = {Size | width = 50, height = 50}
                   , attrs = { Attrs
                             | text = { TextAttrs
                                      | text = "TODO: expr :: GExpression" } }
                   }
          # (ret, world) = jsNewObject "joint.shapes.tonic.Return" [toJSArg args] world
          # world        = addCell ret jgrph world
          = (jgrph, world)

        GStep
          # (anon, world) = jsEmptyObject world
          # world         = jsSetObjectAttr "id" (toJSVal ni) anon world
          # (dec, world)  = jsNewObject "joint.shapes.tonic.Step" [toJSArg anon] world
          # world         = addCell dec jgrph world
          = (jgrph, world)

        GStop
          # (anon, world) = jsEmptyObject world
          # world         = jsSetObjectAttr "id" (toJSVal ni) anon world
          # (stop, world) = jsNewObject "joint.shapes.tonic.StopState" [toJSArg anon] world
          # world         = addCell stop jgrph world
          = (jgrph, world)

        GTaskApp ident exprs
          # args         = { TaskAppArgs
                           | id   = ni
                           , size = {Size | width = 100, height = 100}
                           , name = ident
                           }
                           // TODO: Add exprs
          # (app, world) = jsNewObject "joint.shapes.tonic.TaskApp" [toJSArg args] world
          # world        = addCell app jgrph world
          = (jgrph, world)
        _ = (jgrph, world)
  addEdge (fromNode, toNode) {edge_pattern} (jgrph, world)
    # world = jsTrace (toJSVal ("Adding edge from " +++ toString fromNode +++ " to " +++ toString toNode)) world
    # world = mkBind fromNode toNode edge_pattern jgrph world
    = (jgrph, world)


addNode :: (JSVal DGraph) Int GraphMeta *JSWorld -> *JSWorld
addNode graph ident attrs world = snd (callObjectMethod "addNode" [toJSArg ident, toJSArg attrs] graph world)

addEdge :: (JSVal DGraph) (JSVal f) (JSVal t) GraphMeta *JSWorld -> *JSWorld
addEdge graph fromNode toNode attrs world = snd (callObjectMethod "addEdge" [toJSArg jsNull, toJSArg fromNode, toJSArg toNode, toJSArg attrs] graph world)

:: DGraph = DGraph
:: DNode = DNode

:: GraphMeta =
  { label :: String
  }

drawNode :: (JSVal DGraph) (JSVal Int) (JSVal DNode) *JSWorld -> *JSWorld
drawNode graph u root world
  # (g, world)       = callObjectMethod "append" [toJSArg "g"] root world
  # (lbl, world)     = callObjectMethod "attr" [toJSArg "class", toJSArg "label"] g world
  # (node, world)    = callObjectMethod "node" [toJSArg u] graph world
  # (nodeLbl, world) = jsGetObjectAttr "label" node world
  # (_, world)       = callFunction "addLabel" [toJSArg nodeLbl, toJSArg lbl, toJSArg 0, toJSArg 0] world
  = world

drawEdgeLabel :: (JSVal DGraph) (JSVal Int) (JSVal DNode) *JSWorld -> *JSWorld
drawEdgeLabel graph e root world
  # (g, world)       = callObjectMethod "append" [toJSArg "g"] root world
  # (lbl, world)     = callObjectMethod "attr" [toJSArg "class", toJSArg "edge-label"] g world
  # (node, world)    = callObjectMethod "edge" [toJSArg e] graph world
  # (nodeLbl, world) = jsGetObjectAttr "label" node world
  # (_, world)       = callFunction "addLabel" [toJSArg nodeLbl, toJSArg lbl, toJSArg 0, toJSArg 0] world
  = world

addCell :: (JSVal o) (JSVal g) *JSWorld -> *JSWorld
addCell cell jgrph world = snd (callObjectMethod "addCell" [toJSArg cell] jgrph world)

mkBind :: Int Int (Maybe String) (JSVal g) *JSWorld -> *JSWorld
mkBind sid tid mtxt jgrph world
  # args          = {ArrowArgs | source = {Identifier | id = toString sid}
                               , target = {Identifier | id = toString tid}
                               , labels = mkLabels mtxt}
  # world = jsTrace (toJSVal args) world
  # (bind, world) = jsNewObject "joint.shapes.tonic.Bind" [toJSArg args] world
  = addCell bind jgrph world
  where
  mkLabels Nothing    = []
  mkLabels (Just lbl) = [ {LabelArgs
                        | position = 0.5
                        , attrs    = { Attrs
                                     | text = { TextAttrs
                                     | text = lbl } } }
                        ]

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
