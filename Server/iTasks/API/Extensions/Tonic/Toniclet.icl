implementation module iTasks.API.Extension.Tonic.Toniclet

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import iTasks.Framework.Tonic.AbsSyn
import StdMisc
import Data.Graph

jointDotJS :== "/joint/dist/joint.all.js"
jointDotCSS :== "/joint/dist/joint.all.css"
tonicShapes :== "/joint/plugins/joint.shapes.tonic.js"

mkPaperId :: String -> String
mkPaperId x = "paper" +++ x

toniclet :: GinGraph -> Editlet GinGraph GinGraph
toniclet g
  = Editlet g
  { html     = \pid -> DivTag [IdAttr (mkPaperId pid), ClassAttr (mkPaperId pid)] []
  , updateUI = onUpdate
  , handlers = \_ -> []
  , genDiff  = genDiff
  , appDiff  = appDiff
  }
  where
  loadJointJSLib pid world
    # world = addJSFromUrl jointDotJS (Just (createEditletEventHandler loadPlugins pid)) world
    = addCSSFromUrl jointDotCSS world

  loadPlugins pid evt val mst world
    # world = addJSFromUrl tonicShapes (Just (createEditletEventHandler onLibLoaded pid)) world
    = (val, mst, world)

  onLibLoaded pid evt val mst world
    # (jgrph, world) = jsNewObject "joint.dia.Graph" [] world
    # (div, world)   = callFunction "$" [toJSArg ("#" +++ mkPaperId pid)] world
    # (paper, world) = jsNewObject "joint.dia.Paper"
                         [toJSArg {PaperArgs
                                  | el       = div
                                  , width    = 600
                                  , height   = 300
                                  , gridSize = 1
                                  , model    = jgrph
                                  }] world
    # world          = drawTonicGraph val jgrph world
    = (val, mst, world)

  onUpdate pid Nothing val mst world
    # (joint, world) = findObject "joint" world
    | jsIsUndefined joint
        # world = loadJointJSLib pid world
        = (val, Nothing, world)
    | otherwise
        = onLibLoaded pid Nothing val mst world

  onUpdate pid (Just diff) val mst world
    = (val, mst, world)

  genDiff _ _ = Nothing
  appDiff _ val = val

drawTonicGraph g jgrph world
  # (_, world)     = foldrNodes addNode (jgrph, world) g
  # (_, world)     = foldrEdges addEdge (jgrph, world) g
  = layoutGraph jgrph world
  where
  addNode ni node (jgrph, world)
    = case node.data.nodeType of
        GAssign expr // TODO: Create assign node
          = (jgrph, world)

        GDecision dt expr
          # (dec, world) = jsNewObject "joint.shapes.tonic.DecisionState" [] world // TODO: Fill let state
          # world        = addCell (Just ni) dec jgrph world
          = (jgrph, world)

        GLet lt
          # (ltst, world) = jsNewObject "joint.shapes.tonic.LetState" [] world // TODO: Fill let state
          # world          = addCell (Just ni) ltst jgrph world
          = (jgrph, world)

        GInit
          # (start, world) = jsNewObject "joint.shapes.tonic.StartState" [] world
          # world          = addCell (Just ni) start jgrph world
          = (jgrph, world)

        GListComprehension lc
          = (jgrph, world)

        GParallelSplit
          = (jgrph, world)

        GParallelJoin jt
          = (jgrph, world)

        GReturn expr
          # args = { ReturnStateArgs
                   | size  = {Size | width = 50, height = 50}
                   , attrs = { Attrs
                             | text = { TextAttrs
                             | text = "TODO: expr :: GExpression" } }
                   }
          # (ret, world) = jsNewObject "joint.shapes.tonic.Return" [toJSArg args] world
          # world        = addCell (Just ni) ret jgrph world
          = (jgrph, world)

        GStep
          = (jgrph, world)

        GStop
          # (stop, world) = jsNewObject "joint.shapes.tonic.StopState" [] world
          # world         = addCell (Just ni) stop jgrph world
          = (jgrph, world)

        GTaskApp ident exprs
          # args         = { TaskAppArgs
                           | size = {Size | width = 100, height = 100}
                           , name = ident
                           }
                           // TODO: Add exprs
          # (app, world) = jsNewObject "joint.shapes.tonic.TaskApp" [toJSArg args] world
          # world        = addCell (Just ni) app jgrph world
          = (jgrph, world)
        _ = (jgrph, world)
  addEdge (fromNode, toNode) {edge_pattern} (jgrph, world)
    # world = mkBind fromNode toNode edge_pattern jgrph world
    = (jgrph, world)

addCell :: (Maybe Int) (JSVal o) (JSVal g) *JSWorld -> *JSWorld
addCell mnIdx cell jgrph world
  # world      = case mnIdx of
                   Just idx
                     = jsSetObjectAttr "id" (toJSVal idx) cell world
                   _ = world
  # (_, world) = callObjectMethod "addCell" [toJSArg cell] jgrph world
  = world

mkBind :: Int Int (Maybe String) (JSVal g) *JSWorld -> *JSWorld
mkBind sid tid mtxt jgrph world
  # args          = {ArrowArgs | source = {Identifier | id = toString sid}
                               , target = {Identifier | id = toString tid}
                               , labels = mkLabels mtxt}
  # (bind, world) = jsNewObject "joint.shapes.tonic.Bind" [toJSArg args] world
  = addCell Nothing bind jgrph world
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

:: TaskAppArgs =
  { //position :: Point
    size     :: Size
  , name     :: String
  }
/*
:: StartStateArgs =
  { position :: Point
  }

:: StopStateArgs =
  { position :: Point
  }
  */
:: ReturnStateArgs =
  { //position :: Point
    size     :: Size
  , attrs    :: Attrs
  }
