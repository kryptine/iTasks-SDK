implementation module iTasks.Framework.Tonic.Editlet

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import iTasks.Framework.Tonic.AbsSyn
import StdMisc

jointDotJS :== "/joint/dist/joint.all.js"
jointDotCSS :== "/joint/dist/joint.all.css"
tonicShapes :== "/joint/plugins/joint.shapes.tonic.js"

mkPaperId :: String -> String
mkPaperId x = "paper" +++ x

toniclet :: GinGraph -> Editlet GinGraph GinGraph
toniclet g = Editlet g
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
    # (start, world) = mkStartState world
    # (stop, world)  = mkStopState world

    # world          = addCell stop jgrph world
    # world          = addCell start jgrph world
    # (task1, world) = mkTaskApp { TaskAppArgs
                   | size = {Size | width = 100, height = 100}
                   , name = "task1"
                   } world
    # world          = addCell task1 jgrph world
    # (task2, world) = mkTaskApp { TaskAppArgs
                   | size = {Size | width = 100, height = 100}
                   , name = "task2"
                   } world
    # world          = addCell task2 jgrph world
    # (ret, world)   = mkReturnState { ReturnStateArgs
                     | size = {Size | width = 50, height = 50}
                     , attrs =  { Attrs
                          | text = { TextAttrs
                                | text = "x + y" } }
                     } world
    # world          = addCell ret jgrph world
    # world          = mkBind start task1 Nothing jgrph world
    # world          = mkBind task1 task2 (Just "x") jgrph world
    # world          = mkBind task2 ret (Just "y") jgrph world
    # world          = mkBind ret stop Nothing jgrph world
    # world          = layoutGraph jgrph world
    = world

mkTaskApp :: TaskAppArgs *JSWorld -> *(JSVal o, *JSWorld)
mkTaskApp args world
  = jsNewObject "joint.shapes.tonic.TaskApp" [toJSArg args] world

mkStartState :: *JSWorld -> *(JSVal o, *JSWorld)
mkStartState world
  = jsNewObject "joint.shapes.tonic.StartState" [] world

mkStopState :: *JSWorld -> *(JSVal o, *JSWorld)
mkStopState world
  = jsNewObject "joint.shapes.tonic.StopState" [] world

mkReturnState :: ReturnStateArgs *JSWorld -> *(JSVal o, *JSWorld)
mkReturnState args world
  = jsNewObject "joint.shapes.tonic.Return" [toJSArg args] world

addCell :: (JSVal o) (JSVal g) *JSWorld -> *JSWorld
addCell cell jgrph world
  # (_, world) = callObjectMethod "addCell" [toJSArg cell] jgrph world
  = world

mkBind :: (JSVal l) (JSVal r) (Maybe String) (JSVal g) *JSWorld -> *JSWorld
mkBind source target mtxt jgrph world
  # (sid, world)  = jsGetObjectAttr "id" source world
  # (tid, world)  = jsGetObjectAttr "id" target world
  # args          = {ArrowArgs | source = {Identifier | id = jsValToString sid}
                 , target = {Identifier | id = jsValToString tid}
                 , labels = mkLabels mtxt}
  # (bind, world) = jsNewObject "joint.shapes.tonic.Bind" [toJSArg args] world
  = addCell bind jgrph world
  where
  mkLabels Nothing    = []
  mkLabels (Just lbl) = [ {LabelArgs
              | position = 0.5
              , attrs =   { Attrs
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
