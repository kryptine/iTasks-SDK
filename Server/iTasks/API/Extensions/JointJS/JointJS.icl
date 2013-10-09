implementation module iTasks.API.Extensions.JointJS.JointJS

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import StdMisc

derive class iTask JointJS, JointJSDiff

jointDotJS :== "/joint/dist/joint.all.js"
jointDotCSS :== "/joint/dist/joint.all.css"
tonicShapes :== "/joint/plugins/joint.shapes.tonic.js"

mkPaperId :: String -> String
mkPaperId x = "paper" +++ x

jointJSEditlet :: JointJS -> Editlet JointJS [JointJSDiff]
jointJSEditlet jjs =
	{ Editlet
	| value    = jjs
	, html     = \pid -> DivTag [IdAttr (mkPaperId pid), ClassAttr (mkPaperId pid)] []
	, updateUI = onUpdate
	, handlers = \_ -> []
	, genDiff  = genDiff
	, appDiff  = appDiff
	}
	where
    loadJointJSLib pid world
   		# world = addJSFromUrl jointDotJS (Just (createEditletEventHandler loadPlugins pid)) world
		# world = addCSSFromUrl jointDotCSS world
        = world

	loadPlugins pid evt val mst world        
   		# world = addJSFromUrl tonicShapes (Just (createEditletEventHandler onLibLoaded pid)) world
   		= (val, mst, world)
                    
    onLibLoaded pid evt val mst world
		# (graph, world) = jsNewObject "joint.dia.Graph" [] world
		# (div, world)   = callFunction "$" [toJSArg ("#" +++ mkPaperId pid)] world
		# (paper, world) = jsNewObject "joint.dia.Paper"
								[toJSArg {PaperArgs
										 | el       = div
										 , width    = 600
										 , height   = 300
										 , gridSize = 1
										 , model    = graph
										 }] world
		# (start, world) = mkStartState world
		# world          = addCell start graph world
		# (task1, world) = mkTaskApp { TaskAppArgs
									 | size = {Size | width = 100, height = 100}
									 , name = "task1"
									 } world
		# world          = addCell task1 graph world
		# (task2, world) = mkTaskApp { TaskAppArgs
									 | size = {Size | width = 100, height = 100}
									 , name = "task2"
									 } world
		# world          = addCell task2 graph world
		# (ret, world)   = mkReturnState { ReturnStateArgs
										 | size = {Size | width = 50, height = 50}
										 , attrs =  { Attrs
													| text = { TextAttrs
												 			 | text = "x + y" } }
										 } world
		# world          = addCell ret graph world
		# (stop, world)  = mkStopState world
		# world          = addCell stop graph world
		# world          = mkBind start task1 Nothing graph world
		# world          = mkBind task1 task2 (Just "x") graph world
		# world          = mkBind task2 ret (Just "y") graph world
		# world          = mkBind ret stop Nothing graph world
		# world          = layoutGraph graph world
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
	
	genDiff _ _ = Just []
	appDiff _ val = val

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
addCell cell graph world
	# (_, world) = callObjectMethod "addCell" [toJSArg cell] graph world
	= world

mkBind :: (JSVal l) (JSVal r) (Maybe String) (JSVal g) *JSWorld -> *JSWorld
mkBind source target mtxt graph world
	# (sid, world)  = jsGetObjectAttr "id" source world
	# (tid, world)  = jsGetObjectAttr "id" target world
	# args          = {ArrowArgs | source = {Identifier | id = jsValToString sid}
								 , target = {Identifier | id = jsValToString tid}
								 , labels = mkLabels mtxt}
	# (bind, world) = jsNewObject "joint.shapes.tonic.Bind" [toJSArg args] world
	= addCell bind graph world
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

:: El = El
:: Graph = Graph

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

:: PaperArgs =
	{ el       :: JSVal El
	, width    :: Int
	, height   :: Int
	, gridSize :: Int
	, model    :: JSVal Graph
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