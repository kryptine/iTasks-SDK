implementation module iTasks.API.Extensions.JointJS.JointJS

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import StdMisc, StdDebug

derive class iTask JointJS, JointJSDiff

mkPaperId :: String -> String
mkPaperId x = "paper" +++ x

jointJSEditlet :: JointJS -> Editlet JointJS [JointJSDiff]
jointJSEditlet jjs =
	{ Editlet
	| value    = jjs
	, html     = \pid -> DivTag [IdAttr (mkPaperId pid), ClassAttr (mkPaperId pid), StyleAttr "width: 300px; height: 300px"] []
	, updateUI = onUpdate
	, handlers = \_ -> []
	, genDiff  = genDiff
	, appDiff  = appDiff
	}
	where
    loadJointJSLib pid world
   		# world = addJSFromUrl "/joint/dist/joint.all.js" (Just (createEditletEventHandler onLibLoaded pid)) world
		# world = addCSSFromUrl "/joint/dist/joint.all.css" world
        = world
                    
    onLibLoaded pid evt val mst world
		# (graph, world) = jsNewObject "joint.dia.Graph" [] world
		# (div, world)   = getDomElement (mkPaperId pid) world
		//# (div, world)   = callFunction "$" [toJSArg (mkPaperId pid)] world
		# (paper, world) = jsNewObject "joint.dia.Paper"
								[toJSArg {PaperArgs
										 | el       = div
										 , width    = 300
										 , height   = 300
										 , gridSize = 1
										 , model    = graph
										 }] world
		# (start, world) = jsNewObject "joint.shapes.fsa.StartState"
								[toJSArg {StartArgs|position={Point | x = 100, y = 100}}] world
		# (_, world) 	 = callObjectMethod "addCell" [toJSArg start] graph world
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

:: El = El
:: Graph = Graph

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

:: StartArgs =
	{ position :: Point
	}