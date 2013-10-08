implementation module iTasks.API.Extensions.JointJS.JointJS

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import StdMisc, StdDebug

derive class iTask JointJS, JointJSDiff

PaperId :== "paper"

jointJSEditlet :: JointJS -> Editlet JointJS [JointJSDiff]
jointJSEditlet jjs =
	{ Editlet
	| value    = jjs
	, html     = \id -> DivTag [IdAttr PaperId, ClassAttr PaperId, StyleAttr "width: 300px; height: 300px"] []
	, updateUI = onUpdate
	, handlers = \_ -> []
	, genDiff  = genDiff
	, appDiff  = appDiff
	}
	where
    loadJointJSLib cid world
   		# world = addJSFromUrl "/joint/dist/joint.all.js" (Just (createEditletEventHandler onLibLoaded cid)) world
		# world = addCSSFromUrl "/joint/dist/joint.all.css" world
        = world
                    
    onLibLoaded cid evt val mst world
		# (graph, world) = jsNewObject "joint.dia.Graph" [] world
		# (div, world)   = callFunction "$" [toJSArg PaperId] world
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
		= (val, Just JointJS, world)
		
	onUpdate cid Nothing val mst world
 		# (joint, world) = findObject "joint" world
		| jsIsUndefined joint
            # world = loadJointJSLib cid world
            = (val, Nothing, world)
		| otherwise
			= onLibLoaded cid Nothing val mst world
		
	onUpdate cid (Just diff) val mst world
		= (val, mst, world)
	
	genDiff _ _ = Nothing
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