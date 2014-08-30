module SharedPaint

import iTasks
import iTasks.API.Core.Client.Tasklet
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
from StdArray import class Array(uselect), instance Array {} a

:: PainterState = 	{ tool 		:: String
					, color 	:: String
					, mouseDown :: Maybe (Int, Int)
					, lastDraw  :: Maybe DrawType
					, draw		:: ![DrawType]
					, finished  :: Bool				
					}

:: DrawType = DrawLine !String !Int !Int !Int !Int
		    | DrawRect !String !Bool !Int !Int !Int !Int
		    | DrawCircle !String !Bool !Int !Int !Int !Int

:: Drawing = Drawing [DrawType]

derive class iTask PainterState, DrawType, Drawing

info = "Draw something, but use the pencil _slowly_ in Chrome!" 

painterEditlet :: [DrawType] -> Editlet Drawing [DrawType]
painterEditlet ds = Editlet (Drawing ds) serverDef clientDef
where
	serverDef =
	  {  EditletServerDef
	  |  genUI   = painterGenerateGUI
	  ,  defVal  = Drawing []
	  ,  genDiff = srvGenDiff
	  ,  appDiff = \ns (Drawing ds) -> Drawing (ds ++ ns)
	  }

	clientDef =
	  {  EditletClientDef
	  |  updateUI = updateUI
	  ,  defVal   = {tool = "P", color = "black", mouseDown = Nothing, draw = [], lastDraw = Nothing, finished = False}
	  ,  genDiff  = cltGenDiff
	  ,  appDiff  = \ds cl -> {cl & draw = cl.draw ++ ds}
	  }

	srvGenDiff (Drawing ds1) (Drawing ds2)
		| lds1 == lds2
			= Nothing
			= Just (drop lds1 ds2)
	where
		lds1 = length ds1
		lds2 = length ds2

	cltGenDiff cl1 cl2
		| lds1 == lds2
			= Nothing
			= Just (drop lds1 cl2.draw)
	where
		lds1 = length cl1.draw
		lds2 = length cl2.draw

	updateUI cid Nothing cl world = (cl, world)
	updateUI cid (Just ds) cl world
		# (context, world) = getContext cid False world
		# world = foldl (\world dr = draw context dr world) world (reverse cl.draw)
		= (cl, world)	

getCanvas cid temp world
	= case temp of
		True = getDomElement ("tempcanvas_"+++cid) world
		_	 = getDomElement ("canvas_"+++cid) world

getContext cid temp world
 	# (canvas, world) = getCanvas cid temp world
	# (context, world) = callObjectMethod "getContext" [toJSArg "2d"] canvas world // not "2D" !
	= (context, world)

clearContext context world
	# (_, world) = callObjectMethod "clearRect" 
						[toJSArg 0, toJSArg 0, toJSArg canvasWidth, toJSArg canvasHeight] context world
	= world
			
drawLine context color x1 y1 x2 y2 world
	# (_, world) = callObjectMethod "beginPath" [] context world
	# world	     = jsSetObjectAttr "strokeStyle" (toJSVal color) context world
	# (_, world) = callObjectMethod "moveTo" [toJSArg x1, toJSArg y1] context world
	# (_, world) = callObjectMethod "lineTo" [toJSArg x2, toJSArg y2] context world
	# (_, world) = callObjectMethod "stroke" [] context world
	= world

drawRect context color x1 y1 x2 y2 world
	# world		 = jsSetObjectAttr "strokeStyle" (toJSVal color) context world
	# (_, world) = callObjectMethod "strokeRect" 
					[toJSArg x1, toJSArg y1, toJSArg (x2 - x1), toJSArg (y2 - y1)]
					context world
	= world

drawFilledRect context color x1 y1 x2 y2 world
	# world   	 = jsSetObjectAttr "fillStyle" (toJSVal color) context world
	# (_, world) = callObjectMethod "fillRect"
						[toJSArg x1, toJSArg y1, toJSArg (x2 - x1), toJSArg (y2 - y1)]
						context world
	= world

drawCircle context fill color x1 y1 x2 y2 world
	# (_, world) = callObjectMethod "beginPath" [] context world
	# world		 = jsSetObjectAttr "strokeStyle" (toJSVal color) context world
	# world		 = jsSetObjectAttr "fillStyle" (toJSVal color) context world
	
	# (_, world) = callObjectMethod "arc"
						[toJSArg (center x1 x2), toJSArg (center y1 y2)
						,toJSArg (toInt ((distance x1 y1 x2 y2)/2.0))
						,toJSArg 0, toJSArg (3.14159265*2.0), toJSArg True]
						context world
							
	# (_, world) = case fill of 
					True = callObjectMethod "fill" [] context world
					_	 = callObjectMethod "stroke" [] context world

	# (_, world) = callObjectMethod "closePath" [] context world
	= world
where
	center x1 x2 = (max x1 x2) - (abs (x1 - x2))/2
	distance x1 y1 x2 y2 = sqrt (toReal ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)))
				
draw context (DrawLine color x1 y1 x2 y2) world
	= drawLine context color x1 y1 x2 y2 world
draw context (DrawRect color False x1 y1 x2 y2) world
	= drawRect context color x1 y1 x2 y2 world
draw context (DrawRect color True x1 y1 x2 y2) world
	= drawFilledRect context color x1 y1 x2 y2 world
draw context (DrawCircle color fill x1 y1 x2 y2) world
	= drawCircle context fill color x1 y1 x2 y2 world

canvasWidth :== 300
canvasHeight :== 300

// TODO: http://jaspervdj.be/blaze/tutorial.html

painterGenerateGUI cid world  

	# ws = toString canvasWidth
	# hs = toString canvasHeight
	
	# canvas = 
			DivTag [StyleAttr "position: relative; float: left;"] [
					CanvasTag [IdAttr ("canvas_"+++cid), WidthAttr ws, HeightAttr hs, StyleAttr "border: 1px solid #000;"] [],
					CanvasTag [IdAttr ("tempcanvas_"+++cid), WidthAttr ws, HeightAttr hs, StyleAttr "position: absolute; top: 1px; left: 1px;"] []
					]

	# editor = [
			DivTag [StyleAttr "float: right;"] [
					DivTag [IdAttr ("selectorYellow_"+++cid), StyleAttr "border-style:solid; border-color:white; background-color:yellow; width: 40px; height:40px; margin: 5px;"] [],
					DivTag [IdAttr ("selectorRed_"+++cid), StyleAttr "border-style:solid; border-color:white; background-color:red; width: 40px; height: 40px; margin: 5px;"] [],
					DivTag [IdAttr ("selectorGreen_"+++cid), StyleAttr "border-style:solid; border-color:white; background-color:green; width: 40px; height: 40px; margin: 5px;"] [],
					DivTag [IdAttr ("selectorBlue_"+++cid), StyleAttr "border-style:solid; border-color:white; background-color:blue; width: 40px; height: 40px; margin: 5px;"] [],
					DivTag [IdAttr ("selectorBlack_"+++cid), StyleAttr "border-style:solid; border-color:pink; background-color:black; width: 40px; height: 40px; margin: 5px;"] []
					],
			DivTag [StyleAttr "clear: both"] [],
			DivTag [] ([
					Text "Tool: ",
					SelectTag [IdAttr ("selecttool_"+++cid)] [
						OptionTag [ValueAttr "P"] [Text "Pencil"],
						OptionTag [ValueAttr "L"] [Text "Line"],
						OptionTag [ValueAttr "R"] [Text "Rectangle"],
						OptionTag [ValueAttr "r"] [Text "Rectangle (filled)"],
						OptionTag [ValueAttr "C"] [Text "Circle"],
						OptionTag [ValueAttr "c"] [Text "Circle (filled)"]]
					])
			]
					
	# html = DivTag [StyleAttr "width: 360px; margin-right: auto;"] [canvas:editor]		

	# eventHandlers = [
			ComponentEvent ("selectorYellow_"+++cid) "click" (onSelectColor "yellow"),
			ComponentEvent ("selectorRed_"+++cid) "click" (onSelectColor "red"),
			ComponentEvent ("selectorGreen_"+++cid) "click" (onSelectColor "green"),
			ComponentEvent ("selectorBlue_"+++cid) "click" (onSelectColor "blue"),
			ComponentEvent ("selectorBlack_"+++cid) "click" (onSelectColor "black"),
			ComponentEvent ("canvas_"+++cid) "mousedown" onMouseDown,
			ComponentEvent ("tempcanvas_"+++cid) "mousedown" onMouseDown,			
			ComponentEvent ("canvas_"+++cid) "mouseup" onMouseUp,
			ComponentEvent ("tempcanvas_"+++cid) "mouseup" onMouseUp,			
			ComponentEvent ("canvas_"+++cid) "mousemove" onMouseMove,
			ComponentEvent ("tempcanvas_"+++cid) "mousemove" onMouseMove,
			ComponentEvent ("selecttool_"+++cid) "change" onChangeTool]

	# gui = { width  		= ExactSize (canvasWidth + 70)
			, height 		= ExactSize (canvasHeight + 50)
			, html   		= html
			, eventHandlers = eventHandlers
			}
			
	= (gui, world)
where
	onChangeTool _ {[0]=e} state world
		# (selectedIndex, world) = jsGetObjectAttr "target.selectedIndex" e world
		# (options, world) 		 = jsGetObjectAttr "target.options" e world		
		# (option, world) 		 = jsGetObjectEl (jsValToInt selectedIndex) options world
		# (atool, world) 		 = jsGetObjectAttr "value" option world
		= ({state & tool = jsValToString atool}, world)	

	onSelectColor color _ {[0]=e} state world
		# world = foldl (\world el = setDomAttr el "style.borderColor" (toJSVal "white") world) world
					["selectorYellow_"+++cid,"selectorRed_"+++cid,"selectorGreen_"+++cid,"selectorBlue_"+++cid,"selectorBlack_"+++cid]
		# (target, world) = jsGetObjectAttr "target" e world
		# world = jsSetObjectAttr "style.borderColor" (toJSVal "pink") target world
		= ({state & color = color}, world)

	getCoordinates e world
	    # (x, world) = jsGetObjectAttr "layerX" e world
	    # (y, world) = jsGetObjectAttr "layerY" e world
	    = ((jsValToInt x, jsValToInt y), e, world)

	onMouseDown _ {[0]=e} state world
	    # (coords, e, world) = getCoordinates e world
		= ({state & mouseDown = Just coords, lastDraw = Nothing}, world)

	onMouseUp _ {[0]=e} state world
		# (tempcanvas, world)  	= getCanvas cid True world
		# (tempcontext, world) 	= getContext cid True world
		# (context, world)     	= getContext cid False world
		# (_, world)  		  	= callObjectMethod "drawImage" [toJSArg tempcanvas, toJSArg 0, toJSArg 0] context world
		# world 			   	= clearContext tempcontext world
		| isJust state.lastDraw
			= ({state & mouseDown = Nothing, draw = [fromJust state.lastDraw:state.draw], lastDraw = Nothing}, world)
			= ({state & mouseDown = Nothing}, world)

	// generate onDrawing event
	onMouseMove _ {[0]=e} state world
		= case state.mouseDown of
			Just coord = onDrawing coord e state world
			_          = (state, world)

	onDrawing (dx,dy) e state world
    	# ((x, y), e, world) = getCoordinates e world
		# (tempcontext, world) = getContext cid True world
			
		// Don't clear for pencil
		# world = case state.tool of 
				"P" = world
				    = clearContext tempcontext world

		# drawType = case state.tool of	
				"P" = DrawLine state.color dx dy x y
				"L"	= DrawLine state.color dx dy x y
				"R"	= DrawRect state.color False dx dy x y
				"r"	= DrawRect state.color True dx dy x y
				"C"	= DrawCircle state.color False dx dy x y
				"c"	= DrawCircle state.color True dx dy x y

		# world = draw tempcontext drawType world
			
		// Update start coordinates for pencil
		= case state.tool of 
				"P" = ({state & mouseDown = Just (x,y), draw=[drawType:state.draw], lastDraw = Nothing}, world)
				_   = ({state & lastDraw = Just drawType}, world)

//-------------------------------------------------------------------------

taskletExamples :: [Workflow]
taskletExamples =
	[workflow "Painter tasklet" "Simple painter tasklet" editlet2]

//editlet2 :: Task Drawing
editlet2 = withShared (painterEditlet []) (\smap -> updateSharedInformation "1" [] smap 
																-|| 
													updateSharedInformation "2" [] smap )

editlet = updateInformation "Painter" [] (painterEditlet [DrawLine "red" 1 1 100 100])
		>>= \ds -> viewInformation "Drawing" [] ds
		    
Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist taskletExamples)) world
