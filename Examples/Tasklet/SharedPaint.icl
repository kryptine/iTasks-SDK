module SharedPaint

// On the client, the list of drawings is reversed for efficient appending
// Because of this, there is the trick with reverse at appClientDiff, genClientDiff and updateUI

//import iTasks
import iTasks.API.Core.Client.Tasklet
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
from StdArray import class Array(uselect), instance Array {} a

import Text

:: PainterState = 	{ tool 		:: String
					, color 	:: String
					, mouseDown :: Maybe (Int, Int)
					, lastDraw  :: Maybe DrawType
					, draw		:: ![DrawType]
					}

:: DrawType = DrawLine !String !Int !Int !Int !Int
		    | DrawRect !String !Bool !Int !Int !Int !Int
		    | DrawCircle !String !Bool !Int !Int !Int !Int

:: Drawing = Drawing [DrawType]

instance toString DrawType
where
	toString (DrawLine color a b c d) = "DrawLine " +++ color +++ " " +++toString a +++ " " +++ toString b +++ " " +++ toString c +++ " " +++ toString d 
	toString (DrawRect color filled a b c d) = "DrawRect " +++ color +++ " " +++ toString filled +++ " " +++ toString a +++ " " +++ toString b +++ " " +++ toString c +++ " " +++ toString d 
	toString (DrawCircle color filled a b c d) = "DrawCircle " +++ color +++ " " +++ toString filled +++ " " +++ toString a +++ " " +++ toString b +++ " " +++ toString c +++ " " +++ toString d 

dumpDrawing :: [DrawType] -> String
dumpDrawing drawing = join "," (map toString drawing)

derive class iTask PainterState, DrawType, Drawing

info = "Draw something, but use the pencil _slowly_ in Chrome!" 

painterEditlet :: [DrawType] -> Editlet Drawing [DrawType]
painterEditlet ds
  = { Editlet
    | currVal   = Drawing ds
    , genUI     = painterGenerateGUI
    , serverDef = serverDef
    , clientDef = clientDef
    }
where
	serverDef =
	  {  EditletDef
	  |  performIO = \_ _ s w -> (s, w)
      ,  defVal    = Drawing []
	  ,  genDiff   = srvGenDiff
	  ,  appDiff   = \ns (Drawing ds) -> Drawing (ds ++ ns)
	  }

	clientDef =
	  {  EditletDef
	  |  performIO = updateUI
	  ,  defVal    = {tool = "L", color = "black", mouseDown = Nothing, draw = [], lastDraw = Nothing}
	  ,  genDiff   = cltGenDiff
	  ,  appDiff   = \ds cl -> {cl & draw = reverse ds ++ cl.draw}
	  }

	srvGenDiff (Drawing ds1) (Drawing ds2)
		| lds1 == lds2
			= Nothing
			= trace_n ("l: " +++ toString lds1 +++ " r: " +++ toString lds2) Just (drop lds1 ds2)
	where
		lds1 = length ds1
		lds2 = length ds2

	cltGenDiff cl1 cl2
		| lds1 == lds2
			= Nothing
			= Just (reverse (take (lds2-lds1) cl2.draw))
	where
		lds1 = length cl1.draw
		lds2 = length cl2.draw

	updateUI cid Nothing cl world = (cl, world)
	updateUI cid (Just ds) cl world
		# (context, world) = getContext cid False world
		# world = foldl (\world dr = draw context dr world) world (reverse cl.draw)
		= (cl, world)	

:: JSCanvas = JSCanvas
:: JSCanvasContext = JSCanvasContext
:: Canvas :== JSVal JSCanvas
:: Context :== JSVal JSCanvasContext

:: Color :== String

getCanvas :: ComponentId Bool *JSWorld -> *(Canvas, *JSWorld)
getCanvas cid True  world = .? (getElementById ("tempcanvas_"+++cid)) world
getCanvas cid False world = .? (getElementById ("canvas_"+++cid)) world

getContext :: ComponentId Bool *JSWorld -> *(Context, *JSWorld)
getContext cid temp world
 	# (canvas, world) = getCanvas cid temp world
	= (canvas .# "getContext" .$ ("2d")) world // not "2D" !

clearCanvas :: Context *JSWorld -> *JSWorld
clearCanvas context world
	= (context .# "clearRect" .$! (0, 0, canvasWidth, canvasHeight)) world
			
drawLine :: Context Color Int Int Int Int *JSWorld -> *JSWorld			
drawLine context color x1 y1 x2 y2 world
	# world = (context .# "beginPath" .$! ()     ) world
	# world = (context .# "strokeStyle" .= color ) world
	# world = (context .# "moveTo" .$! (x1, y1)  ) world
	# world = (context .# "lineTo" .$! (x2, y2)  ) world
	# world = (context .# "stroke" .$! ()        ) world
	= world

drawRect :: Context Color Int Int Int Int *JSWorld -> *JSWorld
drawRect context color x1 y1 x2 y2 world
	# world = (context .# "strokeStyle" .= color                     ) world
	# world = (context .# "strokeRect" .$! (x1, y1, x2 - x1, y2 - y1)) world
	= world

drawFilledRect :: Context Color Int Int Int Int *JSWorld -> *JSWorld
drawFilledRect context color x1 y1 x2 y2 world
	# world = (context .# "fillStyle" .= color) world
	# world = (context .# "fillRect" .$! (x1, y1, x2 - x1, y2 - y1)) world
	= world

drawCircle :: Context Bool Color Int Int Int Int *JSWorld -> *JSWorld
drawCircle context fill color x1 y1 x2 y2 world
	# world = (context .# "beginPath" .$! ()      ) world
	# world = (context .# "strokeStyle" .= color  ) world
	# world = (context .# "fillStyle" .= color    ) world
	
	# world = (context .# "arc" .$! (center x1 x2, 
									 center y1 y2, 
									 toInt ((distance x1 y1 x2 y2)/2.0),
									 0, 
									 3.14159265*2.0, 
									 True)) world
							
	# world  = case fill of 
					True = (context .# "fill" .$! ()) world
					_	 = (context .# "stroke" .$! ()) world

	= (context .# "closePath" .$! ()) world
where
	center x1 x2 = (max x1 x2) - (abs (x1 - x2))/2
	distance x1 y1 x2 y2 = sqrt (toReal ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)))
				
draw :: Context DrawType *JSWorld -> *JSWorld
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
// http://stackoverflow.com/questions/18201257/drawing-a-rectangle-without-clearing-the-canvas-eventlisteners

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
			DivTag [] [
					Text "Tool: ",
					SelectTag [IdAttr ("selecttool_"+++cid)] [
						OptionTag [ValueAttr "L"] [Text "Line"],
						OptionTag [ValueAttr "R"] [Text "Rectangle"],
						OptionTag [ValueAttr "r"] [Text "Rectangle (filled)"],
						OptionTag [ValueAttr "C"] [Text "Circle"],
						OptionTag [ValueAttr "c"] [Text "Circle (filled)"]]
					]
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
	onChangeTool :: ComponentId {JSObj JSEvent} PainterState *JSWorld -> *(!PainterState, !*JSWorld)
	onChangeTool _ {[0]=e} state world
		# (selectedIndex, world) = .? (e .# "target" .# "selectedIndex") world
		# (options, world) 		 = .? (e .# "target" .# "options") world		
		# (tool, world) 		 = .? (options .# jsValToInt selectedIndex .# "value") world
		= ({state & tool = jsValToString tool}, world)	

	onSelectColor :: Color ComponentId {JSObj JSEvent} PainterState *JSWorld -> *(!PainterState, !*JSWorld)
	onSelectColor color _ {[0]=e} state world
		# world = foldr clearBorder world
					["selectorYellow_"+++cid,"selectorRed_"+++cid,"selectorGreen_"+++cid,"selectorBlue_"+++cid,"selectorBlack_"+++cid]
		# world = (e .# "target" .# "style" .# "borderColor" .= "pink") world
		= ({state & color = color}, world)
	where
		clearBorder el world
			= (getElementById el .# "style" .# "borderColor" .= "white") world

	getCoordinates e world
	    # (x, world) = .? (e .# "layerX") world
	    # (y, world) = .? (e .# "layerY") world
	    = ((jsValToInt x, jsValToInt y), e, world)

	onMouseDown _ {[0]=e} state world
	    # (coords, e, world) = getCoordinates e world
		= ({state & mouseDown = Just coords, lastDraw = Nothing}, world)

	onMouseUp _ {[0]=e} state world
		# (tempcanvas, world)  	= getCanvas cid True world
		# (tempcontext, world) 	= getContext cid True world
		# (context, world)     	= getContext cid False world
		# world  		  		= (context .# "drawImage" .$! (tempcanvas, 0, 0)) world
		# world 			   	= clearCanvas tempcontext world
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
			
		# world = clearCanvas tempcontext world

		# drawType = case state.tool of	
				"L"	= DrawLine state.color dx dy x y
				"R"	= DrawRect state.color False dx dy x y
				"r"	= DrawRect state.color True dx dy x y
				"C"	= DrawCircle state.color False dx dy x y
				"c"	= DrawCircle state.color True dx dy x y

		# world = draw tempcontext drawType world
			
		= ({state & lastDraw = Just drawType}, world)

//-------------------------------------------------------------------------

import StdMisc, StdDebug

taskletExamples :: [Workflow]
taskletExamples =
	[workflow "Painter tasklet" "Simple painter tasklet" editlet2]

tracelength ds1 ds2 c = trace_n ("l: "+++toString (length ds1)+++", r: "+++toString (length ds2))c

/*
shareditlet name drawing = updateSharedInformation name 
								[UpdateWith (\(Drawing ds) -> painterEditlet ds)
								            (\_ (Editlet value _ _) -> value)] drawing

editlet2 :: Task Drawing
editlet2 = withShared (Drawing []) (\drawing -> shareditlet "1" drawing
													-|| 
									            shareditlet "2" drawing)
*/

shareditlet name editlet = updateSharedInformation name [] editlet								

editlet2 :: Task (Editlet Drawing [DrawType])
editlet2 = withShared (painterEditlet []) (\editlet -> shareditlet "1" editlet
													-|| 
									                   shareditlet "2" editlet)


//editlet = updateInformation "Painter" [] (painterEditlet [DrawLine "red" 1 1 100 100])
//		>>= \ds -> viewInformation "Drawing" [] ds
		    
Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist taskletExamples)) world
