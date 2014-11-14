module SharedPaint

// On the client, the list of drawings is reversed for efficient appending
// Because of this, there is the trick with reverse at appClientDiff, genClientDiff and updateUI

//import iTasks
import iTasks.API.Core.Client.Tasklet
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
from StdArray import class Array(uselect), instance Array {} a

import Text

:: Tool   :== String
:: Color  :== String

:: PainterState = 	{ selectedTool  :: Tool
					, selectedColor :: Color
					, currentOrigin :: Maybe (Int, Int)
					, currentShape  :: Maybe Shape
					, shapes	    :: ![Shape]
					}

:: Filled :== Bool

:: Shape = Line !Color !Int !Int !Int !Int
		 | Rect !Color !Filled !Int !Int !Int !Int
		 | Circle !Color !Filled !Int !Int !Int !Int

:: Drawing = Drawing [Shape]

instance toString Shape
where
	toString (Line color a b c d) = "DrawLine " +++ color +++ " " +++toString a +++ " " +++ toString b +++ " " +++ toString c +++ " " +++ toString d 
	toString (Rect color filled a b c d) = "DrawRect " +++ color +++ " " +++ toString filled +++ " " +++ toString a +++ " " +++ toString b +++ " " +++ toString c +++ " " +++ toString d 
	toString (Circle color filled a b c d) = "DrawCircle " +++ color +++ " " +++ toString filled +++ " " +++ toString a +++ " " +++ toString b +++ " " +++ toString c +++ " " +++ toString d 

dumpDrawing :: [Shape] -> String
dumpDrawing drawing = join "," (map toString drawing)

derive class iTask PainterState, Shape, Drawing

info = "Draw something, but use the pencil _slowly_ in Chrome!" 

painterEditlet :: [Shape] -> Editlet Drawing [Shape]
painterEditlet ds
  = { Editlet
    | currVal   = Drawing ds
    , genUI     = painterGUI
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
	  ,  defVal    = {selectedTool = "L", selectedColor = "black", currentOrigin = Nothing, shapes = [], currentShape = Nothing}
	  ,  genDiff   = cltGenDiff
	  ,  appDiff   = \ds cl -> {cl & shapes = reverse ds ++ cl.shapes}
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
			= Just (reverse (take (lds2-lds1) cl2.shapes))
	where
		lds1 = length cl1.shapes
		lds2 = length cl2.shapes

	updateUI cid Nothing cl world = (cl, world)
	updateUI cid (Just ds) cl world
		# (context, world) = getContext cid False world
		# world = foldl (\world dr = draw context dr world) world (reverse cl.shapes)
		= (cl, world)	

:: JSCanvas = JSCanvas
:: JSCanvasContext = JSCanvasContext
:: Canvas :== JSVal JSCanvas
:: Context :== JSVal JSCanvasContext

getCanvas :: ComponentId Bool *JSWorld -> *(Canvas, *JSWorld)
getCanvas cid True  world = .? (getElementById ("tcanvas_"+++cid)) world
getCanvas cid False world = .? (getElementById ("pcanvas_"+++cid)) world

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

drawRect :: Context Color Filled Int Int Int Int *JSWorld -> *JSWorld
drawRect context color False x1 y1 x2 y2 world
	# world = (context .# "strokeStyle" .= color                     ) world
	# world = (context .# "strokeRect" .$! (x1, y1, x2 - x1, y2 - y1)) world
	= world

drawRect context color True x1 y1 x2 y2 world
	# world = (context .# "fillStyle" .= color					   ) world
	# world = (context .# "fillRect" .$! (x1, y1, x2 - x1, y2 - y1)) world
	= world

drawCircle :: Context Color Filled Int Int Int Int *JSWorld -> *JSWorld
drawCircle context color filled x1 y1 x2 y2 world
	# world = (context .# "beginPath" .$! ()      ) world
	# world = (context .# "strokeStyle" .= color  ) world
	# world = (context .# "fillStyle" .= color    ) world
	
	# world = (context .# "arc" .$! (center x1 x2, 
									 center y1 y2, 
									 toInt ((distance x1 y1 x2 y2)/2.0),
									 0, 
									 3.14159265*2.0, 
									 True)) world
							
	# world  = case filled of 
					True = (context .# "fill" .$! ()) world
					_	 = (context .# "stroke" .$! ()) world

	= (context .# "closePath" .$! ()) world
where
	center x1 x2 = (max x1 x2) - (abs (x1 - x2))/2
	distance x1 y1 x2 y2 = sqrt (toReal ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)))
				
draw :: Context Shape *JSWorld -> *JSWorld
draw context (Line color x1 y1 x2 y2) world
	= drawLine context color x1 y1 x2 y2 world
draw context (Rect color filled x1 y1 x2 y2) world
	= drawRect context color filled x1 y1 x2 y2 world
draw context (Circle color filled x1 y1 x2 y2) world
	= drawCircle context color filled x1 y1 x2 y2 world

canvasWidth :== 300
canvasHeight :== 300

// TODO: http://jaspervdj.be/blaze/tutorial.html
// http://stackoverflow.com/questions/18201257/drawing-a-rectangle-without-clearing-the-canvas-eventlisteners

painterGUI :: ComponentId *World -> *(EditletHTML PainterState, *World)
painterGUI cid world  

	# ws = toString canvasWidth
	# hs = toString canvasHeight
	
	# canvas = 
			DivTag [StyleAttr "position: relative; float: left;"] [
					CanvasTag [IdAttr ("pcanvas_"+++cid), WidthAttr ws, HeightAttr hs, StyleAttr "border: 1px solid #000;"] [],
					CanvasTag [IdAttr ("tcanvas_"+++cid), WidthAttr ws, HeightAttr hs, StyleAttr "position: absolute; top: 1px; left: 1px;"] []
					]

	# editor = [
			DivTag [StyleAttr "float: right;"] selectors,
			DivTag [StyleAttr "clear: both;"] [],
			DivTag [] [Text "Tool: ",SelectTag [IdAttr ("tool_"+++cid)] optionTags]
			]
					
	# html = DivTag [StyleAttr "width: 360px; margin-right: auto;"] [canvas:editor]		

	# eventHandlers = selectorEvents ++ [
			ComponentEvent ("pcanvas_"+++cid) "mousedown" onMouseDown,
			ComponentEvent ("tcanvas_"+++cid) "mousedown" onMouseDown,			
			ComponentEvent ("pcanvas_"+++cid) "mouseup" onMouseUp,
			ComponentEvent ("tcanvas_"+++cid) "mouseup" onMouseUp,			
			ComponentEvent ("cpanvas_"+++cid) "mousemove" onMouseMove,
			ComponentEvent ("tcanvas_"+++cid) "mousemove" onMouseMove,
			ComponentEvent ("tool_"+++cid) "change" onChangeTool]

	# gui = { width  		= ExactSize (canvasWidth + 70)
			, height 		= ExactSize (canvasHeight + 50)
			, html   		= html
			, eventHandlers = eventHandlers
			}
			
	= (gui, world)
where
	optionTags = map (\(id,label) -> OptionTag [ValueAttr id] [Text label])
		[("L","Line"), ("R","Rectangle"), ("r","Rectangle (filled)"), ("C","Circle"), ("c","Circle (filled)")]

	selectors = map (\color -> DivTag [IdAttr (mkSelector color), 
			StyleAttr ("border-style:solid; border-color:white; background-color:" +++ color +++ "; width: 40px; height:40px; margin: 5px;")] []) colors

	selectorEvents = map (\color -> ComponentEvent (mkSelector color) "click" (onSelectColor color)) colors

	colors = ["yellow", "red", "green", "blue", "black"]
	mkSelector color = "sel_" +++ color +++ "_" +++ cid
	
	onChangeTool :: ComponentId {JSObj JSEvent} PainterState *JSWorld -> *(!PainterState, !*JSWorld)
	onChangeTool _ {[0]=e} state world
		# (selectedIndex, world) = .? (e .# "target" .# "selectedIndex") world
		# (options, world) 		 = .? (e .# "target" .# "options") world		
		# (tool, world) 		 = .? (options .# jsValToInt selectedIndex .# "value") world
		= ({state & selectedTool = jsValToString tool}, world)	

	onSelectColor :: Color ComponentId {JSObj JSEvent} PainterState *JSWorld -> *(!PainterState, !*JSWorld)
	onSelectColor color _ {[0]=e} state world
		# world = foldr clearBorder world (map mkSelector colors)
		# world = (e .# "target" .# "style" .# "borderColor" .= "pink") world
		= ({state & selectedColor = color}, world)
	where
		clearBorder el world
			= (getElementById el .# "style" .# "borderColor" .= "white") world

	getCoordinates e world
	    # (x, world) = .? (e .# "layerX") world
	    # (y, world) = .? (e .# "layerY") world
	    = ((jsValToInt x, jsValToInt y), e, world)

	onMouseDown _ {[0]=e} state world
	    # (coords, e, world) = getCoordinates e world
		= ({state & currentOrigin = Just coords, currentShape = Nothing}, world)

	onMouseUp _ {[0]=e} state world
		# (tempcanvas, world)  	= getCanvas cid True world
		# (tempcontext, world) 	= getContext cid True world
		# (context, world)     	= getContext cid False world
		# world  		  		= (context .# "drawImage" .$! (tempcanvas, 0, 0)) world
		# world 			   	= clearCanvas tempcontext world
		| isJust state.currentShape
			= ({state & currentOrigin = Nothing, shapes = [fromJust state.currentShape:state.shapes], currentShape = Nothing}, world)
			= ({state & currentOrigin = Nothing}, world)

	// generate onDrawing event
	onMouseMove _ {[0]=e} state world
		= case state.currentOrigin of
			Just coord = onDrawing coord e state world
			_          = (state, world)

	onDrawing (dx,dy) e state world
    	# ((x, y), e, world) = getCoordinates e world
		# (tempcontext, world) = getContext cid True world
			
		# world = clearCanvas tempcontext world

		# currentShape = case state.selectedTool of	
				"L"	= Line state.selectedColor dx dy x y
				"R"	= Rect state.selectedColor False dx dy x y
				"r"	= Rect state.selectedColor True dx dy x y
				"C"	= Circle state.selectedColor False dx dy x y
				"c"	= Circle state.selectedColor True dx dy x y

		# world = draw tempcontext currentShape world
			
		= ({state & currentShape = Just currentShape}, world)

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

editlet2 :: Task (Editlet Drawing [Shape])
editlet2 = withShared (painterEditlet []) (\editlet -> shareditlet "1" editlet
													-|| 
									                   shareditlet "2" editlet)
 
//editlet = updateInformation "Painter" [] (painterEditlet [DrawLine "red" 1 1 100 100])
//		>>= \ds -> viewInformation "Drawing" [] ds
		    
Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist taskletExamples)) world
