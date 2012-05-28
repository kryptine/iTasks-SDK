module TaskletExamples

import iTasks, Task, Tasklet, sapldebug

//-------------------------------------------------------------------------

:: GoogleMapsOptions = {center    :: HtmlObject
          			   ,zoom      :: Int
          			   ,mapTypeId :: HtmlObject
        			   };

googleMapsTasklet :: Tasklet (Maybe HtmlObject) Void
googleMapsTasklet = 
	{ Tasklet
	| defSt				= Nothing
	, generatorFunc		= googleMapsGUI
	, resultFunc		= \_ = NoValue
	, tweakUI  			= \t = (paneled (Just "Google Maps Tasklet") Nothing Nothing [t])		
	}

googleMapsGUI _ state iworld

	# canvas = DivTag [IdAttr "map_canvas", StyleAttr "width:100%; height:100%"] []

	# gui = { TaskletHTML
			| width  		= Fixed 300
			, height 		= Fixed 300
			, html   		= toString canvas
			, eventHandlers = [HtmlEvent "tasklet" "init" onInit]
			}
			
	= (TaskletHTML gui, state, iworld)
where
    onScriptLoad st _ _ d
	    # (d, typeId) = findObject d "google.maps.MapTypeId.ROADMAP"
	    # (d, center) = createObject d "google.maps.LatLng" [JSFuncArg -34.397, JSFuncArg 150.644]
	    # (d, mapdiv) = getDomElement d "map_canvas" 

	    # (d, map) = createObject d "google.maps.Map" 
	    				[JSFuncArg mapdiv
	    				,JSFuncArg {center = center, zoom = 8, mapTypeId = typeId}]

		= (d, Just map)

	onInit st taskId _ d
		# d = loadExternalJS d "http://maps.googleapis.com/maps/api/js?sensor=false"
					(handleJSEvent onScriptLoad taskId)
				
		= (d, st)

//-------------------------------------------------------------------------
// http://www.sephidev.net/external/webkit/LayoutTests/fast/dom/Geolocation/argument-types-expected.txt
// http://html5doctor.com/finding-your-position-with-geolocation/

:: GeoLocationParams = {enableHighAccuracy :: Bool
					   ,timeout            :: Int
					   ,maximumAge         :: Int
					   }

:: GPSCoord :== (String,String)

geoTasklet :: Tasklet (Maybe GPSCoord) (Maybe GPSCoord)
geoTasklet = 
	{ Tasklet
	| defSt				= Nothing
	, generatorFunc		= geoTaskletGUI
	, resultFunc		= \pos = Value pos Unstable
	, tweakUI  			= \t = (paneled (Just "GEO Tasklet") Nothing Nothing [t])		
	}

geoTaskletGUI _ state iworld

	# gui = { TaskletHTML
			| width  		= Fixed 300
			, height 		= Fixed 30
			, html   		= "Current position: <span id='loc'/>"
			, eventHandlers = [HtmlEvent "tasklet" "init" onInit]
			}
			
	= (TaskletHTML gui, state, iworld)
where
    onSuccess st _ pos d
		# (d, _, la) = getObjectAttr d pos "coords.latitude"
		# (d, _, lo) = getObjectAttr d pos "coords.longitude"		    
		# (d, _) = setDomAttr d "loc" "innerText" (la +++ ", " +++ lo)
    	= (d, Just (la,lo))

    onFailure st _ msg d
		# (d, _) = setDomAttr d "loc" "innerText" "FAILURE"
    	= (d, st)

	onInit st taskId _ d
	    # (d, loc) = findObject d "navigator.geolocation" 
		# (d, loc, _) = runObjectMethod d loc "getCurrentPosition" 
							[JSFuncArg (handleJSEvent onSuccess taskId)
							,JSFuncArg (handleJSEvent onFailure taskId)
				    		,JSFuncArg {enableHighAccuracy = True, timeout = 10 * 1000 * 1000, maximumAge = 0}]
				
		= (d, st)

//-------------------------------------------------------------------------

pushTasklet :: Tasklet Int Int 
pushTasklet = 
	{ Tasklet
	| defSt				= 1
	, generatorFunc		= pushGenerateGUI
	, resultFunc		= \i = Value i Unstable
	, tweakUI  			= \t = (paneled (Just "Push Tasklet") (Just "Push the button 3 times") Nothing [t])	
	}

pushGenerateGUI :: !TaskId !Int !*IWorld -> *(!TaskletGUI Int, !Int, !*IWorld)
pushGenerateGUI _ state iworld  

	# gui = { TaskletHTML
			| width  		= Fixed 50
			, height 		= Fixed 27
			, html   		= "<input type=\"button\" id=\"pushbtn\" name=\"push\" value=\""+++ toString state +++"\">"
			, eventHandlers = [HtmlEvent "pushbtn" "click" onClick]
			}
			
	= (TaskletHTML gui, state, iworld)
where			
	onClick state _ _ d
		# (d, str) = setDomAttr d "pushbtn" "value" (toString (state + 1))
		= (d, state + 1)
 
//-------------------------------------------------------------------------

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

info = "Draw somthing, but please do _not_ use the pencil! (it kills the server)" 

painterTasklet :: Tasklet PainterState Drawing
painterTasklet = 
	{ defSt				= {tool = "P", color = "black", mouseDown = Nothing, draw = [], lastDraw = Nothing, finished = False}
	, generatorFunc		= painterGenerateGUI
	, resultFunc		= \{draw,finished} = Value (Drawing draw) (if finished Stable Unstable)
	, tweakUI  			= \t = (paneled (Just "Drawing Tasklet") (Just info) Nothing [t])	
	}

canvasWidth :== 300
canvasHeight :== 300

// TODO: http://jaspervdj.be/blaze/tutorial.html
painterGenerateGUI _ state iworld  

	# ws = toString canvasWidth
	# hs = toString canvasHeight
	
	# canvas = 
			DivTag [StyleAttr "position: relative; float: left;"] [
					CanvasTag [IdAttr "canvas", WidthAttr ws, HeightAttr hs, StyleAttr "border: 1px solid #000;"] [],
					CanvasTag [IdAttr "tempcanvas", WidthAttr ws, HeightAttr hs, StyleAttr "position: absolute; top: 1px; left: 1px;"] []
					]

	# editor = [
			DivTag [StyleAttr "float: right;"] [
					DivTag [IdAttr "selectorYellow", StyleAttr "border-style:solid; border-color:white; background-color:yellow; width: 40px; height:40px; margin: 5px;"] [],
					DivTag [IdAttr "selectorRed", StyleAttr "border-style:solid; border-color:white; background-color:red; width: 40px; height: 40px; margin: 5px;"] [],
					DivTag [IdAttr "selectorGreen", StyleAttr "border-style:solid; border-color:white; background-color:green; width: 40px; height: 40px; margin: 5px;"] [],
					DivTag [IdAttr "selectorBlue", StyleAttr "border-style:solid; border-color:white; background-color:blue; width: 40px; height: 40px; margin: 5px;"] [],
					DivTag [IdAttr "selectorBlack", StyleAttr "border-style:solid; border-color:pink; background-color:black; width: 40px; height: 40px; margin: 5px;"] []
					],
			DivTag [StyleAttr "clear: both"] [],
			DivTag [] ([
					Text "Tool: ",
					SelectTag [IdAttr "selecttool"] [
						OptionTag [ValueAttr "P"] [Text "Pencil"],
						OptionTag [ValueAttr "L"] [Text "Line"],
						OptionTag [ValueAttr "R"] [Text "Rectangle"],
						OptionTag [ValueAttr "r"] [Text "Rectangle (filled)"],
						OptionTag [ValueAttr "C"] [Text "Circle"],
						OptionTag [ValueAttr "c"] [Text "Circle (filled)"]],
					ButtonTag [IdAttr "clearbtn", TypeAttr "button"] [Text "Clear"],
					ButtonTag [IdAttr "finishbtn", TypeAttr "button"] [Text "Finish"]
					])
			]
					
	# html = DivTag [StyleAttr "width: 360px; margin-right: auto;"] [canvas:editor]		

	# eventHandlers = [
			HtmlEvent "tasklet" "init" onStart,
			HtmlEvent "selectorYellow" "click" (onSelectColor "yellow"),
			HtmlEvent "selectorRed" "click" (onSelectColor "red"),
			HtmlEvent "selectorGreen" "click" (onSelectColor "green"),
			HtmlEvent "selectorBlue" "click" (onSelectColor "blue"),
			HtmlEvent "selectorBlack" "click" (onSelectColor "black"),
			HtmlEvent "canvas" "mousedown" onMouseDown,
			HtmlEvent "tempcanvas" "mousedown" onMouseDown,			
			HtmlEvent "canvas" "mouseup" onMouseUp,
			HtmlEvent "tempcanvas" "mouseup" onMouseUp,			
			HtmlEvent "canvas" "mousemove" onMouseMove,
			HtmlEvent "tempcanvas" "mousemove" onMouseMove,
			HtmlEvent "selecttool" "change" onChangeTool,
			HtmlEvent "clearbtn" "click" onClickClear,
			HtmlEvent "finishbtn" "click" onClickFinish]

	# gui = { TaskletHTML |
			  width  		= Fixed (canvasWidth + 70)
			, height 		= Fixed (canvasHeight + 50)
			, html   		= toString html
			, eventHandlers = eventHandlers
			}
			
	= (TaskletHTML gui, state, iworld)

where
	onStart state _ e d
		# (d, context) = getContext d False
		# (d, context) = foldl (\(d, context) dr = draw d context dr) (d, context) (reverse state.draw)
		= (d, state)
		
	onChangeTool state _ e d
		# (d, e, selectedIndex) = getObjectAttr d e "target.selectedIndex"
		# (d, e, atool) = getObjectAttr d e ("target.options["+++selectedIndex+++"].value")
		= (d, {state & tool = atool})		

	onSelectColor color state _ e d
		# d = foldl (\d el = fst (setDomAttr d el "style.borderColor" "white")) d
					["selectorYellow","selectorRed","selectorGreen","selectorBlue","selectorBlack"]

		# (d, e, target) = getObjectAttrObject d e "target"
		# (d, target, _) = setObjectAttr d target "style.borderColor" "pink"	 
		= (d, {state & color = color})

	getCoordinates d e
	    # (d, e, x) = getObjectAttr d e "layerX"
	    # (d, e, y) = getObjectAttr d e "layerY"
	    = (d, e, ((toInt x), (toInt y)))

	onMouseDown state _ e d
	    # (d, e, coords) = getCoordinates d e
		= (d, {state & mouseDown = Just coords, lastDraw = Nothing})

	getCanvas d temp
		= case temp of
			True = getDomElement d "tempcanvas"
			_	 = getDomElement d "canvas"

	getContext d temp
	 	# (d, canvas) = getCanvas d temp
		# (d, canvas, context) = runObjectMethod d canvas "getContext" [JSFuncArg "2d"]  // not "2D" !
		= (d, context)

	clearContext d context
		# (d, context, _) = runObjectMethod d context "clearRect" 
				[JSFuncArg 0, JSFuncArg 0, JSFuncArg canvasWidth, JSFuncArg canvasHeight] 
		= (d, context)

	onMouseUp state _ e d
		# (d, tempcanvas) = getCanvas d True
		# (d, tempcontext) = getContext d True
		# (d, context) = getContext d False
		# (d, context, _) = runObjectMethod d context "drawImage" [JSFuncArg tempcanvas, JSFuncArg 0, JSFuncArg 0]
		# (d, tempcontext) = clearContext d tempcontext
		| isJust state.lastDraw
			= (d, {state & mouseDown = Nothing, draw = [fromJust state.lastDraw:state.draw], lastDraw = Nothing})
			= (d, {state & mouseDown = Nothing})

	// generate onDrawing event
	onMouseMove state _ e d
		= case state.mouseDown of
			Just coord = onDrawing state coord e d
			_          = (d, state)
				
	drawLine d context color x1 y1 x2 y2
		# (d, context, _) = runObjectMethod d context "beginPath" []
		# (d, context, _) = setObjectAttr d context "strokeStyle" color
		# (d, context, _) = runObjectMethod d context "moveTo" [JSFuncArg x1, JSFuncArg y1]
		# (d, context, _) = runObjectMethod d context "lineTo" [JSFuncArg x2, JSFuncArg y2]
		# (d, context, _) = runObjectMethod d context "stroke" []
		= (d, context)				

	drawRect d context color x1 y1 x2 y2
		# (d, context, _) = setObjectAttr d context "strokeStyle" color
		# (d, context, _) = runObjectMethod d context "strokeRect" 
				[JSFuncArg x1, JSFuncArg y1, JSFuncArg (x2 - x1), JSFuncArg (y2 - y1)]
		= (d, context)

	drawFilledRect d context color x1 y1 x2 y2
		# (d, context, _) = setObjectAttr d context "fillStyle" color
		# (d, context, _) = runObjectMethod d context "fillRect"
				[JSFuncArg x1, JSFuncArg y1, JSFuncArg (x2 - x1), JSFuncArg (y2 - y1)]
		= (d, context)

	drawCircle d context fill color x1 y1 x2 y2
		# (d, context, _) = runObjectMethod d context "beginPath" []
		# (d, context, _) = setObjectAttr d context "strokeStyle" color
		# (d, context, _) = setObjectAttr d context "fillStyle" color
	
		# (d, context, _) = runObjectMethod d context "arc"
						[JSFuncArg (center x1 x2), JSFuncArg (center y1 y2)
						,JSFuncArg (toInt ((distance x1 y1 x2 y2)/2.0))
						,JSFuncArg 0, JSFuncArg (3.14159265*2.0), JSFuncArg True]
							
		# (d, context, _) = case fill of 
						True = runObjectMethod d context "fill" []
						_	 = runObjectMethod d context "stroke" []
		# (d, context, _) = runObjectMethod d context "closePath" []
		= (d, context)
	where
		center x1 x2 = (max x1 x2) - (abs (x1 - x2))/2
		distance x1 y1 x2 y2 = sqrt (toReal ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)))
				
	onDrawing state (dx,dy) e d
	    # (d, e, (x, y)) = getCoordinates d e
		# (d, tempcontext) = getContext d True
			
		// Don't clear for pencil
		# (d, tempcontext) = case state.tool of 
				"P" = (d, tempcontext)
				    = clearContext d tempcontext

		# drawType = case state.tool of	
				"P" = DrawLine state.color dx dy x y
				"L"	= DrawLine state.color dx dy x y
				"R"	= DrawRect state.color False dx dy x y
				"r"	= DrawRect state.color True dx dy x y
				"C"	= DrawCircle state.color False dx dy x y
				"c"	= DrawCircle state.color True dx dy x y

		# (d, tempcontext) = draw d tempcontext drawType
			
		// Update start coordinates for pencil
		= case state.tool of 
				"P" = (d, {state & mouseDown = Just (x,y), draw=[drawType:state.draw], lastDraw = Nothing})
				_   = (d, {state & lastDraw = Just drawType})

	draw d context (DrawLine color x1 y1 x2 y2)
		= drawLine d context color x1 y1 x2 y2
	draw d context (DrawRect color False x1 y1 x2 y2)
		= drawRect d context color x1 y1 x2 y2
	draw d context (DrawRect color True x1 y1 x2 y2)
		= drawFilledRect d context color x1 y1 x2 y2
	draw d context (DrawCircle color fill x1 y1 x2 y2)
		= drawCircle d context fill color x1 y1 x2 y2

	onClickClear state _ e d
		# (d, context) = getContext d False
		# (d, context) = clearContext d context
		= (d, {state & draw = []})
			
	onClickFinish state _ e d
		= (d, {state & finished = True})

//-------------------------------------------------------------------------

taskletExamples :: [Workflow]
taskletExamples =
	[workflow "Simple push button tasklet" "Push the button 3 times" tasklet1,
	 workflow "Painter tasklet" "Simple painter tasklet" tasklet2,
	 workflow "GEO location tasklet" "GEO location tasklet" tasklet3,
	 workflow "Google MAP" "Basic Google Maps functionality" (mkTask googleMapsTasklet)]

tasklet2 :: Task Drawing
tasklet2
	= 		mkTask painterTasklet
		>>* [ OnValue ifStable returnV
            ] 

tasklet1 :: Task Int
tasklet1
	= 		mkTask pushTasklet
		>>* [ OnAction ActionOk (ifValue (\n -> n >= 3)) returnV
            ] 

tasklet3 :: Task (Maybe GPSCoord)
tasklet3
	= 		mkTask geoTasklet
		>>* [ OnAction ActionOk (ifValue isJust) returnV,
		  	  OnAction ActionCancel (\_ = True) (returnC Nothing)
            ] 

ifValue pred (Value v _) = pred v
ifValue _ _ = False

ifStable (Value v Stable) = True
ifStable _ = False

returnC :: b (TaskValue a) -> Task b | iTask b
returnC v _ = return v

returnV :: (TaskValue a) -> Task a | iTask a
returnV (Value v _) = return v
    
Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist taskletExamples)) world
 