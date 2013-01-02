module TaskletServer

import iTasks, Tasklet
import StringAppender, CodeGeneratorJS, graph_to_sapl_string
import sapldebug

//-------------------------------------------------------------------------
//
// TODO:
//
// zoom, maptype, markers

:: GoogleMapsOptions = {center    :: HtmlObject
          			   ,zoom      :: Int
          			   ,mapTypeId :: HtmlObject
        			   };

:: GoogleMapsState = {map      :: Maybe HtmlObject
					 ,centerLA :: Real
					 ,centerLO :: Real}

googleMapsTasklet :: Real Real -> Tasklet GoogleMapsState (Real,Real)
googleMapsTasklet cla clo = 
	{ generatorFunc		= googleMapsGUI
	, resultFunc		= \{centerLA,centerLO} = Value (centerLA,centerLO) False
	, tweakUI  			= setTitle "Google Maps Tasklet"
	}
where
	googleMapsGUI iid taskId Nothing iworld 
		= googleMapsGUI iid taskId (Just {map = Nothing, centerLA = cla, centerLO = clo}) iworld

	googleMapsGUI iid _ (Just st) iworld

		# canvas = DivTag [IdAttr "map_place_holder", StyleAttr "width:100%; height:100%"] []

		# gui = { TaskletHTML
				| width  		= ExactSize 300
				, height 		= ExactSize 300
				, html   		= HtmlDef (html canvas)
				, eventHandlers = [HtmlEvent "tasklet" "init" onInit
				                  ,HtmlEvent "tasklet" "destroy" onDestroy
				                  ,HtmlEvent "tasklet" "afterlayout" onResize]
				}
			
		= (TaskletHTML gui, st, iworld)
				
	where
		updatePerspective st=:{map = Just map} _ _  d 
			# (d, map, center) = runObjectMethod d map "getCenter" []
			# (d, center, la) = runObjectMethod d center "lat" []
			# (d, center, lo) = runObjectMethod d center "lng" []
			= (d, {st & centerLA = fromHtmlObject la, centerLO = fromHtmlObject lo})	

	    onScriptLoad st _ _ d
		    # (d, _) = setDomAttr d "map_place_holder" "innerHTML" "<div id=\"map_canvas\" style=\"width:100%; height:100%\"/>"
		    # (d, mapdiv) = getDomElement d "map_canvas"
	        
		    # (d, typeId) = findObject d "google.maps.MapTypeId.ROADMAP"
		    # (d, center) = createObject d "google.maps.LatLng" [toHtmlObject st.centerLA, toHtmlObject st.centerLO]

		    # (d, map) = createObject d "google.maps.Map" 
		    				[mapdiv
		    				,toHtmlObject {center = center, zoom = 8, mapTypeId = typeId}]

		    # (d, mapevent) = findObject d "google.maps.event" 
			# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "dragend", onChange]
			# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "maptypeid_changed", onChange]
			# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "zoom_changed", onChange]
			= (d, {st & map = Just map})
		where
			onChange = createEventHandler updatePerspective iid

		// Google maps API doesn't like to be loaded twice	
		onInit st iid e d
			# (d, mapsobj) = findObject d "google.maps"
			| isUndefined mapsobj 
			= (loadMapsAPI iid e d, st)
			= onScriptLoad st iid e d
		
		loadMapsAPI iid e d	
			# (d, window)  = findObject d "window"	
			# (d, _, _)    = setObjectAttr d window "gmapscallback" (createEventHandler onScriptLoad iid)
	
			= loadExternalJS d "http://maps.googleapis.com/maps/api/js?sensor=false&callback=gmapscallback"
					(createEventHandler nullEventHandler iid)

		nullEventHandler st _ _ d = (d, st)

		onDestroy st=:{map = Just map} _ _ d
		    # (d, mapevent) = findObject d "google.maps.event" 
			# (d, mapevent, _) = runObjectMethod d mapevent "clearInstanceListeners" [map]
		
			// clear generated stuff
			# (d, _) = setDomAttr d "map_place_holder" "innerHTML" ""
		
			= (d, {st & map = Nothing})

		onDestroy st _ _ d
			= (d, st)

		// http://stackoverflow.com/questions/1746608/google-maps-not-rendering-completely-on-page
		onResize st=:{map = Just map} _ _ d
		    # (d, mapevent) = findObject d "google.maps.event" 
			# (d, mapevent, _) = runObjectMethod d mapevent "trigger" [map, toHtmlObject "resize"]
		
			# (d, center) = createObject d "google.maps.LatLng" [toHtmlObject st.centerLA, toHtmlObject st.centerLO]		
			# (d, map, _) = runObjectMethod d map "setCenter" [center]	
		
			= (d, st)

		onResize st _ _ d
			= (d, st)

//-------------------------------------------------------------------------
// http://www.sephidev.net/external/webkit/LayoutTests/fast/dom/Geolocation/argument-types-expected.txt
// http://html5doctor.com/finding-your-position-with-geolocation/

:: GeoLocationParams = {enableHighAccuracy :: Bool
					   ,timeout            :: Int
					   ,maximumAge         :: Int
					   }

:: GPSCoord :== (Real,Real)

geoTasklet :: Tasklet (Maybe GPSCoord) (Maybe GPSCoord)
geoTasklet = 
	{ generatorFunc		= geoTaskletGUI
	, resultFunc		= \pos = Value pos False
	, tweakUI  			= setTitle "GEO Tasklet"
	}

geoTaskletGUI _ _ _ iworld

	# gui = { TaskletHTML
			| width  		= ExactSize 300
			, height 		= ExactSize 30
			, html   		= HtmlDef "Current position: <span id='loc'/>"
			, eventHandlers = [HtmlEvent "tasklet" "init" onInit]
			}
			
	= (TaskletHTML gui, Nothing, iworld)
where
    onSuccess st _ pos d
		# (d, _, la) = getObjectAttr d pos "coords.latitude"
		# (d, _, lo) = getObjectAttr d pos "coords.longitude"		    
		# (d, _) = setDomAttr d "loc" "innerHTML" (fromHtmlObject la +++ ", " +++ fromHtmlObject lo)
    	= (d, Just (la,lo))

    onFailure st _ msg d
		# (d, _) = setDomAttr d "loc" "innerHTML" "FAILURE"
    	= (d, st)

	onInit st iid _ d
	    # (d, loc) = findObject d "navigator.geolocation" 
		# (d, loc, _) = runObjectMethod d loc "getCurrentPosition" 
							[createEventHandler onSuccess iid
							,createEventHandler onFailure iid
				    		,toHtmlObject {enableHighAccuracy = True, timeout = 10 * 1000 * 1000, maximumAge = 0}]
				
		= (d, st)

//-------------------------------------------------------------------------

pushTasklet :: Tasklet Int Int 
pushTasklet = 
	{ generatorFunc		= pushGenerateGUI
	, resultFunc		= \i = Value i False
	, tweakUI  			= setTitle "Push Tasklet"
	}

pushGenerateGUI :: !TaskInstanceId !TaskId (Maybe Int) !*IWorld -> *(!TaskletGUI Int, !Int, !*IWorld)

pushGenerateGUI iid taskId Nothing iworld  = pushGenerateGUI iid taskId (Just 1) iworld

pushGenerateGUI _ _ (Just st) iworld  

	# gui = { TaskletHTML
			| width  		= ExactSize 50
			, height 		= ExactSize 27
			, html   		= HtmlDef ("<input type=\"button\" id=\"pushbtn\" name=\"push\" value=\""+++ toString st +++"\">")
			, eventHandlers = [HtmlEvent "pushbtn" "click" onClick]
			}
			
	= (TaskletHTML gui, st, iworld)
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

info = "Draw something, but use the pencil _slowly_ in Chrome!" 

painterTasklet :: Tasklet PainterState Drawing
painterTasklet = 
	{ generatorFunc		= painterGenerateGUI
	, resultFunc		= \{draw,finished} = Value (Drawing draw) finished
	, tweakUI  			= setTitle "Drawing Tasklet"
	}

canvasWidth :== 300
canvasHeight :== 300

// TODO: http://jaspervdj.be/blaze/tutorial.html

painterGenerateGUI iid taskId Nothing iworld  
	= painterGenerateGUI iid taskId (Just {tool = "P", color = "black", mouseDown = Nothing, draw = [], lastDraw = Nothing, finished = False}) iworld

painterGenerateGUI _ _ (Just defSt) iworld  

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
			  width  		= ExactSize (canvasWidth + 70)
			, height 		= ExactSize (canvasHeight + 50)
			, html   		= HtmlDef html
			, eventHandlers = eventHandlers
			}
			
	= (TaskletHTML gui, defSt, iworld)

where
	onStart state _ e d
		# (d, context) = getContext d False
		# (d, context) = foldl (\(d, context) dr = draw d context dr) (d, context) (reverse state.draw)
		= (d, state)
		
	onChangeTool state iid e d
		# (d, e, selectedIndex) = getObjectAttr d e "target.selectedIndex"
		# (d, e, atool) = getObjectAttr d e ("target.options["+++ fromInt (fromHtmlObject selectedIndex) +++"].value")

		// Fire event
		# d = fireEvent d iid "changeTool" (fromHtmlObject selectedIndex)

		= (d, {state & tool = fromHtmlObject atool})		

	onSelectColor color state iid e d
		# d = foldl (\d el = fst (setDomAttr d el "style.borderColor" "white")) d
					["selectorYellow","selectorRed","selectorGreen","selectorBlue","selectorBlack"]

		# (d, e, target) = getObjectAttr d e "target"
		# (d, target, _) = setObjectAttr d target "style.borderColor" "pink"
		
		// Change color
		# d = fireEvent d iid "changeColor" color
		
		= (d, {state & color = color})

	getCoordinates d e
	    # (d, e, x) = getObjectAttr d e "layerX"
	    # (d, e, y) = getObjectAttr d e "layerY"
	    = (d, e, (fromHtmlObject x, fromHtmlObject y))

	onMouseDown state _ e d
	    # (d, e, coords) = getCoordinates d e
		= (d, {state & mouseDown = Just coords, lastDraw = Nothing})

	getCanvas d temp
		= case temp of
			True = getDomElement d "tempcanvas"
			_	 = getDomElement d "canvas"

	getContext d temp
	 	# (d, canvas) = getCanvas d temp
		# (d, canvas, context) = runObjectMethod d canvas "getContext" [toHtmlObject "2d"]  // not "2D" !
		= (d, context)

	clearContext d context
		# (d, context, _) = runObjectMethod d context "clearRect" 
				[toHtmlObject 0, toHtmlObject 0, toHtmlObject canvasWidth, toHtmlObject canvasHeight] 
		= (d, context)

	onMouseUp state iid e d
		# (d, tempcanvas) = getCanvas d True
		# (d, tempcontext) = getContext d True
		# (d, context) = getContext d False
		# (d, context, _) = runObjectMethod d context "drawImage" [toHtmlObject tempcanvas, toHtmlObject 0, toHtmlObject 0]
		# (d, tempcontext) = clearContext d tempcontext
		| isJust state.lastDraw
			# d = fireEvent d iid "draw" (fromJust state.lastDraw) // Fire event
			= (d, {state & mouseDown = Nothing, draw = [fromJust state.lastDraw:state.draw], lastDraw = Nothing})
			= (d, {state & mouseDown = Nothing})

	// generate onDrawing event
	onMouseMove state iid e d
		= case state.mouseDown of
			Just coord = onDrawing state coord iid e d
			_          = (d, state)
				
	drawLine d context color x1 y1 x2 y2
		# (d, context, _) = runObjectMethod d context "beginPath" []
		# (d, context, _) = setObjectAttr d context "strokeStyle" color
		# (d, context, _) = runObjectMethod d context "moveTo" [toHtmlObject x1, toHtmlObject y1]
		# (d, context, _) = runObjectMethod d context "lineTo" [toHtmlObject x2, toHtmlObject y2]
		# (d, context, _) = runObjectMethod d context "stroke" []
		= (d, context)				

	drawRect d context color x1 y1 x2 y2
		# (d, context, _) = setObjectAttr d context "strokeStyle" color
		# (d, context, _) = runObjectMethod d context "strokeRect" 
				[toHtmlObject x1, toHtmlObject y1, toHtmlObject (x2 - x1), toHtmlObject (y2 - y1)]
		= (d, context)

	drawFilledRect d context color x1 y1 x2 y2
		# (d, context, _) = setObjectAttr d context "fillStyle" color
		# (d, context, _) = runObjectMethod d context "fillRect"
				[toHtmlObject x1, toHtmlObject y1, toHtmlObject (x2 - x1), toHtmlObject (y2 - y1)]
		= (d, context)

	drawCircle d context fill color x1 y1 x2 y2
		# (d, context, _) = runObjectMethod d context "beginPath" []
		# (d, context, _) = setObjectAttr d context "strokeStyle" color
		# (d, context, _) = setObjectAttr d context "fillStyle" color
	
		# (d, context, _) = runObjectMethod d context "arc"
						[toHtmlObject (center x1 x2), toHtmlObject (center y1 y2)
						,toHtmlObject (toInt ((distance x1 y1 x2 y2)/2.0))
						,toHtmlObject 0, toHtmlObject (3.14159265*2.0), toHtmlObject True]
							
		# (d, context, _) = case fill of 
						True = runObjectMethod d context "fill" []
						_	 = runObjectMethod d context "stroke" []
		# (d, context, _) = runObjectMethod d context "closePath" []
		= (d, context)
	where
		center x1 x2 = (max x1 x2) - (abs (x1 - x2))/2
		distance x1 y1 x2 y2 = sqrt (toReal ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)))
				
	onDrawing state (dx,dy) iid e d
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
	[workflow "Simple push button tasklet" "Push the button 3(three) times" tasklet1,
	 workflow "Painter tasklet" "Simple painter tasklet" tasklet2,
	 workflow "GEO location tasklet" "GEO location tasklet" tasklet3,
	 workflow "Google MAP" "Basic Google Maps functionality" tasklet4]

tasklet2 :: Task Drawing
tasklet2
	= 		mkInstanceId >>= \iid ->
			mkTask (iid, painterTasklet)
		>>* [ OnValue ifStable 
			] 

tasklet1 :: Task Int
tasklet1
	= 		mkInstanceId >>= \iid ->
			mkTask (iid, pushTasklet)
		>>* [ OnAction ActionOk (ifValue (\n -> n >= 3))
            ] 

tasklet3 :: Task (Maybe GPSCoord)
tasklet3
	= 		mkInstanceId >>= \iid ->
			mkTask (iid, geoTasklet)
		>>* [ OnAction ActionOk (ifValue isJust),
		  	  OnAction ActionCancel (\_ = Nothing)
            ] 

tasklet4 :: Task (Real, Real)
tasklet4
	= 		mkInstanceId >>= \iid ->
	 		mkTask (iid, googleMapsTasklet 47.471944 19.050278)
							 
ifValue pred (Value v _) | pred v
	= Just (return v)
	= Nothing

ifStable (Value v True) = Just (return v)
ifStable _				= Nothing

returnC :: b (TaskValue a) -> Task b | iTask b
returnC v _ = return v

returnV :: (TaskValue a) -> Task a | iTask a
returnV (Value v _) = return v

setType st Nothing document = (document, st, False)
setType st (Just dynArg) document 
	# res = case dynArg of
				((a, b, c) :: (Int, String, Bool)) = True
				_		   				           = False
	= (document, st, res)

mkPainter = mkInstanceId >>= \iid -> mkInterfaceTask (iid, painterTasklet) [InterfaceFun "setType" setType]

Start :: *World -> *World
Start world = startEngine 
				[{PublishedTask|url="/bubu"
				               ,task=TaskWrapper (\_ -> mkPainter) //(googleMapsTasklet 47.471944 19.050278))
				               ,defaultFormat = JSONGui}] 
				world
 