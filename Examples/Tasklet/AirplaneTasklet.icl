module AirplaneTasklet

import iTasks, Tasklet
import Text.StringAppender, graph_to_sapl_string
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

:: MarkerOptions = {map        ::  HtmlObject
				   ,position   ::  HtmlObject
				   ,title      ::  HtmlObject
				   ,draggable  ::  Bool
				   }

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
				| width  		= ExactSize 600
				, height 		= ExactSize 300
				, html   		= HtmlDef (html canvas)
				, eventHandlers = [HtmlEvent "tasklet" "init" onInit
				                  ,HtmlEvent "tasklet" "destroy" onDestroy
				                  ,HtmlEvent "tasklet" "afterlayout" onResize]
				}
			
		= (TaskletHTML gui, st, iworld)
				
	where
		updatePerspective st=:{GoogleMapsState|map = Just map} _ _  d 
			# (d, map, center) = runObjectMethod d map "getCenter" []
			# (d, center, la) = runObjectMethod d center "lat" []
			# (d, center, lo) = runObjectMethod d center "lng" []
			= (d, {GoogleMapsState| st & centerLA = fromHtmlObject la, centerLO = fromHtmlObject lo})	

		addMarker st=:{GoogleMapsState|map = Just map} _ e  d 
			# (d, e, latlo) = getObjectAttr d e "latLng" 
			# (d, marker)   = createObject d "google.maps.Marker" [toHtmlObject {MarkerOptions| map = map, position = latlo, title = toHtmlObject "Hoppakee",draggable = True}]		
			= (d, st)	

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
			# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "click", onClick]
			# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "zoom_changed", onChange]
			= (d, {GoogleMapsState| st & map = Just map})
		where
			onChange = createEventHandler updatePerspective iid
			onClick = createEventHandler addMarker iid

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

		onDestroy st=:{GoogleMapsState| map = Just map} _ _ d
		    # (d, mapevent) = findObject d "google.maps.event" 
			# (d, mapevent, _) = runObjectMethod d mapevent "clearInstanceListeners" [map]
		
			// clear generated stuff
			# (d, _) = setDomAttr d "map_place_holder" "innerHTML" ""
		
			= (d, {st & GoogleMapsState.map = Nothing})

		onDestroy st _ _ d
			= (d, st)

		// http://stackoverflow.com/questions/1746608/google-maps-not-rendering-completely-on-page
		onResize st=:{GoogleMapsState| map = Just map} _ _ d
		    # (d, mapevent) = findObject d "google.maps.event" 
			# (d, mapevent, _) = runObjectMethod d mapevent "trigger" [map, toHtmlObject "resize"]
		
			# (d, center) = createObject d "google.maps.LatLng" [toHtmlObject st.centerLA, toHtmlObject st.centerLO]		
			# (d, map, _) = runObjectMethod d map "setCenter" [center]	
		
			= (d, st)

		onResize st _ _ d
			= (d, st)


//-------------------------------------------------------------------------

taskletExamples :: [Workflow]
taskletExamples =
	[workflow "Google MAP" "Basic Google Maps functionality" tasklet4]

tasklet4 :: Task (Real, Real)
tasklet4
	= 		mkInstanceId >>= \iid ->
	 		mkTask (iid, googleMapsTasklet 47.471944 19.050278) id
							 
ifValue pred (Value v _) | pred v
	= Just (return v)
	= Nothing

ifStable (Value v True) = Just (return v)
ifStable _				= Nothing

returnC :: b (TaskValue a) -> Task b | iTask b
returnC v _ = return v

returnV :: (TaskValue a) -> Task a | iTask a
returnV (Value v _) = return v
    
Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist taskletExamples)) world
