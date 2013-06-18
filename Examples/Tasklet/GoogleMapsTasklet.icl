module GoogleMapsTasklet

import iTasks, Tasklet
import Text.StringAppender, graph_to_sapl_string, MovingEntity
import sapldebug

taskletExamples :: [Workflow]
taskletExamples =
	[workflow "Google MAP with sharing" "Basic Google Maps functionality" tasklet]
    
Start :: *World -> *World
//Start world = startEngine tasklet0 world
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist taskletExamples)) world

:: GoogleMapsOptions = {center    :: HtmlObject
          			   ,zoom      :: Int
          			   ,mapTypeId :: HtmlObject
        			   }
 
:: GoogleMapsState = {map         :: Maybe HtmlObject
                     ,time        :: Int
					 ,centerLA    :: Real
					 ,centerLO    :: Real
					 ,waypoints   :: [(!Real,!Real)]
					 ,plane       :: Maybe MovingEntity
					 ,wppos       :: Int
					 ,planejs     :: Maybe HtmlObject
					 ,waypointsjs :: [HtmlObject]
					 ,initialized :: Bool
					 }

:: MarkerOptions = {map        ::  HtmlObject
				   ,position   ::  HtmlObject
				   ,title      ::  HtmlObject
				   ,draggable  ::  Bool
				   ,icon       ::  Maybe HtmlObject
				   }

derive class iTask MovingEntity, EntityProperties

ROUTE2 = [(52.9047608002297, 4.7124481201171875),(52.904346653702405, 4.8401641845703125),
         (52.83927653705786, 4.857330322265625),(52.82932091031373, 4.7076416015625)]


tasklet = withShared (ROUTE2,Just (newMovingEntity 0 (ROUTE2!! 0) 300.0 0)) 
               (\state -> 	interactWithSimulation state
                     -||    simulateAirplanePosition state)
                   
//simulateAirplanePosition :: (Shared ([LatLng],Maybe MovingEntity)) -> Task (([LatLng],Maybe MovingEntity)) 
//simulateAirplanePosition :: (Shared ([LatLng],Maybe MovingEntity)) -> Task Void 
simulateAirplanePosition state
	= withShared (1, 0)
		(\postime ->
			forever (wait 1 >>- update newPlanePosition (postime >+< state))
		) 

newPlanePosition :: ((Int,Int),([(Real,Real)],Maybe MovingEntity)) -> ((Int,Int),([(Real,Real)],Maybe MovingEntity))
newPlanePosition ((pos,time),([],Just plane)) = ((pos,time + 1),([],Just {plane&timeLate = time}))
newPlanePosition ((pos,time),(route,Nothing)) = ((pos,time + 1),(route,Nothing))
newPlanePosition ((pos,time),(route,Just plane)) 
# (plane,pos) = moveAlongWayPointsDeg plane route pos time
= ((pos,time + 1),(route,Just plane))

interactWithSimulation :: (Shared ([(!Real,!Real)],Maybe MovingEntity))  -> Task ([(!Real,!Real)],Maybe MovingEntity)
interactWithSimulation state 
= 
 mkInstanceId >>= \iid -> 
	  		  (mkTaskWithShared (iid, googleMapsTasklet (52.8825984009408,4.74849700927734) 10) state updateFun  @> (mapWp,state)) 
updateFun (wps,plane) st = {st & waypoints = wps, plane = plane}
mapWp (Value wpp _) _ = Just wpp


googleMapsTasklet :: (!Real,!Real) Int -> Tasklet GoogleMapsState ([(!Real,!Real)],Maybe MovingEntity)
googleMapsTasklet (cla,clo) zoom = 
	{ generatorFunc		= googleMapsGUI
	, resultFunc		= \{centerLA,centerLO,waypoints,plane,time,initialized} = if initialized (Value (waypoints,plane) False) NoValue
	, tweakUI  			= setTitle "Google Maps Tasklet"
	}
where
	googleMapsGUI iid taskId Nothing iworld 
		= googleMapsGUI iid taskId (Just {map = Nothing, centerLA = cla, centerLO = clo,
		                                  waypoints = [], time = 0, wppos = 0,planejs = Nothing,waypointsjs = [],
		                                  plane = Just (newMovingEntity 1 ( (cla,clo)) 300.0 0),initialized = False}) iworld

	googleMapsGUI iid _ (Just st) iworld

		# canvas = DivTag [IdAttr "map_place_holder", StyleAttr "width:100%; height:100%"] []

		# gui = { TaskletHTML
				| width  		= FlexSize //ExactSize 600
				, height 		= FlexSize //ExactSize 300
				, html   		= HtmlDef (html canvas)
				, eventHandlers = [HtmlEvent "tasklet" "init" onInit
				                  ,HtmlEvent "tasklet" "update" onUpdate
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

	    onScriptLoad st _ _ d
		    # (d, _) = setDomAttr d "map_place_holder" "innerHTML" "<div id=\"map_canvas\" style=\"width:100%; height:100%\"/>"
		    # (d, mapdiv) = getDomElement d "map_canvas"  
	        
		    # (d, typeId) = findObject d "google.maps.MapTypeId.ROADMAP"
		    # (d, center) = createObject d "google.maps.LatLng" [toHtmlObject st.centerLA, toHtmlObject st.centerLO]

		    # (d, map) = createObject d "google.maps.Map" 
		    				[mapdiv
		    				,toHtmlObject {center = center, zoom = zoom, mapTypeId = typeId}]

		    # (d, mapevent) = findObject d "google.maps.event" 
			# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "dragend", onChange]
			# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "maptypeid_changed", onChange]
			# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "click", onClick]
			# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "zoom_changed", onChange]
			# (d, window)  = findObject d "window"	
			= (d, {GoogleMapsState| st & map = Just map, planejs = Nothing})
		where
			onChange = createEventHandler updatePerspective iid
			onClick  = createEventHandler addMarker iid

		// Google maps API doesn't like to be loaded twice	
		onInit st iid e d
			# (d, mapsobj) = findObject d "google.maps"
			| isUndefined mapsobj 
			= (loadMapsAPI iid e d, st)
			= onScriptLoad st iid e d
		
		onUpdate st iid e d
			# (d,st) = updateView st d
			= (d,st)
		
		loadMapsAPI iid e d	
			# (d, window)  = findObject d "window"	
			# (d, _, _)    = setObjectAttr d window "gmapscallback" (createEventHandler onScriptLoad iid)
	
			= loadExternalJS d "http://maps.googleapis.com/maps/api/js?sensor=false&callback=gmapscallback"
					(createEventHandler nullEventHandler iid)

		nullEventHandler st _ _ d = (d, st)

		onDestroy st=:{GoogleMapsState| map = Just map} _ _ d
		    # (d, mapevent)    = findObject d "google.maps.event" 
			# (d, mapevent, _) = runObjectMethod d mapevent "clearInstanceListeners" [map]
		
			// clear generated stuff
			# (d, _)           = setDomAttr d "map_place_holder" "innerHTML" ""
		
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

		onDragWP wpid st=:{GoogleMapsState|waypoints} _ e  d
			# (d, e, latlo)  = getObjectAttr d e "latLng"
			# (d, latlo, la) = runObjectMethod d latlo "lat" []
			# (d, latlo, lo) = runObjectMethod d latlo "lng" []
			# waypoints      = updateWP wpid la lo waypoints
			= (d, {st & waypoints = waypoints})	

		addMarker st=:{GoogleMapsState|map = Just map, waypoints, waypointsjs} _ e  d 
			# (d, e, latlo)    = getObjectAttr d e "latLng"
			# (d, latlo, la)   = runObjectMethod d latlo "lat" []
			# (d, latlo, lo)   = runObjectMethod d latlo "lng" []
			# wpid             = length waypoints
			# waypoints        = waypoints ++  [(fromHtmlObject la,fromHtmlObject lo)] 
			# (d, marker)      = createObject d "google.maps.Marker" [toHtmlObject {MarkerOptions| map = map, position = latlo, title = toHtmlObject (toString wpid),draggable = True, icon = Nothing}]		
		    # (d, mapevent)    = findObject d "google.maps.event" 
			# (d, _, _)        = runObjectMethod d mapevent "addListener" [marker, toHtmlObject "dragend", createEventHandler (onDragWP wpid) iid]
			= (d, {st & waypoints = waypoints, waypointsjs = waypointsjs ++ [marker]})	
			
        updateWP wpid nla nlo wps = map ud [(wid,la,lo)\\ (la,lo)<- wps & wid <- [0..]]
        where ud wp=:(wid,la,lo) | wpid == wid = (fromHtmlObject nla,fromHtmlObject nlo)
                                               = (la,lo)
                                               
		updateView :: GoogleMapsState *HtmlDocument -> (*HtmlDocument,GoogleMapsState)
		updateView st=:{map = Just map,waypoints,waypointsjs,plane = Just plane=:{MovingEntity|position},planejs = Nothing} d
		# (d, newpos)     = createObject d "google.maps.LatLng" [toHtmlObject (fst position), toHtmlObject (snd position)]
		# (d, planejs)     = createObject d "google.maps.Marker" [toHtmlObject {MarkerOptions| map = map, position = newpos, 
					                                                                        title = toHtmlObject "JSF",draggable = False, 
					                                                                        icon = Just (toHtmlObject "icons/jsf.png")}]	
		#(newwpsjs,d) = doElems waypoints waypointsjs d
		# curid       = length waypointsjs
		#(d,extrawps) = createWaypoints curid map (drop curid waypoints)  d
		= (d, {st & waypointsjs = waypointsjs ++ extrawps, initialized = True, planejs = Just planejs})
					                                                                        	
		updateView st=:{map = Just map,waypoints,waypointsjs,plane = Just plane=:{MovingEntity|position},planejs = Just planejs} d 
		#(newwpsjs,d) = doElems waypoints waypointsjs d
		#(d,planejs)  = setPosition planejs position d
		# curid       = length waypointsjs
		#(d,extrawps) = createWaypoints curid map (drop curid waypoints)  d
		= (d, {st & waypointsjs = waypointsjs ++ extrawps, initialized = True, planejs = Just planejs})
		
		doElems :: [(!Real,!Real)] [HtmlObject] *HtmlDocument -> ([HtmlObject],*HtmlDocument)
		doElems [] ws d  = (ws,d)
		doElems _  [] d  = ([],d)
		doElems [pos:wps] [wpjs:waypointsjs] d
		   # (d,newwpjs) = setPosition wpjs pos d
		   # (nwpsjs,d)  = doElems wps waypointsjs d
		   = ([newwpjs:nwpsjs],d)
		  
		setPosition:: HtmlObject (!Real,!Real) *HtmlDocument -> (*HtmlDocument,HtmlObject)
		setPosition obj position d
				    # (d, newpos)     = createObject d "google.maps.LatLng" [toHtmlObject (fst position), toHtmlObject (snd position)]
					# (d, obj, _)     = runObjectMethod d obj "setPosition" [newpos]
					= (d,obj)
		
		createWaypoints  id map [] d 
		= (d,[])
		createWaypoints  id map  [wp:wps] d  
		# (d,wpjs)  = createWaypoint  id map wp d 
		# (d,wpsjs) = createWaypoints  (id+1) map wps d 
		= (d,[wpjs:wpsjs])
		
		createWaypoint  id map position d
		 # (d, pos)         = createObject d "google.maps.LatLng" [toHtmlObject (fst position), toHtmlObject (snd position)]
		 # (d, marker)      = createObject d "google.maps.Marker" [toHtmlObject {MarkerOptions| map = map, position = pos, title = toHtmlObject (toString id),draggable = True, icon = Nothing}]		
		 # (d, mapevent)    = findObject d "google.maps.event" 
		 # (d, _, _)        = runObjectMethod d mapevent "addListener" [marker, toHtmlObject "dragend", createEventHandler (onDragWP id) iid]
		 = (d,marker)
		
//-------------------------------------------------------------------------


//UTIL
(>>-) infixl 1 :: !(Task a) (Task b) -> Task b | iTask a & iTask b
(>>-) taska taskb = step taska [WhenStable (const taskb)]

//Wait for (at least) n seconds
wait :: Int -> Task Void
wait n = get currentTime >>= \start -> watch currentTime >>* [OnValue (\(Value now _) -> if (now > addSeconds n start) (Just (return Void)) Nothing)]
where
	//ONLY CORRECT FOR n < 60
	addSeconds n t = t + {Time|hour=0,min=0,sec=n}


ifValue pred (Value v _) | pred v
	= Just (return v)
	= Nothing

ifStable (Value v True) = Just (return v)
ifStable _				= Nothing

returnC :: b (TaskValue a) -> Task b | iTask b
returnC v _ = return v

returnV :: (TaskValue a) -> Task a | iTask a
returnV (Value v _) = return v

