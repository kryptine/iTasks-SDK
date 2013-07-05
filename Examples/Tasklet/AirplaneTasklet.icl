module AirplaneTasklet

import iTasks, iTasks.Framework.ClientSupport.Tasklet
import Text.StringAppender, graph_to_sapl_string, MovingEntity
import sapldebug

//-------------------------------------------------------------------------
//
// TODO:
//
// zoom, maptype, markers

:: GoogleMapsOptions = {center    :: HtmlObject
          			   ,zoom      :: Int
          			   ,mapTypeId :: HtmlObject
        			   }
 
:: GoogleMapsState = {map         :: Maybe HtmlObject
                     ,time        :: Int
					 ,centerLA    :: Real
					 ,centerLO    :: Real
					 ,waypoints   :: [(!Int,!Real,!Real)]
					 ,plane       :: Maybe MovingEntity
					 ,wppos       :: Int
					 ,planejs     :: Maybe HtmlObject
					 ,waypointsjs :: [HtmlObject]
					 }

:: MarkerOptions = {map        ::  HtmlObject
				   ,position   ::  HtmlObject
				   ,title      ::  HtmlObject
				   ,draggable  ::  Bool
				   ,icon       ::  Maybe HtmlObject
				   }

derive class iTask MovingEntity, EntityProperties

googleMapsTasklet :: Real Real -> Tasklet GoogleMapsState ([(!Int,!Real,!Real)],Maybe MovingEntity)
googleMapsTasklet cla clo = 
	{ generatorFunc		= googleMapsGUI
	, resultFunc		= \{centerLA,centerLO,waypoints,plane,time} = Value (waypoints,plane) False
	, tweakUI  			= setTitle "Google Maps Tasklet"
	}
where
	googleMapsGUI iid taskId Nothing iworld 
		= googleMapsGUI iid taskId (Just {map = Nothing, centerLA = cla, centerLO = clo,
		                                  waypoints = [], time = 0, wppos = 0,planejs = Nothing,waypointsjs = [],
		                                  plane = Just (newMovingEntity 1 ( (cla,clo)) 300.0 0)}) iworld

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
		    				,toHtmlObject {center = center, zoom = 10, mapTypeId = typeId}]

		    # (d, mapevent) = findObject d "google.maps.event" 
			# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "dragend", onChange]
			# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "maptypeid_changed", onChange]
			# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "click", onClick]
			# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "zoom_changed", onChange]
			# (d, planejs)     = createObject d "google.maps.Marker" [toHtmlObject {MarkerOptions| map = map, position = center, 
			                                                                        title = toHtmlObject "JSF",draggable = False, 
			                                                                        icon = Just (toHtmlObject "icons/jsf.png")}]		
			# (d, window)  = findObject d "window"	
			# (d, _, _)    = runObjectMethod d window "setInterval" [createEventHandler doStep iid, toHtmlObject 1000]
			= (d, {GoogleMapsState| st & map = Just map, planejs = Just planejs})
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
			# d = updateView st d
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
			# waypoints        = waypoints ++  [(wpid,fromHtmlObject la,fromHtmlObject lo)] 
			# (d, marker)      = createObject d "google.maps.Marker" [toHtmlObject {MarkerOptions| map = map, position = latlo, title = toHtmlObject (toString wpid),draggable = True, icon = Nothing}]		
		    # (d, mapevent)    = findObject d "google.maps.event" 
			# (d, _, _)        = runObjectMethod d mapevent "addListener" [marker, toHtmlObject "dragend", createEventHandler (onDragWP wpid) iid]
			= (d, {st & waypoints = waypoints, waypointsjs = waypointsjs ++ [marker]})	
			
        updateWP wpid nla nlo wps = map ud wps
        where ud wp=:(wid,la,lo) | wpid == wid = (wid,fromHtmlObject nla,fromHtmlObject nlo)
                                               = wp
                                               
		// doStep is for simulation on the client
		doStep st=:{GoogleMapsState|waypoints,plane = Just plane,time,wppos,planejs} _  _ d
			# (plane=:{MovingEntity|position},wppos,time) = newPlanePosition ((plane,wppos,time),waypoints)
			# position        = position
		    # (d, newpos)     = createObject d "google.maps.LatLng" [toHtmlObject (fst position), toHtmlObject (snd position)]
			# (d, planejs, _) = runObjectMethod d (fromJust planejs) "setPosition" [newpos]
			= (d, {st & plane = Just plane, time = time, wppos = wppos, planejs = Just planejs})	


updateView :: GoogleMapsState *HtmlDocument -> *HtmlDocument
updateView st=:{waypoints,waypointsjs,plane = Just plane=:{MovingEntity|position},planejs = Just planejs} d 
#newwps       = map (\(_,x,y) -> (x,y)) waypoints
#(newwpsjs,d) = doElems newwps waypointsjs d
#position     = toDegrees position
#(d,planejs)  = setPosition planejs position d
= d
where
 doElems :: [(!Real,!Real)] [HtmlObject] *HtmlDocument -> ([HtmlObject],*HtmlDocument)
 doElems [] [] d = ([],d)
 doElems [pos:wps] [wpjs:waypointsjs] d
   # (d,newwpjs) = setPosition wpjs pos d
   # (nwpsjs,d)  = doElems wps waypointsjs d
   = ([newwpjs:nwpsjs],d)
  
setPosition:: HtmlObject (!Real,!Real) *HtmlDocument -> (*HtmlDocument,HtmlObject)
setPosition obj position d
		    # (d, newpos)     = createObject d "google.maps.LatLng" [toHtmlObject (fst position), toHtmlObject (snd position)]
			# (d, obj, _)     = runObjectMethod d obj "setPosition" [newpos]
			= (d,obj)

newPlanePosition :: ((MovingEntity,Int,Int),[(Int,Real,Real)]) -> (MovingEntity,Int,Int)
newPlanePosition ((plane,pos,time),[]) = ({plane&timeLate = time},pos,time + 1)
newPlanePosition ((plane,pos,time),route) 
# (plane,pos) = moveAlongWayPointsDeg plane (map f route) pos time
= (plane,pos,time + 1)
where f =  g
      g (_,la,lo) = (la,lo)

//-------------------------------------------------------------------------

taskletExamples :: [Workflow]
taskletExamples =
	[workflow "Google MAP" "Basic Google Maps functionality" tasklet4
	,workflow "Google MAP with sharing" "Basic Google Maps functionality" tasklet1]

tasklet1	
=	 mkInstanceId >>= \iid -> 
    	withShared ([],Nothing) (\state ->
	  		  (mkTaskWithShared (iid, googleMapsTasklet 52.8825984009408 4.74849700927734) state updateFun @> (mapWp,state)) 
			  -||-
			  viewSharedInformation "The waypoints are" [] state)


tasklet2	
=	 mkInstanceId >>= \iid -> 
    	withShared ([],Nothing) (\state ->
	  		  (mkTaskWithShared (iid, googleMapsTasklet 52.8825984009408 4.74849700927734) state updateFun @> (mapWp,state)) 
			  -||-
			  viewSharedInformation "The waypoints are" [] state)

updateFun (wps,plane) st = {st & waypoints = wps, plane = plane}
mapWp (Value wpp _) _ = Just wpp

tasklet4
	= 		mkInstanceId >>= \iid ->
	 		mkTask (iid, googleMapsTasklet 52.8825984009408 4.74849700927734) 
	 		>>= \v -> viewInformation "The result is" [] v       
							 
//Simulate the movement of the airplane
//First very simple simulation. Just round robin version of clock
//simulateAirplanePosition :: (Shared ([(!Real,!Real)],MovingEntity)) -> Task (LatLng,Int) //Position and heading
//simulateAirplanePosition (route,plane)
//	= withShared (0,0) (\state ->
//	      forever (wait SIMULATE_INTERVAL >>- update newPlanePosition (state >+| ())

//	newPlanePosition2 :: ((MovingEntity,[(Int,Real,Real)]),(Int,Int)) -> (MovingEntity,Int,Int)
//	newPlanePosition2 ((plane,[]),(pos,time))  = ({plane&timeLate = time},(pos,time + 1))
//	newPlanePosition2 ((plane,route),(pos,time)) 
//	# (plane,pos) = moveAlongWayPoints plane (map f route) pos time
//	= ((plane,route),(pos,time+1))
//	where f = fromDegrees o g
//	      g (_,la,lo) = (la,lo)

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
    
Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist taskletExamples)) world
