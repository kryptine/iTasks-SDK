module LeafletMapExample
import iTasks
import iTasks.Extensions.GIS.Leaflet
import iTasks.Extensions.GIS.LeafletNavalIcons
import iTasks.UI.Definition
import StdFunctions, Data.List, Text.HTML

playWithMaps :: Task ()
playWithMaps = withShared ({defaultValue & icons = shipIcons},defaultValue) (\m ->
	((allTasks [managePerspective m, manageState m, manageMapObjects m]) <<@ ScrollContent)
	-&&-
	manipulateMap m
	) <<@ ArrangeWithSideBar 0 LeftSide True @! ()

derive gDefault LeafletSimpleState, LeafletObjectID
manipulateMap :: (Shared sds (LeafletMap,LeafletSimpleState)) -> Task () | RWShared sds
manipulateMap m = updateSharedInformation [UpdateSharedUsing id (flip const) const (customLeafletEditor eventHandlers defaultValue)] m
	<<@ ApplyLayout (setUIAttributes (sizeAttr FlexSize FlexSize)) @! ()
where
	eventHandlers = {simpleStateEventHandlers & onHtmlEvent = onHtmlEvent}

	onHtmlEvent "closewindows" (l,s) = ({LeafletMap|l & objects = [o \\ o <- l.LeafletMap.objects | not (o =: (Window _))]},s)
	onHtmlEvent _ (l,s) = (l,s)

managePerspective :: (Shared sds (LeafletMap,LeafletSimpleState)) -> Task () | RWShared sds
managePerspective m = Title "Perspective" @>> updateSharedInformation  [] 
	(mapReadWrite (\(x,s) -> x.LeafletMap.perspective, \p (x,s) -> Just ({x & perspective = p},s)) Nothing m) @! ()

manageState :: (Shared sds (LeafletMap,LeafletSimpleState)) -> Task () | RWShared sds
manageState m = Title "State" @>> updateSharedInformation  [] 
	(mapReadWrite (\(x,s) -> s, \sn (x,s) -> Just (x,sn)) Nothing m) @! ()

// objects can currently only be viewed, as the editor for `HtmlTag` only works in view mode
manageMapObjects :: (Shared sds (LeafletMap,LeafletSimpleState)) -> Task () | RWShared sds
manageMapObjects m = Title "View objects" @>> viewSharedInformation [ViewAs toPrj] m
				   -|| addDemoObjects m
				   @! ()
where
	toPrj (m,_) = m.LeafletMap.objects

	addDemoObjects m
		=  Hint "Add objects:" @>> enterChoiceAs [ChooseFromCheckGroup fst] options snd
		>^* [OnAction (Action "Add") (hasValue id)]
	where
	 	options =
			[("Random marker",addRandomMarker m)
			,("Marker at cursor position",addMarkerAtCursor m)
			,("Line connecting current markers",addMarkerConnectingLine m)
			,("Polygon from current markers",addMarkerConnectingPolygon m)
			,("Circle at cursor position",addCircleAtCursor m)
			,("Rectangle around current perspective",addRectangleAroundCurrentPerspective m)
			,("Some window",addWindow m)
			]

	addRandomMarker m
		= 	get randomInt -&&- get randomInt @ toRandomMarker
		>>- \marker -> upd (\(l=:{LeafletMap|objects},s) -> ({LeafletMap|l & objects = objects ++ [marker]},s)) m

	toRandomMarker (rLat,rLng)
		= Marker {markerId = LeafletObjectID markerId, position= {LeafletLatLng|lat = lat, lng = lng}, title = Just markerId, icon = Just icon, popup = Nothing}
	where
		lat = 52.0 + (toReal (500 + (rLat rem 1000)) / 1000.0)
		lng = 6.0 + (toReal (500 + (rLng rem 1000)) / 1000.0)
		markerId = "RANDOM-" <+++ rLat <+++ rLng
		icon = shipIconId (Just (rLat rem 360)) OrangeShip False

	addMarkerConnectingLine m
		= upd (\(l=:{LeafletMap|objects},s) -> ({LeafletMap|l & objects = objects ++ [line objects]},s)) m
	where
		line objects = Polyline { polylineId = LeafletObjectID "markerConnection"
                                , style      = [Style (LineStrokeColor "#f0f"), Style (LineStrokeWidth 4)]
                                , points     = points objects
                                , editable   = True
                                }
		points objects = [position \\ Marker {LeafletMarker|position} <- objects]

	addMarkerConnectingPolygon m
		= upd (\(l=:{LeafletMap|objects},s) -> ({LeafletMap|l & objects = objects ++ [polygon objects]},s)) m
	where
		polygon objects = Polygon { polygonId = LeafletObjectID "markerConnection"
                                  , style     = [ Style (AreaLineStrokeColor "#000")
                                                , Style (AreaLineStrokeWidth 2)
                                                , Style (AreaFillColor "#0f0")
                                                ]
                                  , points    = points objects
                                  , editable  = True
                                  }
		points objects = [position \\ Marker {LeafletMarker|position} <- objects]

	addMarkerAtCursor m
		= upd (\(l=:{LeafletMap|objects},s=:{LeafletSimpleState|cursor}) -> ({LeafletMap|l & objects = withMarkerFromCursor cursor objects},s)) m
	where
		withMarkerFromCursor Nothing objects = objects
		withMarkerFromCursor (Just position) objects = objects ++ [Marker {markerId = LeafletObjectID "CURSOR", position= position, title = Nothing, icon = Nothing, popup = Nothing}]

	addCircleAtCursor m
		= upd (\(l=:{LeafletMap|objects},s=:{LeafletSimpleState|cursor}) -> ({LeafletMap|l & objects = withCircleFromCursor cursor objects},s)) m
	where
		withCircleFromCursor Nothing objects = objects
		withCircleFromCursor (Just position) objects = objects ++ [Circle {circleId = LeafletObjectID "CIRCLE_CURSOR", center = position, radius = 100000.0, editable = True, style = []}]

	addRectangleAroundCurrentPerspective m
		= upd (\(l=:{LeafletMap|perspective={LeafletPerspective|bounds},objects},s) -> ({LeafletMap|l & objects = withRectangleAroundCurrentPerspective bounds objects},s)) m
	where
		withRectangleAroundCurrentPerspective Nothing objects = objects
		withRectangleAroundCurrentPerspective (Just bounds) objects = objects ++ [Rectangle {rectangleId = LeafletObjectID "RECT_PERSPECTIVE", bounds = bounds, editable = True, style = []}]

	addWindow m
		= upd (\(l=:{LeafletMap|objects},s) -> ({LeafletMap| l & objects = [Window window:objects]},s)) m
	where
		window =
			{ windowId       = LeafletObjectID "WINDOW"
			, initPosition   = {x = 100, y = 100}
			, title          = "Test Window"
			, content        = DivTag []
				[H1Tag [] [Text "This is test content!"]
				,ATag [HrefAttr "#",OnclickAttr "itasks.htmlEvent(event, 'closewindows')"] [Text "Close windows"]
				]
			, relatedMarkers = [(LeafletObjectID "home", [])]
			}

Start world = doTasks playWithMaps world
