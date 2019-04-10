module LeafletMapExample
import iTasks
import iTasks.Extensions.GIS.Leaflet
import iTasks.Extensions.GIS.LeafletNavalIcons
import iTasks.UI.Definition
import Data.List

playWithMaps :: Task ()
playWithMaps = withShared {defaultValue & icons = shipIcons} (\m ->
	(allTasks [managePerspective m, manageMapObjects m])
	-&&-
	manipulateMap m
	) <<@ ArrangeWithSideBar 0 LeftSide 600 True @! ()

manipulateMap :: (Shared sds LeafletMap) -> Task () | RWShared sds
manipulateMap m = updateSharedInformation () [] m
	<<@ ApplyLayout (layoutSubUIs (SelectByPath [1]) (setUIAttributes (sizeAttr FlexSize FlexSize))) @! ()

managePerspective :: (Shared sds LeafletMap) -> Task () | RWShared sds
managePerspective m = updateSharedInformation (Title "Perspective") [] (mapReadWrite (\x -> x.LeafletMap.perspective,\p x -> Just {x & perspective = p}) Nothing m)  @! ()

// objects can currently only be viewed, as the editor for `HtmlTag` only works in view mode
manageMapObjects :: (Shared sds LeafletMap) -> Task () | RWShared sds
manageMapObjects m = viewSharedInformation (Title "View objects") [ViewAs toPrj] m
				   -|| addDemoObjects m
				   @! ()
where
	toPrj m = m.LeafletMap.objects

	addDemoObjects m
		= enterChoiceAs "Add objects:" [ChooseFromCheckGroup fst] options snd
		>^* [OnAction (Action "Add") (hasValue id)]
	where
	 	options =
			[("Random marker",addRandomMarker m)
			,("Marker at cursor position",addMarkerAtCursor m)
			,("Line connecting current markers",addMarkerConnectingLine m)
			,("Polygon from current markers",addMarkerConnectingPolygon m)
			,("Circle at cursor position",addCircleAtCursor m)
			,("Rectangle around current perspective",addRectangleAroundCurrentPerspective m)
			]

	addRandomMarker m
		= 	get randomInt -&&- get randomInt @ toRandomMarker
		>>- \marker -> upd (\l=:{LeafletMap|objects} -> {LeafletMap|l & objects = objects ++ [marker]}) m

	toRandomMarker (rLat,rLng)
		= Marker {markerId = LeafletObjectID markerId, position= {LeafletLatLng|lat = lat, lng = lng}, title = Just markerId, icon = Just icon, selected = False, popup = Nothing}
	where
		lat = 52.0 + (toReal (500 + (rLat rem 1000)) / 1000.0)
		lng = 6.0 + (toReal (500 + (rLng rem 1000)) / 1000.0)
		markerId = "RANDOM-" <+++ rLat <+++ rLng
		icon = shipIconId (Just (rLat rem 360)) OrangeShip False

	addMarkerConnectingLine m
		= upd (\l=:{LeafletMap|objects} -> {LeafletMap|l & objects = objects ++ [line objects]}) m
	where
		line objects = Polyline { polylineId = LeafletObjectID "markerConnection"
                                , style      = [Style (LineStrokeColor "#f0f"), Style (LineStrokeWidth 4)]
                                , points     = points objects
                                , editable   = True
                                }
		points objects = [position \\ Marker {LeafletMarker|position} <- objects]

	addMarkerConnectingPolygon m
		= upd (\l=:{LeafletMap|objects} -> {LeafletMap|l & objects = objects ++ [polygon objects]}) m
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
		= upd (\l=:{LeafletMap|perspective={LeafletPerspective|cursor},objects} -> {LeafletMap|l & objects = withMarkerFromCursor cursor objects}) m
	where
		withMarkerFromCursor Nothing objects = objects
		withMarkerFromCursor (Just position) objects = objects ++ [Marker {markerId = LeafletObjectID "CURSOR", position= position, title = Nothing, icon = Nothing, selected = False, popup = Nothing}]

	addCircleAtCursor m
		= upd (\l=:{LeafletMap|perspective={LeafletPerspective|cursor},objects} -> {LeafletMap|l & objects = withCircleFromCursor cursor objects}) m
	where
		withCircleFromCursor Nothing objects = objects
		withCircleFromCursor (Just position) objects = objects ++ [Circle {circleId = LeafletObjectID "CIRCLE_CURSOR", center = position, radius = 100000.0, editable = True, style = []}]

	addRectangleAroundCurrentPerspective m
		= upd (\l=:{LeafletMap|perspective={LeafletPerspective|bounds},objects} -> {LeafletMap|l & objects = withRectangleAroundCurrentPerspective bounds objects}) m
	where
		withRectangleAroundCurrentPerspective Nothing objects = objects
		withRectangleAroundCurrentPerspective (Just bounds) objects = objects ++ [Rectangle {rectangleId = LeafletObjectID "RECT_PERSPECTIVE", bounds = bounds, editable = True, style = []}]

Start world = doTasks playWithMaps world
