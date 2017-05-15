module LeafletMapExample
import iTasks
import iTasks.API.Extensions.GIS.Leaflet
import iTasks.UI.Definition
import Data.List 

playWithMaps :: Task ()
playWithMaps = withShared defaultValue (\m ->
	(allTasks [managePerspective m, manageMapObjects m])
	-&&-
	manipulateMap m
	) <<@ ArrangeWithSideBar 0 LeftSide 600 True @! () 

manipulateMap :: (Shared LeafletMap) -> Task ()
manipulateMap m = updateSharedInformation () [] m 
	<<@ ApplyLayout (layoutSubUIs (SelectByPath [1]) (setUIAttributes (sizeAttr FlexSize FlexSize))) @! ()

managePerspective :: (Shared LeafletMap) -> Task ()
managePerspective m = updateSharedInformation (Title "Perspective") [] (mapReadWrite (\x -> x.LeafletMap.perspective,\p x -> Just {x & perspective = p}) m)  @! ()

manageMapObjects :: (Shared LeafletMap) -> Task ()
manageMapObjects m = updateSharedInformation (Title "Manage objects") [UpdateAs toPrj fromPrj] m
				   -|| addDemoObjects m
				   @! ()
where
	toPrj m = m.LeafletMap.objects
	fromPrj m objects = {m & objects = objects}

	addDemoObjects m 
		= enterChoiceAs "Add objects:" [ChooseFromCheckGroup fst] options snd
		>^* [OnAction (Action "Add") (hasValue id)]
	where
	 	options =
			[("Random marker",addRandomMarker m)
			,("Marker at cursor position",addMarkerAtCursor m)
			,("Line connecting current markers",addMarkerConnectingLine m)
			,("Polygon from current markers",addMarkerConnectingPolygon m)
			]

	addRandomMarker m
		= 	get randomInt -&&- get randomInt @ toRandomMarker
		>>- \marker -> upd (\l=:{LeafletMap|objects} -> {LeafletMap|l & objects = objects ++ [marker]}) m 

	toRandomMarker (rLat,rLng)
		= Marker {markerId = markerId, position= {LeafletLatLng|lat = lat, lng = lng}, title = Just markerId, icon = Nothing, selected = False}
	where
		lat = 52.0 + (toReal (500 + (rLat rem 1000)) / 1000.0)
		lng = 6.0 + (toReal (500 + (rLng rem 1000)) / 1000.0)
		markerId = "RANDOM-" <+++ rLat <+++ rLng

	addMarkerConnectingLine m
		= upd (\l=:{LeafletMap|objects} -> {LeafletMap|l & objects = objects ++ [line objects]}) m
	where
		line objects = Polyline {polylineId = "markerConnection", strokeColor = "#f0f", strokeWidth = 4, points = points objects}
		points objects = [position \\ Marker {LeafletMarker|position} <- objects]

	addMarkerConnectingPolygon m
		= upd (\l=:{LeafletMap|objects} -> {LeafletMap|l & objects = objects ++ [polygon objects]}) m
	where
		polygon objects = Polygon {polygonId = "markerConnection", strokeColor = "#000", strokeWidth = 2, fillColor = Just "#0f0", points = points objects}
		points objects = [position \\ Marker {LeafletMarker|position} <- objects]

	addMarkerAtCursor m
		= upd (\l=:{LeafletMap|perspective={LeafletPerspective|cursor},objects} -> {LeafletMap|l & objects = withMarkerFromCursor cursor objects}) m
	where
		withMarkerFromCursor Nothing objects = objects
		withMarkerFromCursor (Just position) objects = objects ++ [Marker {markerId = "CURSOR", position= position, title = Nothing, icon = Nothing, selected = False}]

Start world = startEngine playWithMaps world
