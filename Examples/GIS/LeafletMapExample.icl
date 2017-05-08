module LeafletMapExample
import iTasks
import iTasks.API.Extensions.GIS.Leaflet
import iTasks.UI.Definition
import Data.List 

playWithMaps :: Task ()
playWithMaps = withShared defaultValue (\m ->
	(allTasks [viewPerspective m, manageMapObjects m,manageMapLayers m])
	-&&-
	manipulateMap m
	) <<@ ArrangeWithSideBar 0 LeftSide 300 True @! () 

manipulateMap :: (Shared LeafletMap) -> Task ()
manipulateMap m = updateSharedInformation () [] m 
	<<@ ApplyLayout (layoutSubUIs (SelectByPath [1]) (setUIAttributes (sizeAttr FlexSize FlexSize))) @! ()

viewPerspective :: (Shared LeafletMap) -> Task ()
viewPerspective m = viewSharedInformation (Title "Perspective") [] (mapRead (\x -> x.LeafletMap.perspective) m)  @! ()

manageMapObjects :: (Shared LeafletMap) -> Task ()
manageMapObjects m = viewInformation (Title "Manage objects") [] ()

manageMapLayers :: (Shared LeafletMap) -> Task ()
manageMapLayers m = viewInformation (Title "Manage layers") [] ()

Start world = startEngine playWithMaps world
