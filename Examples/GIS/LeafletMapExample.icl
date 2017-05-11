module LeafletMapExample
import iTasks
import iTasks.API.Extensions.GIS.Leaflet
import iTasks.UI.Definition
import Data.List 

playWithMaps :: Task ()
playWithMaps = withShared defaultValue (\m ->
	(allTasks [viewPerspective m, manageMapObjects m])
	-&&-
	manipulateMap m
	) <<@ ArrangeWithSideBar 0 LeftSide 300 True @! () 

manipulateMap :: (Shared LeafletMap) -> Task ()
manipulateMap m = updateSharedInformation () [] m 
	<<@ ApplyLayout (layoutSubUIs (SelectByPath [1]) (setUIAttributes (sizeAttr FlexSize FlexSize))) @! ()

viewPerspective :: (Shared LeafletMap) -> Task ()
viewPerspective m = viewSharedInformation (Title "Perspective") [] (mapRead (\x -> x.LeafletMap.perspective) m)  @! ()

manageMapObjects :: (Shared LeafletMap) -> Task ()
manageMapObjects m = updateSharedInformation (Title "Manage objects") [UpdateAs toPrj fromPrj] m @! ()
where
	toPrj m = m.LeafletMap.objects
	fromPrj m objects = {m & objects = objects}

Start world = startEngine playWithMaps world
