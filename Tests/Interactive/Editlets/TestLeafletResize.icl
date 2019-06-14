module TestLeafletResize
import iTasks
import iTasks.Extensions.GIS.Leaflet

test =  (viewInformation "Map resizing" [] {LeafletMap|perspective=defaultValue,objects=objects,tilesUrls=[],icons=[]} <<@ FlexInner <<@ AddCSSClass "itasks-flex-height")
	-|| (updateInformation "List to force resizing" [] [1,2,3,4] <<@ AddCSSClass "itasks-wrap-height")

where
	objects = [Polygon {polygonId = LeafletObjectID "poly", points = points,style=[], editable = True}]
	points = [{LeafletLatLng|lat=52.0,lng=7.0},{LeafletLatLng|lat=54.0,lng=7.0},{LeafletLatLng|lat=52.0,lng=5.0}]

FlexInner :== ApplyLayout (layoutSubUIs (SelectByPath [1]) (setUIAttributes (heightAttr FlexSize)))

Start world = doTasks test world
