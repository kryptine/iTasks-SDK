module TestLeafletMap
import iTasks
import iTasks.Extensions.GIS.Leaflet

test :: Task LeafletMap
test = enterInformation "Test a Leaflet map" []

Start world = doTasks test world
