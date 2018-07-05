module TestGoogleMap
import iTasks

import iTasks.Extensions.GIS.GoogleMap

test :: Task GoogleMap
test = enterInformation "Test a Google map" []

Start world = startEngine test world
