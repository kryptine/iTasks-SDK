implementation module Tests.Interactive.Editlets

import iTasks, TestFramework

testEditlets :: TestSuite
testEditlets = testsuite "Editlets" "These tests check if the advanced clientside editors (editlets) work correctly"
	[testEditlet,testSVGEditlet,testLeafletMap,testGoogleMap]

import iTasks.API.Extensions.Clock
testEditlet = interactive "Simple editlet" "Look at the image below" "You should see a changing interactive clock" tut
where
	tut = viewSharedInformation "Clock" [ViewWith (\t -> AnalogClock t)] currentTime

import Graphics.Scalable, StdReal
import iTasks.API.Extensions.SVG.SVGlet
testSVGEditlet = interactive "SVG editlet rendering" "Look at the image presented" "You should see the dutch flag" tut
where
	tut = viewInformation "SVG image" [imageView (\_ _ -> nederland) (\_ _ -> Nothing)] ()

	nederland :: Image m
	nederland = banden (H *. 3 /. 2,H) [toSVGColor {r=174,g=28,b=40},toSVGColor "white",toSVGColor {r=33,g=70,b=139}]

	banden (w,h) kleuren = above [] [] [rect w (h /. (length kleuren)) <@< {fill = kleur} <@< {stroke = toSVGColor "none"} \\ kleur <- kleuren] Nothing

	H = px 32.0				
	W = H *. 1.5

import iTasks.API.Extensions.GIS.Leaflet
testLeafletMap = interactive "Leaflet Map" "Try to zoom and pan the map" "You should see a Leaflet Map in which you can pan and zoom" tut
where
	tut :: Task LeafletMap
	tut = enterInformation "Test a Leaflet map" []

import iTasks.API.Extensions.GIS.GoogleMap
testGoogleMap = interactive "Google Map" "Try to zoom and pan the map" "You should see a Google Map in which you can pan and zoom" tut
where
	tut :: Task GoogleMap
	tut = enterInformation "Test a Google map" []
