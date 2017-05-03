implementation module Tests.Interactive.Editlets

import iTasks, TestFramework

testEditletsI :: TestSuite
testEditletsI = testsuite "Editlets" "These tests check if the advanced clientside editors (editlets) work correctly"
	[testEditlet
	,testDashEditlet
    ,testSVGEditlet
    ,testSVGEditletClick
    ,testLeafletMap
    ,testGoogleMap
	,testAceTextArea
	,testAceTextAreaWithShare
	,testAceEditorWithShare
	,testPikadayEditlet
	]

import iTasks.API.Extensions.Clock
testEditlet = itest "Simple clock editlet" "Look at the image below" "You should see a changing interactive clock" tut
where
	tut = viewSharedInformation "Clock" [ViewAs (\t -> AnalogClock t)] currentTime

import iTasks.API.Extensions.Dashboard
testDashEditlet = itest "Another simple editlet" "Look at the image below" "You should see a status LED" tut
where
	tut = viewInformation "LED" [] LightOnRed

import Graphics.Scalable, StdReal
import iTasks.API.Extensions.SVG.SVGEditor
testSVGEditlet = itest "SVG editlet rendering" "Look at the image presented" "You should see the dutch flag" tut
where
	tut = updateInformation "SVG image" [UpdateUsing id (const id) (fromSVGEditor svgeditor)] 42
	svgeditor = {SVGEditor|initView=const (),renderImage = \_ _ _ -> nederland, updView = \m v -> v, updModel = \m v -> m}

	nederland :: Image m
	nederland = banden (H *. 3 /. 2,H) [toSVGColor {r=174,g=28,b=40},toSVGColor "white",toSVGColor {r=33,g=70,b=139}]

	banden (w,h) kleuren = above [] [] [rect w (h /. (length kleuren)) <@< {fill = kleur} <@< {stroke = toSVGColor "none"} \\ kleur <- kleuren] NoHost

	H = px 32.0				
	W = H *. 1.5

import Graphics.Scalable.Internal
testSVGEditletClick = itest "SVG editlet clicks" "Click on the image a couple of times" "The text should update to reflect the number of clicks" tut
where
	tut = updateInformation "SVG Clicks" [UpdateUsing (\m -> m) (\m v -> v) (fromSVGEditor svgeditor)] "No clicks"
		>&> \s -> viewSharedInformation "DEBUG" [] s
	svgeditor = {SVGEditor|initView=id,renderImage = renderImage, updView = \m v -> m, updModel = \m v -> v}

	renderImage :: String String *TagSource -> Image String
	renderImage str _ _
        #! r = rect (px 100.0) (px 100.0)
        #! t = text (normalFontDef "Arial" 10.0) str <@< { fill = toSVGColor "white" }
        = overlay (repeat (AtMiddleX, AtMiddleY)) [] [t] (Host r) <@< { onclick = \n _ -> case n of
                                                                                      1 -> "one click"
                                                                                      2 -> "double click"
                                                                                      n -> toString n +++ " clicks"
                                                                , local = False }

import iTasks.API.Extensions.GIS.Leaflet
testLeafletMap = itest "Leaflet Map" "Try to zoom and pan the map" "You should see a Leaflet Map in which you can pan and zoom" tut
where
	tut :: Task LeafletMap
	tut = enterInformation "Test a Leaflet map" []

import iTasks.API.Extensions.GIS.GoogleMap
testGoogleMap = itest "Google Map" "Try to zoom and pan the map" "You should see a Google Map in which you can pan and zoom" tut
where
	tut :: Task GoogleMap
	tut = enterInformation "Test a Google map" []


import iTasks.API.Extensions.Editors.Ace
testAceTextArea = itest "Ace text area" "Try to edit some text" "You should see an editor with line numbers" tut
where
	tut :: Task String
	tut = testEditor aceTextArea "Hello world" Update

testAceTextAreaWithShare = itest "Ace textarea on share" "Try to edit some text" "You should see an editor with line numbers" tut
where
	tut :: Task String
	tut = testEditorWithShare aceTextArea "Hello world" Update

testAceEditorWithShare = itest "Full-fledged ace editor on share" "Try to edit some text" "You should see an editor with line numbers" tut
where
	tut :: Task (AceOptions,AceState)
	tut = testEditorWithShare aceEditor defaultValue Update

import iTasks.API.Extensions.Form.Pikaday
testPikadayEditlet = itest "Pikaday date picker" "Try to edit a date" "You should see a date picker" tut
where
	tut :: Task String
	tut = testEditorWithShare pikadayField "2017-01-01" Update


