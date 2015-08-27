module InteractiveTests
/**
* This module contains a collection of interactive tests that need to
* be checked interactively by a human tester.
*/
import iTasks

:: TestResult = Pass | Fail !(Maybe Note) //Observed behavior

derive class iTask TestResult

// TEST FRAMEWORK
testInteractive :: !String (Task a) !html !html -> Task TestResult | iTask a & iTask html
testInteractive title testCase instructions expectation
	= 	viewInformation () [] (H1Tag [] [Text title]) 
	||-	((viewInformation (Title "Instructions") [] instructions <<@ ForceLayout)
		  -&&- (viewInformation (Title "Expected result") [] expectation <<@ ForceLayout ) <<@ ArrangeHorizontal )
	||- testCase
	||- enterInformation (Title "Result") []

// CONCRETE TESTCASES
import iTasks.API.Extensions.Clock
testEditlet = testInteractive "Simple editlet" tc "Look at the image below" "You should see a changing interactive clock"
where
	tc = viewSharedInformation "Clock" [ViewWith (\t -> AnalogClock t)] currentTime

import Graphics.Scalable, StdReal
import iTasks.API.Extensions.SVG.SVGlet
testSVGEditlet = testInteractive "SVG editlet rendering" tc "Look at the image presented" "You should see the dutch flag"
where
	tc = viewInformation "SVG image" [imageView (\_ _ -> nederland) (\_ _ -> Nothing)] ()

	nederland :: Image m
	nederland = banden (H *. 3 /. 2,H) [toSVGColor {r=174,g=28,b=40},toSVGColor "white",toSVGColor {r=33,g=70,b=139}]

	banden (w,h) kleuren = above [] [] [rect w (h /. (length kleuren)) <@< {fill = kleur} <@< {stroke = toSVGColor "none"} \\ kleur <- kleuren] Nothing

	H = px 32.0				
	W = H *. 1.5

testAll :: Task [TestResult]
testAll = allTasks 
	[testEditlet
	,testSVGEditlet
	]

Start w = startEngine testAll w
