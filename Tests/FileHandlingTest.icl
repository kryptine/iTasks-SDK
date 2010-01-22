module FileHandlingTest

import iTasks

:: Rec =
	{ naam :: String
	, leeftijd :: Int
	, cv :: Document
	, opmerkingen :: Maybe Note
	}

derive gParse Rec
derive gPrint Rec
derive gVisualize Rec
derive gUpdate Rec

docAction :: Task Rec
docAction = enterInformation "Document Test"

docTask :: Task Void
docTask = docAction >>= updateInformation "Please review" >>= showMessageAbout "Result" >>| return Void

intAction :: Task [Int]
intAction = enterInformation "Enter Number List" 

Start :: *World -> *World
Start world = startEngine [ workflow "Document Test" docTask,
						    workflow "Int Test" (intAction >>| return Void)
						  ] world