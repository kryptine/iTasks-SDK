module MessageTest

import iTasks

derive bimap Maybe,(,)

derive gPrint Rec
derive gVisualize Rec
derive gParse Rec
derive gUpdate Rec

enter :: Task Int
enter = enterInformation "Enter a number"

instructionAbout :: Task Void
instructionAbout = enter >>= \v -> showInstructionAbout "Test Instruction 2" "Please speak this number out loud" v

:: Rec = 
	{ hidden 	:: Hidden Int
	, html		:: HtmlDisplay Int
	, editable	:: Editable Document
	, normal	:: Int
	, list		:: Editable [HtmlDisplay Rec]
	, tuple		:: (Editable Int, String)
	}

typestest :: Task (HtmlDisplay Rec)
typestest = updateInformation "Update" (HtmlDisplay {Rec | normal = 4, hidden = Hidden 1, html = HtmlDisplay 2, editable = Editable emptyDoc,list= Editable [], tuple = (Editable 1, "Test")})

Start :: *World -> *World
Start world = startEngine [
			workflow "Display Instruction" (showInstruction "Test Instruction" "Please perform the following.."),
			workflow "Display Instruction About" instructionAbout,
			workflow "Types Test" typestest
		] world 