module MessageTest

import iTasks
import CommonDomain

derive bimap Maybe,(,)

derive gPrint Typetest, Test, RecA, RecB
derive gVisualize Typetest, Test, RecA, RecB
derive gParse Typetest, Test, RecA, RecB
derive gUpdate Typetest, Test, RecA, RecB
derive gMakeLocalCopy Typetest, Test, RecA, RecB
derive gMakeSharedCopy Typetest, Test, RecA, RecB
derive gMerge Typetest, Test, RecA, RecB
derive gError Typetest, Test, RecA, RecB
derive gHint Typetest, Test, RecA, RecB

enter :: Task Int
enter = enterInformation "Enter a number"

instructionAbout :: Task Void
instructionAbout = enter >>= \v -> showInstructionAbout "Test Instruction 2" "Please speak this number out loud" v

:: Rec = 
	{ hidden 	:: Hidden String
	, html		:: HtmlDisplay String
	, editable	:: Editable Document
	, normal	:: Int
	, list		:: Editable [HtmlDisplay Rec]
	, tuple		:: (Editable Int, String)
	}
	
:: Typetest =
	{ hidden	:: Hidden Test
	, static	:: HtmlDisplay Test
	, normal	:: (String,Int,Int)
	, editable	:: Editable Test
	}

typestest :: Task ( Typetest)
typestest = enterInformation "Value" >>= \val ->
										updateInformation "Update" ( {Typetest 
													| hidden = Hidden val
													, static = HtmlDisplay val
													, editable = Editable val
													, normal = ("Test",0,4)
													}
										)

tupletest :: Task ([String],String,String)
tupletest = enterInformation "Tuple"

:: Test = Con1 Int | Con2 | Con3 String | Con4 Int String

:: RecA = 
	{ test	:: Maybe Test
	, record :: RecB
	}
	
:: RecB = 
	{ fieldA :: Date
	, fieldB :: String
	}

consTest :: Task (Maybe Test)
consTest = enterInformation "Cons"

recordtest :: Task RecA
recordtest = enterInformation "Record"

sharedTest :: Task Void
sharedTest = ignoreResult (updateSharedLocal "Shared" [quitButton] ({Date | day = 29, mon = 10, year = 1982}) [idEditor,idEditor])

sharedTest2 :: Task Void
sharedTest2 = ignoreResult (updateSharedLocal "Shared" [quitButton] {fieldA = {Date | day = 29, mon = 10, year = 1982}, fieldB = "B"} [idEditor,staticDisplay])

staticDisplay = editor {editorFrom = \val -> HtmlDisplay val, editorTo = \(HtmlDisplay v) _ -> v}
quitButton = ButtonAction (ActionQuit, Always)
	
Start :: *World -> *World
Start world = startEngine [
			workflow "Constructor Test" consTest,
			workflow "Display Instruction" (showInstruction "Test Instruction" "Please perform the following.."),
			workflow "Display Instruction About" instructionAbout,
			workflow "Types Test" typestest,
			workflow "Record Test" recordtest,
			workflow "Tuple Test" tupletest,
			workflow "Shared Test" sharedTest,
			workflow "Shared Test 2" sharedTest2
		] world 