module MessageTest

import iTasks

derive bimap Maybe

enter :: Task Int
enter = enterInformation "Enter a number"

instructionAbout :: Task Void
instructionAbout = enter >>= \v -> displayInstructionAbout "Test Instruction 2" "Please speak this number out loud" v

Start :: *World -> *World
Start world = startEngine [
			workflow "Display Instruction" (displayInstruction "Test Instruction" "Please perform the following.."),
			workflow "Display Instruction About" instructionAbout
		] world 