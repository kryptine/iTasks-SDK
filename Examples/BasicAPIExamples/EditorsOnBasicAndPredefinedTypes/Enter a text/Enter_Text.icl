module Enter_Text

// Enter a text string in a text area

import iTasks

Start :: *World -> *World
Start world 
	= startEngine enterText world
	
enterText :: Task String
enterText 
	= 	enterInformation "Enter a text:" [EnterUsing id textArea]
	>>=	viewInformation "You entered:" [ViewUsing id textArea]
	>>=	return
	
