definition module InteractionTasks

from TSt		import :: Task
from Types		import :: Role
from Html		import :: HtmlTag
from iTasks		import class iTask(..)
from ProcessDB	import :: Action
import GenVisualize, GenUpdate, GenMerge, StoreTasks

//*** Input collection tasks ***//

/*
* Ask the user to enter information.
*
* @param String				A short descriptive subject
* @param description 		A description of the task to display to the user
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. If not specified
*							a default button is rendered.
*
* @return 					A task-form which allows the user to enter information
*/
enterInformation			:: !String !description -> Task a											| html description & iTask a
enterInformationA			:: !String !description ![TaskAction a] -> Task (!Action,!a)				| html description & iTask a

/*
* Ask the user to update predefined information. 
*
* @param String				A short descriptive subject
* @param description 		A description of the task to display to the user
* @param a					The predefined value of the task
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. If not specified
*							a default button is rendered.
*
* @return 					A task-form which allows the user to enter information
*/
updateInformation			:: !String !description a -> Task a											| html description & iTask a
updateInformationA			:: !String !description ![TaskAction a] a  -> Task (!Action,!a)				| html description & iTask a

/*
* Ask the user to enter information, given some additional context information
*
* @param String				A short descriptive subject
* @param description 		A description of the task to display to the user
* @param b					Additional context information
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. If not specified
*							a default button is rendered.
*
* @return 					A task-form which allows the user to enter information and displays the context information
*/
enterInformationAbout		:: !String !description  b -> Task a										| html description & iTask a & iTask b
enterInformationAboutA		:: !String !description ![TaskAction a] b -> Task (!Action,!a)				| html description & iTask a & iTask b

/*
* Ask the user to update predefined information, given some additonal context information
*
* @param String				A short descriptive subject
* @param description 		A description of the task to display to the user
* @param b					Additional context information
* @param a					The predefined value of the task
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. If not specified
*							a default button is rendered.
*
* @return 					A task-form which allows the user to enter information and displays the context information
*/
updateInformationAbout		:: !String !description b a -> Task a										| html description & iTask a & iTask b
updateInformationAboutA		:: !String !description ![TaskAction a] b a  -> Task (!Action,!a)			| html description & iTask a & iTask b

/*
* Asks the user to confirm or decline a question.
*
* @param String				A short descriptive subject
* @param description 		A description of the task to display to the user
*
* @return 					A boolean indicating 'accepted' (True) or 'declined' (False)
*/
requestConfirmation			:: !String !description -> Task Bool										| html description

/*
* Asks the user to accept or decline a question, given some additional context information
*
* @param String				A short descriptive subject
* @param description 		A description of the task to display to the user
* @param a					Additional context information
*
* @return 					A boolean indiciating 'accepted' (True) or 'declined' (False) and displays the context information
*/
requestConfirmationAbout	:: !String !description a -> Task Bool										| html description & iTask a

/*
* Ask the user to select one item from a list of options
*
* @param String				A short descriptive subject
* @param description 		A description of the task to display to the user
* @param [a]				A list of options
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. If not specified
*							a default button is rendered.
*
* @return 					A task-form which allows the user to choose an item
*/
enterChoice					:: !String !description [a] -> Task a										| html description & iTask a
enterChoiceA				:: !String !description ![TaskAction a] [a] -> Task (!Action,!a)			| html description & iTask a

/*
* Ask the user to select one item from a list of options with already one option pre-selected
*
* @param String				A short descriptive subject
* @param description 		A description of the task to display to the user
* @param [a]				A list of options
* @param Int				The index of the item which should be pre-selected
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. If not specified
*							a default button is rendered.
*
* @return 					A task-form which allows the user to choose an item
*/
updateChoice				:: !String !description [a] Int -> Task a									| html description & iTask a
updateChoiceA 				:: !String !description ![TaskAction a] [a] Int -> Task (!Action,!a)		| html description & iTask a 

/*
* Ask the user to select one item from a list of options, given some context information
*
* @param String				A short descriptive subject
* @param description 		A description of the task to display to the user
* @param b					Additional context information
* @param [a]				A list of options
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. If not specified
*							a default button is rendered.
*
* @return 					A task-form which allows the user to choose an item and displays the context information
*/
enterChoiceAbout			:: !String !description b [a] -> Task a										| html description & iTask a & iTask b
enterChoiceAboutA			:: !String !description ![TaskAction a] b [a] -> Task (!Action,!a)			| html description & iTask a & iTask b

/*
* Ask the user to select one item from a list of options with already one option pre-selected, given some context information
*
* @param String				A short descriptive subject
* @param description 		A description of the task to display to the user
* @param b					Additional context information
* @param [a]				A list of options
* @param Int				The index of the item which should be pre-selected
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. If not specified
*							a default button is rendered.
*
* @return 					A task-form which allows the user to choose an item and displays the context information
*/
updateChoiceAbout			:: !String !description b [a] Int -> Task a									| html description & iTask a & iTask b
updateChoiceAboutA			:: !String !description ![TaskAction a] b [a] Int -> Task (!Action,!a)		| html description & iTask a & iTask b

/*
* Ask the user to select one or more items from a list of options
*
* @param String				A short descriptive subject
* @param description 		A description of the task to display to the user
* @param [a]				A list of options
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. If not specified
*							a default button is rendered.
*
* @return 					A task-form which allows the user to choose items
*/
enterMultipleChoice			:: !String !description [a] -> Task [a]										| html description & iTask a
enterMultipleChoiceA		:: !String !description ![TaskAction [a]] [a] -> Task (!Action,![a])		| html description & iTask a

/*
* Ask the user to select one or more items from a list of options with already some options pre-selected
*
* @param String				A short descriptive subject
* @param description 		A description of the task to display to the user
* @param [a]				A list of options
* @param Int				The index of the item which should be pre-selected
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. If not specified
*							a default button is rendered.
*
* @return 					A task-form which allows the user to choose items
*/
updateMultipleChoice		:: !String !description [a] [Int] -> Task [a]								| html description & iTask a
updateMultipleChoiceA		:: !String !description ![TaskAction [a]] [a] [Int] -> Task (!Action,![a])	| html description & iTask a

/*
* Ask the user to select one or more items from a list of options, given additional context information
*
* @param String				A short descriptive subject
* @param description 		A description of the task to display to the user
* @param b					Additional context information
* @param [a]				A list of options
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. If not specified
*							a default button is rendered.
*
* @return 					A task-form which allows the user to choose items and displays the context information
*/
enterMultipleChoiceAbout	:: !String !description b [a] -> Task [a]									| html description & iTask a & iTask b
enterMultipleChoiceAboutA	:: !String !description ![TaskAction [a]] b [a] -> Task (!Action,![a])		| html description & iTask a & iTask b

/*
* Ask the user to select one or more items from a list of options with already some options pre-selected, given additional context information
*
* @param String				A short descriptive subject
* @param description 		A description of the task to display to the user
* @param b					Additional context information
* @param [a]				A list of options
* @param Int				The index of the item which should be pre-selected
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. If not specified
*							a default button is rendered.
*
* @return 					A task-form which allows the user to choose items and displays the context information
*/
updateMultipleChoiceAbout	:: !String !description b [a] [Int] -> Task [a]								| html description & iTask a & iTask b
updateMultipleChoiceAboutA	:: !String !description ![TaskAction [a]] b [a] [Int] -> Task (!Action,![a])| html description & iTask a & iTask b

//*** Output tasks ***//

/*
* Show a basic message to the user. The user can end the task after reading the message. 

* @param String				A short descriptive subject
* @param message 			A message to display to the user
* @param [TaskAction a] 	A list of buttons or menus through which the user can submit the value. If not specified
*							a default button is rendered
*
* @return					a or an Action if [TaskAction a] is specified
*/
showMessage					:: !String !message a -> Task a										| html message & iTask a
showMessageA				:: !String !message ![TaskAction a] a -> Task (!Action,!a)			| html message & iTask a

/*
* Show a basic message and additional context information to the user. The user can end the task after reading the message. 
*
* @param String				A short descriptive subject
* @param message			The message to display to the user
* @param a					Additional context information
* @param [TaskAction a] 	A list of buttons or menus through which the user can submit the value. If not specified
*							a default button is rendered
*
* @return					a or an Action if [TaskAction a] is specified
*/
showMessageAbout			:: !String !message a -> Task a										| html message & iTask a
showMessageAboutA			:: !String !message ![TaskAction a] a -> Task (!Action,!a)			| html message & iTask a

/*
* Show a basic message to the user. The user cannot end the task after reading the message. 
*
* @param String				A short descriptive subject
* @param message			The message to display to the user
* @param a					The value that is returned when the task is finished
*
* @return					a
*/
showStickyMessage			:: !String !message a -> Task a										| html message & iTask a

/*
* Show a basic message and some context information to the user. The user cannot end the task after reading the message. 
*
* @param String				A short descriptive subject
* @param message			The message to display to the user
* @param a					Additional context information
*
* @return					a
*/
showStickyMessageAbout		:: !String !message a -> Task a										| html message & iTask a


//*** Instruction tasks ***//

/* 
* Shows a instruction to the user. The user can dismiss the instruction.
*
* @param String				A short descriptive subject
* @param instruction		The instruction
* @param a					The value that is returned when the task is finished		
*
* @return					a
*/
showInstruction 			:: !String !instruction	a	-> Task a								| html instruction & iTask a

/* 
* Shows a instruction and additional context information to the user. The user can dismiss the instruction.
*
* @param String				A title message
* @param instruction		The instruction
* @param a					Additional context information
*
* @return					a
*/
showInstructionAbout 		:: !String !instruction a 	-> Task a								| html instruction & iTask a

//*** Shared variable tasks ***//
class SharedVariable a | gMerge{|*|} a

:: Editor s a	= {editorFrom :: s -> a, editorTo :: a s -> s}
:: Listener s a	= {listenerFrom :: s -> a}
:: View s

listener	:: !(Listener s a)	-> View s | iTask a & iTask s & SharedVariable s
editor		:: !(Editor s a)	-> View s | iTask a & iTask s & SharedVariable s

idEditor	:: View s	| iTask s & SharedVariable s
idListener	:: View s	| iTask s & SharedVariable s

updateShared			:: question ![TaskAction s] !(DBid s) ![View s] -> Task (!Action, !s)	| html question & iTask s & SharedVariable s
updateSharedLocal		:: question ![TaskAction s] !s ![View s] -> Task (!Action, !s)			| html question & iTask s & SharedVariable s

// To allow users to specify a followup action to their current task
// most interactive tasks allow you to specify actions that can be chosen.
// These actions are either available as a button on the bottom of the task interface
// or as an item in the task menu, or both.
// Additionally conditions can be specified when the action is allowed to be performed.

:: TaskAction a 		= 	ButtonAction		!(!Action, ActionCondition a) 
						| 	MenuAction			!(!Action, ActionCondition a)
						| 	ButtonAndMenuAction !(!Action, ActionCondition a) 
						| 	MenuParamAction		!(!String, ActionCondition a)

:: ActionCondition a 	= 	Always 
						| 	IfValid 
						| 	Predicate ((EditorValue a) -> Bool)

:: EditorValue a 		= 	Invalid 
						| 	Valid !a

// This html class makes it possible to use either strings, or html as description/message/instruction
class html a  
where
	html :: a -> [HtmlTag]
	
instance html String
instance html [HtmlTag]