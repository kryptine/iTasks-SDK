definition module InteractionTasks
/*
* This module provides means to interact with users
*/
from TSt		import :: Task
from Types		import :: Role
from Html		import :: HtmlTag
from iTasks		import class iTask(..)

import GenVisualize, GenUpdate, GenMerge, StoreTasks

derive gVisualize Action
derive gUpdate Action
derive gVerify Action

derive JSONEncode Action
derive JSONDecode Action

instance == Action

/*
* To allow users to specify a followup action to their current task
* most interactive tasks allow you to specify actions that can be chosen.
* These actions are either available as a button on the bottom of the task interface
* or as an item in the task menu, or both.
* Additionally conditions can be specified when the action is allowed to be performed.
*/
:: Action	= Action !ActionName !ActionLabel 
			| ActionParam !ActionName !ActionLabel !ActionData 
			| ActionOk
			| ActionCancel
			| ActionYes
			| ActionNo
			| ActionNext
			| ActionPrevious
			| ActionFinish
			| ActionNew
			| ActionOpen
			| ActionSave
			| ActionSaveAs
			| ActionQuit
			| ActionClose
			| ActionHelp
			| ActionAbout
			| ActionFind
			| ActionDelete
			| ActionEdit
			
:: ActionName	:== String 	//Locally unique identifier for actions
:: ActionLabel	:== String	//Textual label for the action
:: ActionData	:== String	//Extra data to pass along with an action

:: Menus		:== [Menu]
:: Menu 		= Menu !String ![MenuItem]
:: MenuItem 	= SubMenu !String ![MenuItem] 
				| MenuItem !String !Action !(Maybe Hotkey)
				| MenuSeparator 
				
:: Hotkey =	{ key	:: !Key
			, ctrl	:: !Bool
			, alt	:: !Bool
			, shift	:: !Bool
			}
			
:: Key = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
	
actionIcon 	:: !Action -> String
actionLabel	:: !Action -> String
actionName	:: !Action -> String

// This triple is used to link actions to user interfaces.
// Its three parts represent the (what , when, where) aspects of actions.
// What: The conceptual action to be taken
// When: The conditions that determine if the action can be taken
// Where: The place of an actions gui component in the interface (button, menu or both)
:: TaskAction a		:== (!Action, !(Verified a) -> Bool, !ActionPlacement)

// Locations for placing an action
:: ActionPlacement	= AsButton | InMenu

//Wrapper for task values that indicates if value passes the verification step
:: Verified a 		= 	Invalid
					|	Valid !a
						
//Default predicates on editor values to use with actions
always		:: (Verified a) -> Bool
ifvalid		:: (Verified a) -> Bool
ifinvalid	:: (Verified a) -> Bool

/*
* This html class makes it possible to use either strings, or html as description/message/instruction
*/
class html a  
where
	html :: a -> HtmlTag
	
instance html String
instance html [HtmlTag]
instance html Note
instance html (Maybe a) | html a

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

/*
* Shared variables tasks allow to specify multiple views upon the same set of data (the share). These views can be either
* viewed in separate tasks, but can also be combined locally into a single editor. Typically a view is created using an instance
* of the Editor- or Listener-type, which specify how the shared data should be transformed into a single View and -in case of an editor-
* back into the original data.
*/
class SharedVariable a | gMerge{|*|} a

:: Editor s a	= {editorFrom :: s -> a, editorTo :: a s -> s}
:: Listener s a	= {listenerFrom :: s -> a}
:: View s

/*
* Creates a view from a Listener specification. Listeners can be used only to view the shared data.
*
* @param (Listener s a)		The specification of the listener-transformation
*
* @return (View s)			A view of the shared data which is read-only
*/
listener	:: !(Listener s a)	-> View s | iTask a & iTask s & SharedVariable s

/*
* Creates a view from an Editor specification. Editors can be used to both view and update the shared data.
*
* @param (Editor s a)		The specification of the editor-transformation
*
* @return (View s)			A view of the shared data
*/
editor		:: !(Editor s a)	-> View s | iTask a & iTask s & SharedVariable s

/*
* Creates a editor-view on the shared data, applying the identity function as transformation.
*
* @return View s			A view of the shared data
*/
idEditor	:: View s	| iTask s & SharedVariable s
idListener	:: View s	| iTask s & SharedVariable s

/*
* Creates a task from a specified set of views using shared data which is stored in a database.
* 
* @param String				A short descriptive subject
* @param question			A description of the task
* @param [TaskAction s]		A list of buttons or menus through which the user can submit the value.
* @param (DBId s)			The database handle of the shared data
* @param [View s]			A list of views on the shared data, which are presented to the user
*
* @return Action			The action the user performed
* @return s					The value/state of the shared data at the time this function is evaluated
*/
updateShared			:: !String description ![TaskAction s] !(DBId s) ![View s] -> Task (!Action, !s)	| html description & iTask s & SharedVariable s

/*
* Creates a task from a specified set of views using shared data which is only available to this specific task.
* 
* @param String				A short descriptive subject
* @param question			A description of the task
* @param [TaskAction s]		A list of buttons or menus through which the user can submit the value.
* @param [View s]			A list of views on the shared data, which are presented to the user
*
* @return Action			The action the user performed
* @return s					The value/state of the shared data at the time this function is evaluated
*/
updateSharedLocal		:: !String description ![TaskAction s] !s ![View s] -> Task (!Action, !s)			| html description & iTask s & SharedVariable s

