definition module InteractionTasks

from TSt		import :: Task
from Types		import :: Role
from Html		import :: HtmlTag
from iTasks		import class iTask(..)
from ProcessDB	import :: Action
import GenPrint, GenParse, GenVisualize, GenUpdate, GenMerge, StoreTasks

// This type class contains types that may be used as
// messages and questions: plain strings and html.
// 
// It is for convenience only, do not create new instances yourself.
// Use the generic visualization instead.

//*** Input tasks ***//

/*
* Ask the user to enter information.
* @param question 		A question to display to the user
* @param [TaskAction a]	A list of buttons or menus, through which the user can submit the value. If not specified
*						a default button is rendered.
*
* @return 				A task-form which allows the user to enter information
*/
enterInformation			:: question -> Task a												| html question & iTask a
enterInformationA			:: question ![TaskAction a] -> Task (!Action,!a)					| html question & iTask a

/*
* Ask the user to update predefined information. 
* @param question 		A question to display to the user
* @param a				The predefined value of the task
* @param [TaskAction a]	A list of buttons or menus, through which the user can submit the value. If not specified
*						a default button is rendered.
*
* @return 				A task-form which allows the user to enter information
*/
updateInformation			:: question a -> Task a												| html question & iTask a
updateInformationA			:: question ![TaskAction a] a  -> Task (!Action,!a)					| html question & iTask a

/*
* Ask the user to enter information, given some additional context information
* @param question 		A question to display to the user
* @param b				Additional context information
* @param [TaskAction a]	A list of buttons or menus, through which the user can submit the value. If not specified
*						a default button is rendered.
*
* @return 				A task-form which allows the user to enter information and displays the context information
*/
enterInformationAbout		:: question b -> Task a												| html question & iTask a & iTask b
enterInformationAboutA		:: question ![TaskAction a] b -> Task (!Action,!a)					| html question & iTask a & iTask b

/*
* Ask the user to update predefined information, given some additonal context information
* @param question 		A question to display to the user
* @param b				Additional context information
* @param a				The predefined value of the task
* @param [TaskAction a]	A list of buttons or menus, through which the user can submit the value. If not specified
*						a default button is rendered.
*
* @return 				A task-form which allows the user to enter information and displays the context information
*/
updateInformationAbout		:: question b a -> Task a											| html question & iTask a & iTask b
updateInformationAboutA		:: question ![TaskAction a] b a  -> Task (!Action,!a)				| html question & iTask a & iTask b

/*
* Asks the user to confirm or decline a question.
* @param question 		A question to display to the user
*
* @return 				A boolean indiciating 'accepted' (True) or 'declined' (False)
*/
requestConfirmation			:: question -> Task Bool											| html question

/*
* Asks the user to accept or decline a question, given some additional context information
* @param question 		A question to display to the user
* @param a				Additional context information
*
* @return 				A boolean indiciating 'accepted' (True) or 'declined' (False) and displays the context information
*/
requestConfirmationAbout	:: question a -> Task Bool											| html question & iTask a

/*
* Ask the user to select one item from a list of options
* @param question 		A question to display to the user
* @param [a]			A list of options
* @param [TaskAction a]	A list of buttons or menus, through which the user can submit the value. If not specified
*						a default button is rendered.
*
* @return 				A task-form which allows the user to choose an item
*/
enterChoice					:: question [a] -> Task a											| html question & iTask a
enterChoiceA				:: question ![TaskAction a] [a] -> Task (!Action,!a)				| html question & iTask a

/*
* Ask the user to select one item from a list of options with already one option pre-selected
* @param question 		A question to display to the user
* @param [a]			A list of options
* @param Int			The index of the item which should be pre-selected
* @param [TaskAction a]	A list of buttons or menus, through which the user can submit the value. If not specified
*						a default button is rendered.
*
* @return 				A task-form which allows the user to choose an item
*/
updateChoice				:: question [a] Int -> Task a										| html question & iTask a
updateChoiceA 				:: question ![TaskAction a] [a] Int -> Task (!Action,!a)			| html question & iTask a 

/*
* Ask the user to select one item from a list of options, given some context information
* @param question 		A question to display to the user
* @param b				Additional context information
* @param [a]			A list of options
* @param [TaskAction a]	A list of buttons or menus, through which the user can submit the value. If not specified
*						a default button is rendered.
*
* @return 				A task-form which allows the user to choose an item and displays the context information
*/
enterChoiceAbout			:: question b [a] -> Task a											| html question & iTask a & iTask b
enterChoiceAboutA			:: question ![TaskAction a] b [a] -> Task (!Action,!a)				| html question & iTask a & iTask b

/*
* Ask the user to select one item from a list of options with already one option pre-selected, given some context information
* @param question 		A question to display to the user
* @param b				Additional context information
* @param [a]			A list of options
* @param Int			The index of the item which should be pre-selected
* @param [TaskAction a]	A list of buttons or menus, through which the user can submit the value. If not specified
*						a default button is rendered.
*
* @return 				A task-form which allows the user to choose an item and displays the context information
*/
updateChoiceAbout			:: question b [a] Int -> Task a										| html question & iTask a & iTask b
updateChoiceAboutA			:: question ![TaskAction a] b [a] Int -> Task (!Action,!a)			| html question & iTask a & iTask b

/*
* Ask the user to select one or more items from a list of options
* @param question 		A question to display to the user
* @param [a]			A list of options
* @param [TaskAction a]	A list of buttons or menus, through which the user can submit the value. If not specified
*						a default button is rendered.
*
* @return 				A task-form which allows the user to choose items
*/
enterMultipleChoice			:: question [a] -> Task [a]											| html question & iTask a
enterMultipleChoiceA		:: question ![TaskAction [a]] [a] -> Task (!Action,![a])			| html question & iTask a

/*
* Ask the user to select one or more items from a list of options with already one option pre-selected
* @param question 		A question to display to the user
* @param [a]			A list of options
* @param Int			The index of the item which should be pre-selected
* @param [TaskAction a]	A list of buttons or menus, through which the user can submit the value. If not specified
*						a default button is rendered.
*
* @return 				A task-form which allows the user to choose items
*/
updateMultipleChoice		:: question [a] [Int] -> Task [a]									| html question & iTask a
updateMultipleChoiceA		:: question ![TaskAction [a]] [a] [Int] -> Task (!Action,![a])		| html question & iTask a

/*
* Ask the user to select one or more items from a list of options, given additional context information
* @param question 		A question to display to the user
* @param b				Additional context information
* @param [a]			A list of options
* @param [TaskAction a]	A list of buttons or menus, through which the user can submit the value. If not specified
*						a default button is rendered.
*
* @return 				A task-form which allows the user to choose items and displays the context information
*/
enterMultipleChoiceAbout	:: question b [a] -> Task [a]										| html question & iTask a & iTask b
enterMultipleChoiceAboutA	:: question ![TaskAction [a]] b [a] -> Task (!Action,![a])			| html question & iTask a & iTask b

/*
* Ask the user to select one or more items from a list of options with already one option pre-selected, given additional context information
* @param question 		A question to display to the user
* @param b				Additional context information
* @param [a]			A list of options
* @param Int			The index of the item which should be pre-selected
* @param [TaskAction a]	A list of buttons or menus, through which the user can submit the value. If not specified
*						a default button is rendered.
*
* @return 				A task-form which allows the user to choose items and displays the context information
*/
updateMultipleChoiceAbout	:: question b [a] [Int] -> Task [a]									| html question & iTask a & iTask b
updateMultipleChoiceAboutA	:: question ![TaskAction [a]] b [a] [Int] -> Task (!Action,![a])	| html question & iTask a & iTask b

//*** Output tasks ***//

/*
* Show a basic message to the user. The user can end the task after reading the message. 
* @param message			The message to display to the user
* @param [TaskAction Void] 	A list of buttons or menus through which the user can submit the value. If not specified
*							a default button is rendered
*
* @return					Void or an Action if [TaskAction Void] is specified
*/
showMessage					:: message -> Task Void												| html message
showMessageA				:: message ![TaskAction Void] -> Task Action						| html message

/*
* Show a basic message and additional context information to the user. The user can end the task after reading the message. 
* @param message			The message to display to the user
* @param a					Additional context information
* @param [TaskAction Void] 	A list of buttons or menus through which the user can submit the value. If not specified
*							a default button is rendered
*
* @return					Void or an Action if [TaskAction Void] is specified
*/
showMessageAbout			:: message a -> Task Void											| html message & iTask a
showMessageAboutA			:: message ![TaskAction Void] a -> Task Action						| html message & iTask a

/*
* Show a basic message to the user. The user cannot end the task after reading the message. 
* @param message			The message to display to the user
*
* @return					Void or an Action if [TaskAction Void] is specified
*/
showStickyMessage			:: message -> Task Void												| html message

/*
* Show a basic message and some context information to the user. The user cannot end the task after reading the message. 
* @param message			The message to display to the user
* @param a					Additional context information
*
* @return					Void or an Action if [TaskAction Void] is specified
*/
showStickyMessageAbout		:: message a -> Task Void											| html message & iTask a

/* 
* Shows a instruction to the user. The user can dismiss the instruction.
* @param String				A title message
* @param instruction		The instruction
*
* @return					Void
*/
showInstruction 			:: !String !instruction		-> Task Void | html instruction

/* 
* Shows a instruction and additional context information to the user. The user can dismiss the instruction.
* @param String				A title message
* @param instruction		The instruction
* @param b					Additional context information
*
* @return					Void
*/
showInstructionAbout 		:: !String !instruction b 	-> Task Void | html instruction & iTask b

//Notify a user through external media. For example via e-mail or sms. (Not implemented)
//notifyUser					:: message UserName -> Task Void									| html message
//notifyGroup					:: message Role -> Task Void										| html message

//*** Shared variable tasks ***//
generic gMakeSharedCopy a :: !a !String -> a
derive gMakeSharedCopy OBJECT, CONS, PAIR, FIELD, EITHER, UNIT
derive gMakeSharedCopy Int, Real, Char, Bool, String
derive gMakeSharedCopy Document, [], Maybe, Either, (,), (,,), (,,,), Void, Static, Hidden

generic gMakeLocalCopy a :: !a !*TSt -> (a,!*TSt)
derive gMakeLocalCopy OBJECT, CONS, PAIR, FIELD, EITHER, UNIT
derive gMakeLocalCopy Int, Real, Char, Bool, String
derive gMakeLocalCopy Document, [], Maybe, Either, (,), (,,), (,,,), Void, Static, Hidden

class SharedVariable a | gMerge{|*|}, gMakeSharedCopy{|*|}, gMakeLocalCopy{|*|} a

:: Editor s a	= {editorFrom :: s -> a, editorTo :: a s -> s}
:: Listener s a	= {listenerFrom :: s -> a}
:: View s

listener	:: !(Listener s a)	-> View s | iTask a & iTask s & SharedVariable s
editor		:: !(Editor s a)	-> View s | iTask a & iTask s & SharedVariable s

idEditor	:: View s	| iTask s & SharedVariable s
idListener	:: View s	| iTask s & SharedVariable s

updateShared				:: question ![TaskAction s] !(DBid s) ![View s] -> Task (!Action, !s)	| html question & iTask s & SharedVariable s
updateSharedLocal			:: question ![TaskAction s] !s ![View s] -> Task (!Action, !s)			| html question & iTask s & SharedVariable s

class html a  
where
	html :: a -> [HtmlTag]
	
instance html String
instance html [HtmlTag]

:: TaskAction a 		= 	ButtonAction (ActionWithCond a) 
						| 	MenuAction (ActionWithCond a) 
						| 	ButtonAndMenuAction (ActionWithCond a) 
						| 	MenuParamAction (String, ActionCondition a)
:: ActionWithCond a 	:== (Action, ActionCondition a)
:: ActionCondition a 	= 	Always 
						| 	IfValid 
						| 	Predicate ((EditorValue a) -> Bool)
:: EditorValue a 		= 	Invalid 
						| 	Valid a

//*** Utility Functions ***//
//Generate a set of action buttons by joining the buttons that are always shown and those only active when valid
//makeButtons :: !String ![(Action, Bool)] -> [(!Action,!String,!String,!String,!Bool)]
//taskPanel :: String [HtmlTag] (Maybe [HtmlTag]) (Maybe [TUIDef]) [(Action,String,String,String,Bool)] -> TUIDef