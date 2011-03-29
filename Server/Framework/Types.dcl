definition module Types
/**
* This module provides types for all the globally shared concepts
* of the iTasks framework.
*/

import GenEq, Maybe, JSON, Store, Map, Void, Either, FilePath
from Shared		import :: Shared
from HTML 		import class html
from Time		import :: Timestamp
from Config		import :: Config

derive JSONEncode	Currency, FormButton, User, UserDetails, Document, Hidden, Display, Editable, VisualizationHint
derive JSONEncode	Note, Password, Date, Time, DateTime, Choice, MultipleChoice, Map, Void, Either, Timestamp, Tree, TreeNode
derive JSONEncode	EmailAddress, Session, Action, Table, Shared, HtmlDisplay
derive JSONDecode	Currency, FormButton, User, UserDetails, Document, Hidden, Display, Editable, VisualizationHint
derive JSONDecode	Note, Password, Date, Time, DateTime, Choice, MultipleChoice, Map, Void, Either, Timestamp, Tree, TreeNode
derive JSONDecode	EmailAddress, Session, Action, Table, Shared, HtmlDisplay
derive gEq			Currency, FormButton, User, UserDetails, Document, Hidden, Display, Editable, VisualizationHint
derive gEq			Note, Password, Date, Time, DateTime, Choice, MultipleChoice, Map, Void, Either, Timestamp, Tree, TreeNode
derive gEq			EmailAddress, Session, Action, Maybe, JSONNode, (->), Dynamic, Table, Shared, HtmlDisplay
derive JSONEncode	TaskPriority, TaskProperties, ProcessProperties, ManagerProperties, SystemProperties, TaskProgress, FormWidth, TaskDescription, TaskStatus, RunningTaskStatus
derive JSONDecode	TaskPriority, TaskProperties, ProcessProperties, ManagerProperties, SystemProperties, TaskProgress, FormWidth, TaskDescription, TaskStatus, RunningTaskStatus
derive gEq			TaskPriority, TaskProperties, ProcessProperties, ManagerProperties, SystemProperties, TaskProgress, FormWidth, TaskDescription, TaskStatus, RunningTaskStatus

instance toString User
instance toString Note
instance toString Password
instance toString Date
instance toString Time
instance toString DateTime
instance toString Currency
instance toString TaskPriority
instance fromString Date
instance fromString Time
instance fromString DateTime

instance == User
instance == Document
instance == Note
instance == Password

instance < Time
instance < Date
instance < DateTime
instance < User
instance < Currency

instance + Time		//Basic addition, righthand argument is treated as interval (seconds are added first)
instance + Date		//Basic addition, righthand argument is treated as interval (days are added first)
instance + DateTime	//Basic addition, righthand argument is treated as interval
instance + Currency 

instance - Time		//Naive fieldwise subtraction
instance - Date		//Naive fieldwise subtraction
instance - DateTime	//Naive fieldwise subtraction
instance - Currency

instance toInt Currency
instance zero Currency
instance html Note

// Strings with special meanings
:: EmailAddress	= EmailAddress !String
:: URL			= URL !String
:: PhoneNr		= PhoneNr String
:: Password		= Password !String
// Plain text notes
:: Note = Note !String

// Money
:: Currency		// Type of currency and amount in cents. ISO4217 currency codes are used
	= EUR !Int
	| GBP !Int
	| USD !Int
	| JPY !Int

:: Date	=
	{ day	:: !Int
	, mon	:: !Int
	, year	:: !Int
	}
:: Time =
	{ hour	:: !Int
	, min	:: !Int
	, sec	:: !Int
	}
:: DateTime = DateTime !Date !Time

// Documents
:: Document =
	{ documentId	:: !DocumentId				//A unique identifier of the document
	, name			:: !String					//The filename of a document
	, mime			:: !String					//The mime type of the document
	, size			:: !Int						//The filesize in bytes
	}
:: DocumentId :== String

// Form buttons
:: FormButton 		= 
	{ label			:: !String
	, icon			:: !String
	, state			:: !ButtonState
	}
:: ButtonState		= NotPressed | Pressed

// Represents the choice of one element from a list
:: Choice			a = Choice			![a] !Int
// Represents the choice of a number of items from a list
:: MultipleChoice	a = MultipleChoice	![a] ![Int]

// Generates a choice with given options
choice 				:: ![a]								-> Choice a
// Generates a choice with given options and preselected item
choiceSel			:: ![a] !Int						-> Choice a
// Gets the currently chosen item
getChoice			:: !(Choice a)						-> a
// Gets the index of the currently chosen item
getChoiceIndex		:: !(Choice a)						-> Int
// Sets the index of the currently chosen item
setChoiceIndex		:: !Int !(Choice a)					-> Choice a
// Transforms the choice's options
mapOptions			:: !(a -> b) !(Choice a)			-> Choice b
// Sets the choice's options, tries to keep the selection as intact as possible
setOptions			:: ![a] !(Choice a)					-> Choice a | gEq{|*|} a

// Generates a multiple choice with given options
multipleChoice		:: ![a] 							-> MultipleChoice a
// Generates a multiple choice with given options and preselected items
multipleChoiceSel	:: ![a] ![Int]						-> MultipleChoice a
// Get the currently chosen items
getChoices			:: !(MultipleChoice a)				-> [a]
// Gets the indexes of the currently chosen items
getChoiceIndexes	:: !(MultipleChoice a)				-> [Int]
// Sets the indexes of the currently chosen items
setChoiceIndexes	:: ![Int] !(MultipleChoice a)		-> MultipleChoice a
// Transforms the multiple choice's options
mapOptionsM			:: !(a -> b) !(MultipleChoice a)	-> MultipleChoice b
// Sets the multiple choice's options, tries to keep the selection as intact as possible
setOptionsM			:: ![a] !(MultipleChoice a)			-> MultipleChoice a | gEq{|*|} a

// Represents a tree from with the user can choose one leaf
:: Tree a = Tree ![TreeNode a] !Int
:: TreeNode a = Leaf !a | Node !TreeLabel ![TreeNode a]
:: TreeLabel :== String

// Generates a tree with initially no chosen item
mkTree		:: ![TreeNode a]	-> Tree a
// Generates a tree with initially chosen item
mkTreeSel	:: ![TreeNode a] !a	-> Tree a | gEq{|*|} a
// Gets the currently selected leaf of a VALID tree
getSelectedLeaf :: !(Tree a) -> a

:: Table a = Table ![a]

fromTable :: !(Table a) -> [a]

// Field behaviour extensions
:: VisualizationHint a 	= VHEditable a
					   	| VHDisplay a
					   	| VHHidden a
:: Editable a 			= Editable a		// Variable is always rendered within a form as editor field
:: Display a 			= Display a			// Variable is always rendered within a form as a static element
:: Hidden a 			= Hidden a			// Variable is never rendered

fromVisualizationHint :: !(VisualizationHint .a) -> .a
toVisualizationHint :: !.a -> (VisualizationHint .a)

fromEditable :: !(Editable .a) -> .a
toEditable :: !.a -> (Editable .a)

fromDisplay :: !(Display .a) -> .a
toDisplay :: !.a -> (Display .a)

fromHidden :: !(Hidden .a) -> .a
toHidden :: !.a -> (Hidden .a)

:: HtmlDisplay = HtmlDisplay !String
toHtmlDisplay	:: !h -> HtmlDisplay | html h
fromHtmlDisplay	:: !HtmlDisplay -> String
instance toString HtmlDisplay

// Properties of processes	
:: ProcessProperties =
	{ taskProperties	:: !TaskProperties
	, managerProperties	:: !ManagerProperties
	, systemProperties	:: !SystemProperties
	, progress			:: !TaskProgress
	}

:: SystemProperties =
	{ taskId			:: !TaskId					// Process table identification
	, parent			:: !Maybe TaskId			// The (direct) parent process
	, status			:: !TaskStatus				// Is a maintask active,suspended,finished or excepted
	, issuedAt			:: !Timestamp				// When was the task created
	, firstEvent		:: !Maybe Timestamp			// When was the first work done on this task
	, latestEvent		:: !Maybe Timestamp			// When was the latest event on this task	
	, deleteWhenDone	:: !Bool					// Delete the process after completion
	, menu				:: !ActionMenu				// The maintask's menu
	}
	
:: TaskId :== String		// String serialization of TaskNr values

:: TaskStatus	= Running		// A process which is currently running (active or suspended)
				| Finished		// A process terminated normally
				| Excepted		// A process terminated with an exception
				| Deleted		// A process is deleted (never set, but returned when process can not be found)

:: ManagerProperties =
	{ worker			:: !User					// Who has to do the task? 
	, priority			:: !TaskPriority			// What is the current priority of this task?
	, deadline			:: !Maybe DateTime			// When is the task due?
	, status			:: !RunningTaskStatus
	}
	
:: RunningTaskStatus	= Active		// A process is active and can be further evaluated
						| Suspended		// A process is (temporarily) suspended and will not be evaluated until it is activated 
	
:: TaskProperties =
	{ taskDescription	:: !TaskDescription			// Description of the task
	, tags				:: ![String]				// A list of tags
	, isControlTask		:: !Bool					// is the task a control task?
	}

:: TaskDescription	=
	{ title				:: !String					// The task's title
	, description		:: !String					// A longer description of the task (HTML string)
	}
	
:: TaskPriority		= HighPriority					// tasks can have three levels of priority
					| NormalPriority
					| LowPriority
					
:: FormWidth	= FWAuto							// Set form width to client default
				| FWFullWidth						// Set form width to maximum width
	
:: TaskProgress		= TPActive			//Worker is happily working on the task
					| TPStuck			//Worker is stuck and needs assistence
					| TPWaiting			//Worker is waiting, not actively working on the task
					| TPReject			//Worker does not want to continue working on the task
					
:: TaskContainerType	= CTDetached !ManagerProperties !ActionMenu	// task detached as separate process
						| CTWindow !WindowTitle !ActionMenu			// task shwon in a window (with own menu)
						| CTDialog !WindowTitle						// task shwon as dialogue (without own menu)
						| CTInBody									// task shown in the body of the parallel container
						| CTHidden									// task not shown to the user
						
:: ActionMenu :== [ActionName] -> MenuDefinition

noMenu		:: ActionMenu
staticMenu	:: !MenuDefinition -> ActionMenu

:: WindowTitle :== String

instance toString TaskStatus
instance == TaskStatus
instance == RunningTaskStatus

class descr d
where
	toDescr :: d -> TaskDescription

instance descr String
instance descr (String, descr) | html descr
instance descr TaskDescription

initTaskProperties :: TaskProperties
initManagerProperties :: ManagerProperties

// Users	
:: User
	= AnyUser						// Any not further specified person
	| RootUser						// The system super user
	| RegisteredUser !UserDetails	// A registered person of whom we know details
	| NamedUser !String				// A person identified by a username
	| SessionUser !SessionId		// A person that is only identified by a session
	
:: UserDetails			=
	{ userName		:: !UserId
	, password		:: !Password
	, displayName	:: !String
	, emailAddress	:: !EmailAddress
	, roles			:: !Maybe [Role]
	}

:: UserId			:== String
:: Role				:== String

// Session
:: SessionId		:== String
:: Session			=
	{ sessionId	::	!String
	, user		::	!User
	, timestamp	::	!Timestamp
	}

:: ProcessId		:== String

/*
* Gives the unique username of a user
*
* @param The user
* @return The user's username
*/
userName 			:: !User -> String
/*
* Gives the display name of a user
*
* @param The user
* @return The user's display name
*/
displayName			:: !User -> String
/* 
* Gives the roles of the passed user
*
* @param The user
* @return The roles currently assigned to this user
*/
getRoles			:: !User -> [Role]

/*
* To allow users to specify a followup action to their current task
* most interactive tasks allow you to specify actions that can be chosen.
* These actions are either available as a button on the bottom of the task interface
* or as an item in the task menu, or both.
* Additionally conditions can be specified when the action is allowed to be performed.
*/
:: Action	= Action !ActionName !ActionLabel
			| ActionOk
			| ActionCancel
			| ActionYes
			| ActionNo
			| ActionNext
			| ActionPrevious
			| ActionFinish
			| ActionContinue
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

:: ActionName	:== String	//Locally unique identifier for actions
:: ActionLabel	:== String	//Textual label for the action

instance == Action

class actionName a :: a -> String

instance actionName Action
instance actionName ActionName

actionIcon 	:: !Action -> String
actionLabel	:: !Action -> String

// Definition of menus

:: MenuDefinition :== [Menu]
:: Menu 		= Menu !MenuLabel ![MenuItem]
:: MenuItem 	= E.action:	MenuItem !action !(Maybe Hotkey) & menuAction action
				| 			SubMenu !MenuLabel ![MenuItem]
				| 			MenuSeparator
:: MenuLabel	:== String
				
:: Hotkey =	{ key	:: !Key
			, ctrl	:: !Bool
			, alt	:: !Bool
			, shift	:: !Bool
			}	
:: Key = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

class menuAction a :: a -> MenuAction

:: MenuAction :== (ActionName, ActionLabel)

instance menuAction Action
instance menuAction ActionName
instance menuAction (actionName, ActionLabel) | actionName actionName

:: InteractiveTaskType = Information | Message | Instruction | Monitor | Control

// iWorld
:: *IWorld		=	{ application	:: !String		// The name of the application	
					, store			:: !Store		// The generic data store
					, config		:: !Config		// The server configuration
					, world			:: !*World		// The outside world
					, timestamp		:: !Timestamp	// The timestamp of the current request
					, localDateTime	:: !DateTime	// The local date & time of the current request
					, tmpDirectory	:: !FilePath	// The path for temporary files, the garbage collector also works on files in this dir
					}
