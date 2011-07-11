definition module SystemTypes
/**
* This module provides types for all the globally shared concepts
* of the iTasks framework.
*/

import GenEq, Maybe, JSON, Store, Void, Either, FilePath, HTML
from Map 			import :: Map
from Map 			import qualified get
from HTML 			import class html
from Time			import :: Timestamp
from Config			import :: Config
from IWorld			import :: IWorld
from TUIDefinition	import :: TUISize, :: TUIMargins, :: TUIMinSize
from Task			import :: Task
from iTaskClass		import class iTask, generic gVerify, :: VerSt, generic gDefaultMask, :: UpdateMask, generic gUpdate, :: USt, :: UpdateMode, generic gVisualize, :: VSt, :: Visualization

derive JSONEncode	Currency, FormButton, ButtonState, User, UserDetails, Document, Hidden, Display, Editable, VisualizationHint
derive JSONEncode	Note, Password, Date, Time, DateTime, RadioChoice, ComboChoice, TreeChoice, CheckMultiChoice, Map, Void, Either, Timestamp, Tree, TreeNode, Table
derive JSONEncode	EmailAddress, Session, Action, HtmlDisplay, WorkflowDescription, ControlSize, FillControlSize, FillWControlSize, FillHControlSize
derive JSONDecode	Currency, FormButton, ButtonState, User, UserDetails, Document, Hidden, Display, Editable, VisualizationHint
derive JSONDecode	Note, Password, Date, Time, DateTime, RadioChoice, ComboChoice, TreeChoice, CheckMultiChoice, Map, Void, Either, Timestamp, Tree, TreeNode, Table
derive JSONDecode	EmailAddress, Session, Action, HtmlDisplay, WorkflowDescription, ControlSize, FillControlSize, FillWControlSize, FillHControlSize
derive gEq			Currency, FormButton, User, UserDetails, Document, Hidden, Display, Editable, VisualizationHint
derive gEq			Note, Password, Date, Time, DateTime, RadioChoice, ComboChoice, TreeChoice, CheckMultiChoice, Map, Void, Either, Timestamp, Tree, TreeNode, Table
derive gEq			EmailAddress, Session, Action, Maybe, JSONNode, (->), Dynamic, HtmlDisplay, WorkflowDescription, ControlSize, FillControlSize, FillWControlSize, FillHControlSize
derive JSONEncode	TaskPriority, TaskProperties, ProcessProperties, ManagerProperties, SystemProperties, TaskDescription, TaskStatus, RunningTaskStatus, WorkflowTaskContainer
derive JSONDecode	TaskPriority, TaskProperties, ProcessProperties, ManagerProperties, SystemProperties, TaskDescription, TaskStatus, RunningTaskStatus, WorkflowTaskContainer
derive gEq			TaskPriority, TaskProperties, ProcessProperties, ManagerProperties, SystemProperties, TaskDescription, TaskStatus, RunningTaskStatus, WorkflowTaskContainer

instance toString User
instance toString Note
instance toString Password
instance toString Date
instance toString Time
instance toString DateTime
instance toString Currency
instance toString TaskPriority
instance toString Document
instance toString FormButton
instance toString (TaskList s)
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

class toEmail r where toEmail :: !r -> EmailAddress
instance toEmail EmailAddress
instance toEmail String
instance toEmail User

:: URL			= URL !String
:: PhoneNr		= PhoneNr String
:: Password		= Password !String
// Plain text notes
:: Note			= Note !String

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
	{ documentId	:: !DocumentId				//*A unique identifier of the document
	, name			:: !String					//*The filename of a document
	, mime			:: !String					//*The mime type of the document
	, size			:: !Int						//*The filesize in bytes
	}
:: DocumentId :== String

//* Form buttons
:: FormButton 		= 
	{ label			:: !String
	, icon			:: !String
	, state			:: !ButtonState
	}
:: ButtonState		= NotPressed | Pressed

//* Represents the choice of one element from a list represented as radio buttons
:: RadioChoice v o = RadioChoice ![(!v,!o)] !(Maybe Int)
mkRadioChoice :: !(container(!v,!o)) !(Maybe o) -> RadioChoice v o | OptionContainer container & gEq{|*|} o
instance Choice RadioChoice

//* Represents the choice of one element from a list represented as combo box
:: ComboChoice v o = ComboChoice ![(!v,!o)] !(Maybe Int)
mkComboChoice :: !(container (!v,!o)) !(Maybe o) -> ComboChoice v o | OptionContainer container & gEq{|*|} o
instance Choice ComboChoice

//* Represents a tree from with the user can choose one element
:: TreeChoice v o = TreeChoice !(Tree (!v,!o)) !(Maybe Int)
:: Tree a = Tree !.[.TreeNode a]
:: TreeNode a = Leaf !a | Node !TreeLabel !.[TreeNode a]
:: TreeLabel :== String
mkTreeChoice :: !(container (!v,!o)) !(Maybe o) -> TreeChoice v o | OptionContainer container & gEq{|*|} o
instance Choice TreeChoice
instance Functor Tree

/**
* Interface for types representing choices of one element out of a set of options.
* There are different kinds of containers for such options (e.g. lists, trees, ...).
* Each option consists of an actual value (o) & a view value shown to the user (v).
*/
class Choice choiceType
where
	//* Generates a choice with given options and possibly initial selection
	mkChoice				:: !(container (!v,!o)) !(Maybe o)			-> choiceType v o | OptionContainer container & gEq{|*|} o
	//* Selects the given option, if not present in list of options selection is cleared
	selectOption			:: !o !(choiceType v o)						-> choiceType v o | gEq{|*|} o
	//* Gets the current selection assuming it is present (a valid choice always has a selection)
	getSelection			:: !(choiceType v o)						-> o
	//* Gets the current selection if present
	getMbSelection			:: !(choiceType v o)						-> Maybe o
	//* Gets the current selection's view if present
	getMbSelectionView		:: !(choiceType v o)						-> Maybe v
	//* Sets the choice's options, tries to keep the selection as intact as possible
	setOptions				:: !(container (!v,!o)) !(choiceType v o)	-> choiceType v o | OptionContainer container & gEq{|*|} o

:: ChoiceType	= AutoChoiceView
				| ChooseFromRadioButtons
				| ChooseFromComboBox
				//| ChooseFromTable
				| ChooseFromTree

//* Represents the choice of a number of items from a list
:: CheckMultiChoice v o = CheckMultiChoice ![(!v,!o)] ![Int]
mkCheckMultiChoice :: !(container (!v,!o)) ![o] -> CheckMultiChoice v o | OptionContainer container & gEq{|*|} o
instance MultiChoice CheckMultiChoice

:: MultiChoiceType	= AutoMultiChoiceView
					| ChooseFromCheckBoxes

/**
* Interface for types representing choices a number of elements out of a set of options.
* There are different kinds of containers for such options (e.g. lists, trees, ...).
* Each option consists of an actual value (o) & a view value shown to the user (v).
*/
class MultiChoice choiceType
where
	//* Generates a multi choice with given options and initial selection
	mkMultiChoice			:: !(container (!v,!o)) ![o]				-> choiceType v o | OptionContainer container & gEq{|*|} o
	//* Selects the given options, selections not present in list of options are ignored
	selectOptions			:: ![o] !(choiceType v o)					-> choiceType v o | gEq{|*|} o
	//* Gets the current selections
	getSelections			:: !(choiceType v o)						-> [o]
	//* Gets the current selection's views
	getSelectionViews		:: !(choiceType v o)						-> [v]
	//* Sets the choice's options, tries to keep the selection as intact as possible
	setMultiOptions			:: !(container (!v,!o)) !(choiceType v o)	-> choiceType v o | OptionContainer container & gEq{|*|} o

class OptionContainer container | Functor container
where
	toOptionList				:: !(container o) -> [o]
	toOptionTree				:: !(container o) -> Tree o
	suggestedChoiceType			:: !(container o) -> ChoiceType
	suggestedMultiChoiceType	:: !(container o) -> MultiChoiceType
	
instance OptionContainer []
instance OptionContainer Tree

//* Represents a table consisting of headers, the displayed data cells & possibly a selection
:: Table = Table ![String] ![[HtmlTag]] !(Maybe Int)

//* Field behaviour extensions
:: VisualizationHint a 	= VHEditable a
					   	| VHDisplay a
					   	| VHHidden a

//* Variable is always rendered within a form as editor field
:: Editable a 			= Editable a		

//* Variable is always rendered within a form as a static element
:: Display a 			= Display a			

//* Variable is never rendered
:: Hidden a 			= Hidden a			

fromVisualizationHint :: !(VisualizationHint .a) -> .a
toVisualizationHint :: !.a -> VisualizationHint .a

fromEditable :: !(Editable .a) -> .a
toEditable :: !.a -> Editable .a

fromDisplay :: !(Display .a) -> .a
toDisplay :: !.a -> Display .a

fromHidden :: !(Hidden .a) -> .a
toHidden :: !.a -> Hidden .a

:: HtmlDisplay = HtmlDisplay !String
toHtmlDisplay	:: !h -> HtmlDisplay | html h
fromHtmlDisplay	:: !HtmlDisplay -> String
instance toString HtmlDisplay

// Wrapper types for changing the control's sizes
:: ControlSize a		= ControlSize		!TUISize !TUISize !(Maybe TUIMargins) !a	//* all controls generated for a have specified sizes
:: FillControlSize a	= FillControlSize	!a											//* all controls generated for a fill the parent
:: FillWControlSize a	= FillWControlSize	!a											//* all controls generated for a fill the parent's width
:: FillHControlSize a	= FillHControlSize	!a											//* all controls generated for a fill the parent's height

toControlSize :: !TUISize !TUISize !(Maybe TUIMargins) !.a -> ControlSize .a
fromControlSize :: !(ControlSize .a) -> .a
toFillControlSize :: !.a -> FillControlSize .a
fromFillControlSize :: !(FillControlSize .a) -> .a
toFillWControlSize :: !.a -> FillWControlSize .a
fromFillWControlSize :: !(FillWControlSize .a) -> .a
toFillHControlSize :: !.a -> FillHControlSize .a
fromFillHControlSize :: !(FillHControlSize .a) -> .a

//* Represents lists of tasks (SHOULD BE ABSTRACT)
:: TaskList s
	= GlobalTaskList			//*The global list of task instances
	| ParallelTaskList !TaskId	//*The list of task instances of a parallel task


//* Properties of task processes	
:: ProcessProperties =
	{ taskProperties	:: !TaskProperties
	, managerProperties	:: !ManagerProperties
	, systemProperties	:: !SystemProperties
	}
	
:: TaskProperties =
	{ taskDescription	:: !TaskDescription				//* Description of the task
	, tags				:: ![String]					//* A list of tags
	, interactionType	:: !Maybe InteractionTaskType	//* type of interaction (for interaction tasks)
	, localInteraction	:: !Bool						//* indicates that the task's interaction is restricted to local data while it is running
	, controlTask		:: !Bool						//* indicates that the task is used to control another one
	}

:: TaskDescription	=
	{ title				:: !String					//* The task's title
	, description		:: !String					//* A longer description of the task (HTML string)
	}
	
:: SystemProperties =
	{ taskId			:: !TaskId					//* Task identification
	, status			:: !TaskStatus				//* Is a maintask active,suspended,finished or excepted
	, issuedAt			:: !Timestamp				//* When was the task created
	, firstEvent		:: !Maybe Timestamp			//* When was the first work done on this task
	, latestEvent		:: !Maybe Timestamp			//* When was the latest event on this task	
	}

isActive :: !ProcessProperties -> Bool
	
//* String serialization of TaskNr values	
:: TaskId :== String		

:: TaskStatus	= Running		//* A process which is currently running (active or suspended)
				| Finished		//* A process terminated normally
				| Excepted		//* A process terminated with an exception
				| Deleted		//* A process is deleted (never set, but returned when process can not be found)

:: ManagerProperties =
	{ worker			:: !User					//* Who has to do the task? 
	, priority			:: !TaskPriority			//* What is the current priority of this task?
	, deadline			:: !Maybe DateTime			//* When is the task due?
	, status			:: !RunningTaskStatus
	}
	
:: RunningTaskStatus	= Active		//* A process is active and can be further evaluated
						| Suspended		//* A process is (temporarily) suspended and will not be evaluated until it is activated 
	

//* tasks can have three levels of priority
:: TaskPriority		= HighPriority					
					| NormalPriority
					| LowPriority
					
formatPriority	:: !TaskPriority	-> HtmlTag
						
:: WindowTitle :== String

instance toString TaskStatus
instance == TaskStatus
instance == RunningTaskStatus

class descr d
where
	toDescr :: !d -> TaskDescription

instance descr String
instance descr (!String, !descr) | html descr
instance descr TaskDescription

initTaskProperties :: TaskProperties
initManagerProperties :: ManagerProperties

setRunning	:: !ProcessProperties -> ProcessProperties
setFinished	:: !ProcessProperties -> ProcessProperties
setExcepted	:: !ProcessProperties -> ProcessProperties

// Users	
:: User
	= AnyUser						//* Any not further specified person
	| RootUser						//* The system super user
	| RegisteredUser !UserDetails	//* A registered person of whom we know details
	| NamedUser !String				//* A person identified by a username
	| SessionUser !SessionId		//* A person that is only identified by a session
	
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

:: ProcessId		:== Int

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

/**
* The information state of a running task.
*/
:: InformationState s =	{ modelValue	:: !s		// the value of the data model the editor is working on
						, localValid	:: !Bool	// a flag indicating if the editor's local view is valid
						}
:: TermFunc a b :== (InformationState a) -> InteractionTerminators b

:: InteractionTerminators a	= UserActions		![(!Action,!Maybe a)]	// A list of actions the user can possibly trigger, actions with a Just-value stop the task with given result, others (Nothing) are disabled
							| StopInteraction	!a						// The task stops and produces result a
							
/*
* To allow users to specify a followup action to their current task
* most interaction tasks allow you to specify actions that can be chosen.
* These actions are either available as a button on the bottom of the task interface
* or as an item in the task menu, or both.
* Additionally conditions can be specified when the action is allowed to be performed.
*/
:: Action	= Action !ActionName
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

instance == Action
actionName :: !Action -> ActionName

actionIcon 	:: !Action -> String
			
:: Hotkey =	{ key	:: !Key
			, ctrl	:: !Bool
			, alt	:: !Bool
			, shift	:: !Bool
			}
:: Key :== Char

:: InteractionTaskType	= InputTask | UpdateTask | OutputTask !OutputTaskType
:: OutputTaskType		= ActiveOutput | PassiveOutput

:: WorkflowDescription =	{ workflowId		:: !WorkflowId
							, path				:: !String
							, roles				:: ![String]
							, description		:: !String
							, managerProperties	:: !ManagerProperties
							}
							
:: WorkflowId :== Int

:: WorkflowTaskContainer	= E.a:		WorkflowTask		(Task a)		& iTask a
							| E.a b:	ParamWorkflowTask	(a -> (Task b))	& iTask a & iTask b
				
// A workflow specification
:: Workflow		=	{ path				:: String					//* a unique name of this workflow
					, roles				:: [String]					//* the roles that are allowed to initate this workflow
					, task				:: WorkflowTaskContainer	//* the thread of the main task of the workflow
					, description		:: String					//* a description of the workflow
					, managerProperties	:: ManagerProperties		//* the initial manager properties of the main task
					}
					
isAllowedWorkflow :: !User !(Maybe UserDetails) !WorkflowDescription -> Bool