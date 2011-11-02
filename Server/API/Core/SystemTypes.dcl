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
from IWorld			import :: IWorld
from TUIDefinition	import :: TUISize, :: TUIMargins, :: TUIMinSize
from Task			import :: Task
from iTaskClass		import class iTask, generic gVerify, :: VerSt, generic gDefaultMask, :: UpdateMask, generic gUpdate, :: USt, :: UpdateMode, generic gVisualizeEditor, generic gVisualizeText, generic gHeaders, generic gGridRows, :: VSt, :: StaticVisualizationMode, :: TUIDef

derive JSONEncode		Currency, FormButton, ButtonState, User, UserDetails, Document, Hidden, Display, Editable, VisualizationHint, HtmlTag
derive JSONEncode		Note, Username, Password, Date, Time, DateTime, RadioChoice, ComboChoice, TreeChoice, GridChoice, CheckMultiChoice, Map, Void, Either, Timestamp, Tree, TreeNode, Table
derive JSONEncode		EmailAddress,ProcessId, Action, HtmlInclude, ControlSize, FillControlSize, FillWControlSize, FillHControlSize
derive JSONDecode		Currency, FormButton, ButtonState, User, UserDetails, Document, Hidden, Display, Editable, VisualizationHint, HtmlTag
derive JSONDecode		Note, Username, Password, Date, Time, DateTime, RadioChoice, ComboChoice, TreeChoice, GridChoice, CheckMultiChoice, Map, Void, Either, Timestamp, Tree, TreeNode, Table
derive JSONDecode		EmailAddress, ProcessId, Action, HtmlInclude, ControlSize, FillControlSize, FillWControlSize, FillHControlSize
derive gEq				Currency, FormButton, User, UserDetails, Document, Hidden, Display, Editable, VisualizationHint, HtmlTag
derive gEq				Note, Username, Password, Date, Time, DateTime, RadioChoice, ComboChoice, TreeChoice, GridChoice, CheckMultiChoice, Map, Void, Either, Timestamp, Tree, TreeNode, Table
derive gEq				EmailAddress, ProcessId, Action, Maybe, JSONNode, (->), Dynamic, HtmlInclude, ControlSize, FillControlSize, FillWControlSize, FillHControlSize
derive JSONEncode		TaskInstanceMeta, TaskMeta, ManagementMeta, TaskPriority, ProgressMeta, TaskStatus
derive JSONDecode		TaskInstanceMeta ,TaskMeta, ManagementMeta, TaskPriority, ProgressMeta, TaskStatus
derive gEq				TaskInstanceMeta ,TaskMeta, ManagementMeta, TaskPriority, ProgressMeta, TaskStatus
derive gVisualizeText	ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus
derive gVisualizeEditor	ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus
derive gHeaders			ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus
derive gGridRows		ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus
derive gUpdate			ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus
derive gDefaultMask		ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus
derive gVerify			ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus

derive class iTask Credentials, Config


instance toString Note
instance toString User
instance toString Username
instance toString Password
instance toString Date
instance toString Time
instance toString DateTime
instance toString Currency
instance toString TaskPriority
instance toString Document
instance toString FormButton
instance toString ProcessId
instance toString (TaskList s)
instance fromString Date
instance fromString Time
instance fromString DateTime
instance fromString ProcessId

instance == Note
instance == Document
instance == User
instance == Username
instance == Password
instance == ProcessId

instance < Time
instance < Date
instance < DateTime
instance < User
instance < Username
instance < Password
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
:: TreeNode a = Leaf !a | Node !a !.[TreeNode a]
mkTreeChoice :: !(container (!v,!o)) !(Maybe o) -> TreeChoice v o | OptionContainer container & gEq{|*|} o
instance Choice TreeChoice
instance Functor Tree

//* Represents the choice of one element from a list represented as grid
//* (typically v is a record which's labels are used as headers)
:: GridChoice v o = GridChoice ![(!v,!o)] !(Maybe Int)
mkGridChoice :: !(container (!v,!o)) !(Maybe o) -> GridChoice v o | OptionContainer container & gEq{|*|} o
instance Choice GridChoice

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
	//* Selects the option given by the index
	selectIndex				:: !Int !(choiceType v o)					-> choiceType v o
	//* Generates empty choice
	mkEmptyChoice			:: 											   choiceType v o
	
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

:: HtmlInclude	= HtmlInclude String

// Wrapper types for changing the control's sizes
:: ControlSize a		= ControlSize		!(Maybe TUISize) !(Maybe TUISize) !(Maybe TUIMargins) !a	//* all controls generated for a have specified sizes
:: FillControlSize a	= FillControlSize	!a											//* all controls generated for a fill the parent
:: FillWControlSize a	= FillWControlSize	!a											//* all controls generated for a fill the parent's width
:: FillHControlSize a	= FillHControlSize	!a											//* all controls generated for a fill the parent's height

toControlSize :: !(Maybe TUISize) !(Maybe TUISize) !(Maybe TUIMargins) !.a -> ControlSize .a
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

	
//* String serialization of TaskNr values	
:: TaskId :== String

//* Properties of tasks
:: TaskMeta =
	{ title				:: !String						//* A descriptive title
	, instruction		:: !Maybe String				//* Instruction of the task
	, icon				:: !Maybe String				//* An icon reference for the task
	, tags				:: ![String]					//* A list of tags
	, hide				:: !Bool						//* Hide the interface of this task (may be ignored by parallel layouters)
	, window			:: !Bool						//* Show the interface of this task in a window (if supported by the parallel layouter)
	, interactionType	:: !Maybe InteractionTaskType	//* type of interaction (for interaction tasks)
	, localInteraction	:: !Bool						//* indicates that the task's interaction is restricted to local data while it is running
	}
	
:: ManagementMeta =
	{ worker			:: !Maybe User				//* Who has to do the task? 
	, role				:: !Maybe Role				//* What role does a worker need to do the task
	, startAt			:: !Maybe DateTime			//* When is the task supposed to start
	, completeBefore	:: !Maybe DateTime			//* When does the task need to be completed
	, notifyAt			:: !Maybe DateTime			//* When would you like to be notified about the task
	, priority			:: !TaskPriority			//* What is the current priority of this task?
	}
	
:: ProgressMeta =
	{ issuedAt			:: !DateTime				//* When was the task created
	, issuedBy			:: !User					//* By whom was the task created
	, status			:: !TaskStatus				//* Is a maintask active,suspended,finished or excepted
	, firstEvent		:: !Maybe DateTime			//* When was the first work done on this task
	, latestEvent		:: !Maybe DateTime			//* When was the latest event on this task	
	}
		
:: TaskStatus
	= Running		//* A process which is currently running (active or suspended)
	| Finished		//* A process terminated normally
	| Excepted		//* A process terminated with an exception
	| Deleted		//* A process is deleted (never set, but returned when process can not be found)

:: TaskInstanceMeta =
	{ processId			:: !ProcessId
	, taskMeta			:: !TaskMeta
	, progressMeta		:: !ProgressMeta
	, managementMeta	:: !ManagementMeta
	, subInstances		:: ![TaskInstanceMeta]
	} 

:: ProcessId
	= SessionProcess !String
	| WorkflowProcess !Int
	| EmbeddedProcess !Int !TaskId


//* tasks can have three levels of priority
:: TaskPriority		= HighPriority					
					| NormalPriority
					| LowPriority
					
formatPriority	:: !TaskPriority	-> HtmlTag
						
instance toString TaskStatus
instance == TaskStatus

class descr d
where
	initTaskMeta :: !d -> TaskMeta

instance descr Void
instance descr String
instance descr (!String, !descr) | html descr
instance descr TaskMeta


noMeta :: ManagementMeta

// Users	
:: User
	= AnyUser						//* Any not further specified person
	| RootUser						//* The system super user
	| RegisteredUser !UserDetails	//* A registered person of whom we know details
	| NamedUser !String				//* A person identified by a username
	| SessionUser !String			//* A person that is only identified by a session
	
:: UserDetails			=
	{ username		:: !Username
	, password		:: !Password
	, displayName	:: !String
	, emailAddress	:: !EmailAddress
	, roles			:: !Maybe [Role]
	}

// Authentication
:: Credentials =
	{ username	:: !Username
	, password	:: !Password
	}
	
:: Password		= Password !String
:: Username		= Username !String

:: Role			:== String

//Configuration
:: Config =
	{ clientPath		:: !String			// Where is the client located.
	, staticPath		:: !String			// Additional location where statically served content may be placed
	, rootPassword		:: !String			// Password for the 'root' superuser (default 'root').
	, rootEmail			:: !String			// E-mail address for the 'root' superuser (default root@localhost).
	, sessionTime		:: !Int				// Time (in seconds) before inactive sessions are garbage collected. Default is 3600 (one hour).
	, serverPort		:: !Int				// The TCP port the server runs on. Default is 80.
	, serverPath		:: !String			// The path at which the services are served (default /services)
	, debug				:: !Bool			// Run the server in debug mode (default False).
	, smtpServer		:: !String			// The smtp server to use for sending e-mails
	, generalWorkflows	:: !Bool			// Enable the "general" workflows for managing ad-hoc work
	, runAsyncPath		:: !String			// Path to RunAsync tool for running asynchronous OS tasks and timers.
	, curlPath			:: !String			// Path to Curl needed for RPC tasks.
	}


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

