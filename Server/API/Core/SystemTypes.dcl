definition module SystemTypes
/**
* This module provides types for all the globally shared concepts
* of the iTasks framework.
*/

import GenEq_NG, Maybe, JSON_NG, Store, Void, Either, FilePath, HTML, Error, File, OS
from Map 			import :: Map
from Map 			import qualified get
from HTML 			import class html
from Time			import :: Timestamp
from IWorld			import :: IWorld
from TUIDefinition	import :: TUISize, :: TUIMargins, :: TUIMinSize
from Task			import :: Task, :: TaskId, :: TaskAttribute
from iTaskClass		import class iTask, generic gVerify, :: VerSt, generic gDefaultMask, :: UpdateMask, generic gUpdate, :: USt, :: UpdateMode, generic gVisualizeEditor, generic gVisualizeText, generic gHeaders, generic gGridRows, :: VSt, :: VisualizationResult, :: StaticVisualizationMode(..), :: TUIDef, visualizeAsText
from Shared			import :: ReadWriteShared, :: ReadOnlyShared, :: RWShared

// Strings with special meanings
:: EmailAddress	= EmailAddress !String
class toEmail r where toEmail :: !r -> EmailAddress
instance toEmail EmailAddress
instance toEmail String

// Uniform resource locators
:: URL			= URL !String

// Plain text notes
:: Note			= Note !String

// Money (ISO4217 currency codes are used)
:: EUR 			= EUR !Int		//Euros (amount in cents)
:: USD 			= USD !Int		//Dollars (amount in cents)

// (Local) date and time
:: Date	=
	{ day	:: !Int // 1..31
	, mon	:: !Int // 1..12
	, year	:: !Int
	}
:: Time =
	{ hour	:: !Int
	, min	:: !Int
	, sec	:: !Int
	}
:: DateTime = DateTime !Date !Time

// Integer value in a range on a scale
:: BoundedInt =
	{ min	:: Int
	, cur	:: Int
	, max	:: Int
	}

// Documents
:: Document =
	{ documentId	:: !DocumentId				//*A unique identifier of the document
	, name			:: !String					//*The filename of a document
	, mime			:: !String					//*The mime type of the document
	, size			:: !Int						//*The filesize in bytes
	}
:: DocumentId	:== String


//* Task results
:: TaskValue a		= NoValue				
					| Value !a !Stability 
			
:: TaskTime			:== Int

:: Stability		= Unstable | Stable

//* Meta-data of tasks
:: TaskMeta		:==	[TaskAttribute]					//* Task meta data consists of untyped attributes

:: ManagementMeta =
	{ title				:: !Maybe String			//* Title to identify the task
	, worker			:: !UserConstraint			//* Who has to do the task? 
	, role				:: !Maybe Role				//* What role does a worker need to do the task
	, startAt			:: !Maybe DateTime			//* When is the task supposed to start
	, completeBefore	:: !Maybe DateTime			//* When does the task need to be completed
	, notifyAt			:: !Maybe DateTime			//* When would you like to be notified about the task
	, priority			:: !TaskPriority			//* What is the current priority of this task?
	}
	
:: ProgressMeta =
	{ issuedAt			:: !DateTime				//* When was the task created
	, issuedBy			:: !User					//* By whom was the task created
	, status			:: !Stability				//* Is a maintask active,suspended,finished or excepted
	, firstEvent		:: !Maybe DateTime			//* When was the first work done on this task
	, latestEvent		:: !Maybe DateTime			//* When was the latest event on this task	
	}
		
//* Each task can be identified by two numbers:
// - A unique number identifying the top-level state
// - A unique number the task within the the state
:: TaskId		= TaskId !InstanceNo !TaskNo
:: InstanceNo	:== Int
:: TaskNo		:== Int

:: SessionId :== String

//* Types for manipulating task lists

:: TaskListId s
	= TopLevelTaskList			//*The top-level list of task instances
	| ParallelTaskList !TaskId	//*The list of task instances of a parallel task

:: TaskList a =
	{ listId	:: !(TaskListId a)
	, items		:: ![TaskListItem a]
	}

:: TaskListItem a =
	{ taskId			:: !TaskId
	, value				:: !TaskValue a
	, taskMeta			:: !TaskMeta
	, managementMeta	:: !Maybe ManagementMeta	//Only for detached tasks
	, progressMeta		:: !Maybe ProgressMeta		//Only for detached tasks
	}

:: SharedTaskList a	:==	ReadOnlyShared (TaskList a)

:: ParallelTaskType	
	= Embedded 
	| Detached !ManagementMeta

:: ParallelTask a	:== (SharedTaskList a) -> Task a

//* Users	
:: User
	= AnonymousUser !SessionId								//* An anonymous user identified only by a session id
	| AuthenticatedUser !UserId ![Role] !(Maybe UserTitle)	//* An authenticated user

//* Constrained users, to indicate who can work on a task
:: UserConstraint
	= AnyUser
	| UserWithId !UserId
	| UserWithRole !Role

class toUserConstraint a
where
	toUserConstraint :: a -> UserConstraint

instance toUserConstraint UserConstraint
instance toUserConstraint User
instance toUserConstraint String

:: UserId		:== String
:: Role			:== String
:: UserTitle	:== String			//* A descriptive name of a user (not used for identification)
	
//* Authentication
:: Credentials =
	{ username	:: !Username
	, password	:: !Password
	}
	
:: Password		= Password !String
:: Username		= Username !String

//* Predefined exception types used by library tasks

:: FileException		= FileException !FilePath !FileError
:: ParseException		= CannotParse !String
:: CallException		= CallFailed !OSError
:: SharedException		= SharedException !String
:: RPCException			= RPCException !String
:: OSException			= OSException !OSError
:: WorkOnException		= WorkOnNotFound | WorkOnEvalError | WorkOnDependencyCycle

derive JSONEncode		EUR, USD, BoundedInt, FormButton, ButtonState, User, Document, Hidden, Display, Editable, VisualizationHint, HtmlTag
derive JSONEncode		URL, Note, Username, Password, Date, Time, DateTime, Map, Void, Either, Timestamp, ComboChoice, RadioChoice, TreeChoice, GridChoice, CheckMultiChoice, Tree, TreeNode, Table
derive JSONEncode		EmailAddress, Action, HtmlInclude, ControlSize, FillControlSize, FillWControlSize, FillHControlSize
derive JSONDecode		EUR, USD, BoundedInt, FormButton, ButtonState, User, Document, Hidden, Display, Editable, VisualizationHint, HtmlTag
derive JSONDecode		URL, Note, Username, Password, Date, Time, DateTime, Map, Void, Either, Timestamp, ComboChoice, RadioChoice, TreeChoice, GridChoice, CheckMultiChoice, Tree, TreeNode, Table
derive JSONDecode		EmailAddress, Action, HtmlInclude, ControlSize, FillControlSize, FillWControlSize, FillHControlSize
derive gEq				EUR, USD, BoundedInt, FormButton, User, Document, Hidden, Display, Editable, VisualizationHint, HtmlTag
derive gEq				URL, Note, Username, Password, Date, Time, DateTime, Map, Void, Either, Timestamp, ComboChoice, RadioChoice, TreeChoice, GridChoice, CheckMultiChoice, Tree, TreeNode, Table
derive gEq				EmailAddress, Action, Maybe, JSONNode, (->), Dynamic, HtmlInclude, ControlSize, FillControlSize, FillWControlSize, FillHControlSize
derive JSONEncode		TaskListItem, ManagementMeta, TaskPriority, ProgressMeta, TaskValue, Stability
derive JSONDecode		TaskListItem, ManagementMeta, TaskPriority, ProgressMeta, TaskValue, Stability
derive gEq				TaskListItem, ManagementMeta, TaskPriority, ProgressMeta, TaskValue, Stability
derive gVisualizeText	TaskListItem, ProgressMeta, TaskValue, Stability
derive gVisualizeEditor	TaskListItem, ProgressMeta, TaskValue, Stability
derive gHeaders			TaskListItem, ProgressMeta, TaskValue, Stability
derive gGridRows		TaskListItem, ProgressMeta, TaskValue, Stability
derive gUpdate			TaskListItem, ProgressMeta, TaskValue, Stability
derive gDefaultMask		TaskListItem, ProgressMeta, TaskValue, Stability
derive gVerify			TaskListItem, ProgressMeta, TaskValue, Stability

derive class iTask	Credentials, Config, TaskId
derive class iTask	FileException, ParseException, CallException, SharedException, RPCException, OSException, WorkOnException
instance toString	FileException, ParseException, CallException, SharedException, RPCException, OSException, WorkOnException

instance toString Note
instance toString URL 
instance toString EUR
instance toString USD

instance toString Date
instance toString Time
instance toString DateTime
instance toString Document
instance toString User
instance toString Username
instance toString Password
instance toString TaskId
instance toString TaskPriority

instance toString FormButton
instance toString (TaskListId s)
instance fromString Date
instance fromString Time
instance fromString DateTime
instance fromString TaskId

instance == Note
instance == EUR
instance == USD
instance == Document
instance == User
instance == Username
instance == Password
instance == TaskId
instance == Date
instance == Time
instance == DateTime

instance < EUR
instance < USD
instance < Time
instance < Date
instance < DateTime
instance < User
instance < Username
instance < Password
instance < TaskId

instance + Time		//Basic addition, righthand argument is treated as interval (seconds are added first)
instance + Date		//Basic addition, righthand argument is treated as interval (days are added first)
instance + DateTime	//Basic addition, righthand argument is treated as interval
instance + EUR
instance + USD

instance - Time		//Basic subtraction, righthand argument is treated as interval (seconds are subtracted first)
instance - Date		//Naive fieldwise subtraction
instance - DateTime	//Naive fieldwise subtraction
instance - EUR
instance - USD

instance toInt EUR
instance toInt USD
instance zero EUR
instance zero USD

instance html Note
instance html URL 

//* Form buttons
:: FormButton 		= 
	{ label			:: !String
	, icon			:: !String
	, state			:: !ButtonState
	}
:: ButtonState		= NotPressed | Pressed

//* Simple tree type (used primarily for creating trees to choose from)
:: Tree a = Tree !.[.TreeNode a]
:: TreeNode a = Leaf !a | Node !a !.[TreeNode a]

instance Functor Tree

//* Represents a table consisting of headers, the displayed data cells & possibly a selection
:: Table = Table ![String] ![[HtmlTag]] !(Maybe Int)

//Generate a table from a value
toTable	:: ![a] -> Table | gHeaders{|*|} a & gGridRows{|*|} a & gVisualizeText{|*|} a

//* Represents the choice of one element from a list represented as combo box
:: ComboChoice v o = ComboChoice ![(!v,!o)] !(Maybe Int)
:: ComboChoiceNoView o = ComboChoiceNoView ![o] !(Maybe Int)

//* Represents the choice of one element from a list represented as radio buttons
:: RadioChoice v o = RadioChoice ![(!v,!o)] !(Maybe Int)
:: RadioChoiceNoView o = RadioChoiceNoView ![o] !(Maybe Int)

//* Represents a tree from with the user can choose one element
:: TreeChoice v o = TreeChoice !(Tree (!v,!o)) !(Maybe Int)
:: TreeChoiceNoView o = TreeChoiceNoView !(Tree o) !(Maybe Int)

//* Represents the choice of one element from a list represented as grid
//* (typically v is a record which's labels are used as headers)
:: GridChoice v o = GridChoice ![(!v,!o)] !(Maybe Int)
:: GridChoiceNoView o = GridChoiceNoView ![o] !(Maybe Int)

//* Represents the choice of one element from a set with a dynamic representation
:: DynamicChoice v o
	= DCCombo (ComboChoice v o)
	| DCRadio (RadioChoice v o)
	| DCTree  (TreeChoice v o)
	| DCGrid  (GridChoice v o)

:: DynamicChoiceNoView o
	= DCComboNoView (ComboChoiceNoView o)
	| DCRadioNoView (RadioChoiceNoView o)
	| DCTreeNoView	(TreeChoiceNoView o)
	| DCGridNoView	(GridChoiceNoView o)

derive JSONEncode	DynamicChoice,DynamicChoiceNoView
derive JSONDecode	DynamicChoice,DynamicChoiceNoView
derive gEq			DynamicChoice,DynamicChoiceNoView

/**
* Interface for types representing choices of one element out of a set of options.
* There are different kinds of containers for such options (e.g. lists, trees, ...).
* Each option consists of an actual value (o) & a view value shown to the user (v).
*/
class Choice t
where
	//* Selects the given option, if not present in list of options selection is cleared
	selectOption			:: !o !(t v o)					-> t v o | gEq{|*|} o
	//* Gets the current selection assuming it is present (a valid choice always has a selection)
	getSelection			:: !(t v o)						-> o
	//* Gets the current selection if present
	getMbSelection			:: !(t v o)						-> Maybe o
	//* Gets the current selection's view if present
	getMbSelectionView		:: !(t v o)						-> Maybe v

class ChoiceNoView t
where
	//* Selects the given option, if not present in list of options selection is cleared
	selectOptionNoView			:: !o !(t o)					-> t o | gEq{|*|} o
	//* Gets the current selection assuming it is present (a valid choice always has a selection)
	getSelectionNoView			:: !(t o)						-> o
	//* Gets the current selection if present
	getMbSelectionNoView		:: !(t o)						-> Maybe o

instance Choice ComboChoice,RadioChoice,TreeChoice,GridChoice,DynamicChoice
instance ChoiceNoView DynamicChoiceNoView,RadioChoiceNoView,ComboChoiceNoView,TreeChoiceNoView,GridChoiceNoView

//* Represents the choice of a number of items from a list
:: CheckMultiChoice v o = CheckMultiChoice ![(!v,!o)] ![Int]

/**
* Interface for types representing choices a number of elements out of a set of options.
* There are different kinds of containers for such options (e.g. lists, trees, ...).
* Each option consists of an actual value (o) & a view value shown to the user (v).
*/
class MultiChoice choiceType
where
	//* Selects the given options, selections not present in list of options are ignored
	selectOptions			:: ![o] !(choiceType v o)					-> choiceType v o | gEq{|*|} o
	//* Gets the current selections
	getSelections			:: !(choiceType v o)						-> [o]
	//* Gets the current selection's views
	getSelectionViews		:: !(choiceType v o)						-> [v]

instance MultiChoice CheckMultiChoice

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

//* tasks can have three levels of priority
:: TaskPriority		= HighPriority					
					| NormalPriority
					| LowPriority
					
formatPriority	:: !TaskPriority	-> HtmlTag
						
instance toString Stability
instance == Stability

//Define initial meta attributes
TASK_ATTRIBUTE	:== "task"
LIST_ATTRIBUTE	:== "list"
TITLE_ATTRIBUTE	:== "title"
HINT_ATTRIBUTE	:== "hint"
ERROR_ATTRIBUTE	:== "error"
ICON_ATTRIBUTE	:== "icon"
TIME_ATTRIBUTE	:== "time"	//Task time, used for ordering but not real time

class descr d
where
	initAttributes :: !d -> [TaskAttribute]

instance descr Void
instance descr String	//Hint
instance descr (!String, !String) //Title, Hint
instance descr (!Icon, !String, !String) //Icon, Title , Hint
instance descr Title
instance descr Hint
instance descr Icon
instance descr Attribute

instance descr Att
instance descr [d] | descr d

:: Attribute			= Attribute !String !String
:: Att					= E.a: Att !a & descr a

:: Title				= Title !String
:: Hint					= Hint !String
:: Window				= Window
:: Icon					= Icon !String
						| IconView
						| IconEdit

noMeta :: ManagementMeta

//Configuration
:: Config =
	{ sessionTime		:: !Int				// Time (in seconds) before inactive sessions are garbage collected. Default is 3600 (one hour).
	, smtpServer		:: !String			// The smtp server to use for sending e-mails
	}

/*
* To allow the specification of a followup action to their current task
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
			| ActionRefresh

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
