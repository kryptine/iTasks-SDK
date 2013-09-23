definition module iTasks.API.Core.SystemTypes
/**
* This module provides types for all the globally shared concepts
* of the iTasks framework.
*/
//import GenEq
//import Data.Maybe, Text.JSON, Data.Void, Data.Either, System.FilePath, Text.HTML, Data.Error, System.File, System.OS

import iTasks.Framework.Store

// TODO: Remove these big-bang imports
import Data.Void, Text.HTML, GenEq

from Data.Maybe import :: Maybe
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from Data.Void import :: Void
from Data.Either import :: Either
from System.FilePath import :: FilePath
from Text.HTML import :: HtmlTag, :: HtmlAttr
from Data.Functor import class Functor
from Data.Error import :: MaybeError
from System.File import :: FileError
from System.OSError import :: OSError, :: OSErrorMessage, :: OSErrorCode
from StdOverloaded import class +, class -, class <, class zero, class fromString, class toInt
from StdGeneric import :: ConsPos
from GenEq import generic gEq
from Data.Map 				import :: Map
from Data.Map 				import qualified get
from Text.HTML 				import class html
from System.Time				import :: Timestamp
from iTasks.Framework.IWorld			import :: IWorld
from iTasks.Framework.UIDefinition		import :: UIDef, :: UIControlStack, :: UIAnnotatedControls, :: UIControl, :: UISize, :: UIDirection, :: UISideSizes, :: UIMinSize, :: UIAttributes
from iTasks.Framework.Task				import :: Task, :: TaskId
from iTasks.Framework.Generic				import class iTask
from iTasks.Framework.Generic.Interaction	import generic gEditor, generic gEditMeta, generic gVerify, generic gUpdate, :: VSt, :: VisualizationResult,:: EditMeta, :: VerifyOptions
from iTasks.Framework.Generic.Visualization	import generic gVisualizeText, :: VisualizationFormat(..), visualizeAsText
from iTasks.Framework.Generic.Defaults		import generic gDefault
from iTasks.Framework.Shared			import :: ReadWriteShared, :: ReadOnlyShared, :: RWShared
from iTasks.API.Core.Client.Interface	import :: JSWorld, :: JSVal
from iTasks.API.Core.LayoutCombinators	import :: LayoutRules
import iTasks.Framework.SerializationGraphCopy

//****************************************************************************//
// Common data types that have specialized user interfaces
//****************************************************************************//

//* E-mail addresses
:: EmailAddress	= EmailAddress !String

//* Uniform resource locators
:: URL			= URL !String
instance toString	URL
instance html		URL

//* Plain text notes 
:: Note			= Note !String
instance toString	Note
instance html		Note
instance ==			Note

//* Source code
:: CleanCode	= CleanCode !String
instance toString CleanCode

//* Money (ISO4217 currency codes are used)
:: EUR 			= EUR !Int		//Euros (amount in cents)
:: USD 			= USD !Int		//Dollars (amount in cents)

instance toString	EUR, USD
instance + 			EUR, USD
instance - 			EUR, USD
instance == 		EUR, USD
instance < 			EUR, USD
instance toInt		EUR, USD
instance zero		EUR, USD

//* (Local) date and time
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

//Date addition" righthand argument is treated as interval (days are added first)
//Time addition: righthand argument is treated as interval (seconds are added first)
//Time subtraction: righthand argument is treated as interval (seconds are subtracted first)
instance toString	Date, Time, DateTime
instance fromString	Date, Time, DateTime
instance +			Date, Time, DateTime
instance -			Date, Time, DateTime
instance ==			Date, Time, DateTime
instance <			Date, Time, DateTime

//* Documents
:: Document =
	{ documentId	:: !DocumentId				//*A unique identifier of the document
	, contentUrl	:: !String					//*A url to where the document can be downloaded
	, name			:: !String					//*The filename of a document
	, mime			:: !String					//*The mime type of the document
	, size			:: !Int						//*The filesize in bytes
	}
:: DocumentId	:== String

instance toString	Document
instance ==			Document

//* Authentication
:: Credentials =
	{ username	:: !Username
	, password	:: !Password
	}

:: Username		= Username !UserId

:: Password		= Password !String

instance toString		Username, Password
instance ==				Username, Password
instance <				Username, Password

derive JSONEncode		EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password 
derive JSONDecode		EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password
derive gDefault			EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password
derive gEq				EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password

derive gVisualizeText	EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password
derive gEditor 			EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password
derive gEditMeta		EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password
derive gUpdate			EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password 
derive gVerify			EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password

derive class iTask	Credentials

//* Common exceptions used by API tasks

:: FileException		= FileException !FilePath !FileError
:: ParseException		= CannotParse !String
:: CallException		= CallFailed !OSError
:: SharedException		= SharedException !String
:: RPCException			= RPCException !String
:: OSException			= OSException !OSError
:: WorkOnException		= WorkOnNotFound | WorkOnEvalError | WorkOnDependencyCycle

derive class iTask	FileException, ParseException, CallException, SharedException, RPCException, OSException, WorkOnException
instance toString	FileException, ParseException, CallException, SharedException, RPCException, OSException, WorkOnException

//****************************************************************************//
// Low level data types that can be used to construct more fine grained user
// experiences.
// These types map to specific user interface components.
//****************************************************************************//

//* A sliding scale
:: Scale =
	{ min	:: Int
	, cur	:: Int
	, max	:: Int
	}
	
//* Progress bars
:: Progress =
	{ progress		:: !ProgressAmount 	//*Value between 0.0 and 1.0 indicating how much progress has been made
	, description	:: !String			//*Description of how much progress has been made
	}
:: ProgressAmount
	= ProgressUndetermined	//Value for progress that cannot be estimated
	| ProgressRatio Real	//Value between 0.0 and 1.0 that defines the ratio of progress

//* Inclusion of external html files
:: HtmlInclude	= HtmlInclude String

//* Form buttons
:: FormButton 		= 
	{ label			:: !String
	, icon			:: !String
	, state			:: !ButtonState
	}
:: ButtonState		= NotPressed | Pressed

instance toString FormButton

//* Table consisting of headers, the displayed data cells & possibly a selection
:: Table = Table ![String] ![[HtmlTag]] !(Maybe Int)

toTable	:: ![a] -> Table | gEditMeta{|*|} a & gVisualizeText{|*|} a

//* Simple tree type (used primarily for creating trees to choose from)
:: Tree a = Tree !.[.TreeNode a]
:: TreeNode a = Leaf !a | Node !a !.[TreeNode a]

//* More elaborate tree type for grouping elements hierarchically from a choice
:: ChoiceTree v =
    { label     :: v
    , icon      :: Maybe String
    , value     :: ChoiceTreeValue
    , type      :: ChoiceTreeType v
    }
:: ChoiceTreeValue  = ChoiceNode !Int | GroupNode !String
:: ChoiceTreeType v = LeafNode | CollapsedNode [ChoiceTree v] | ExpandedNode [ChoiceTree v]

choiceTreeChildren :: (ChoiceTree v) -> [ChoiceTree v]
ifExpandedGroup :: String [ChoiceTreeValue] [ChoiceTree v] -> ChoiceTreeType v
ifExpandedChoice :: Int [ChoiceTreeValue] [ChoiceTree v] -> ChoiceTreeType v

instance == ChoiceTreeValue

instance Functor Tree
instance Functor ChoiceTree

derive JSONEncode		Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive JSONDecode		Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gDefault			Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gEq				Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gVisualizeText	Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gEditor	        Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gEditMeta		Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gUpdate			Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gVerify			Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode, ChoiceTree, ChoiceTreeValue, ChoiceTreeType

//* Represents the choice of one element from a list represented as combo box
:: ComboChoice v = ComboChoice ![v] !(Maybe Int)

//* Represents the choice of one element from a list represented as radio buttons
:: RadioChoice v = RadioChoice ![v] !(Maybe Int)

//* Bundles a tree with options with a selection
:: TreeChoice v = TreeChoice ![ChoiceTree v] !(Maybe Int)

//* Represents the choice of one element from a list represented as grid
//* (typically v is a record which's labels are used as headers)
:: GridChoice v = GridChoice ![v] !(Maybe Int)

//* Represents the choice of one element from a set with a dynamic representation
:: DynamicChoice v
	= DCCombo (ComboChoice v)
	| DCRadio (RadioChoice v)
	| DCTree  (TreeChoice v)
	| DCGrid  (GridChoice v)

derive JSONEncode		ComboChoice, RadioChoice, TreeChoice, GridChoice, DynamicChoice
derive JSONEncode		CheckMultiChoice
derive JSONDecode		ComboChoice, RadioChoice, TreeChoice, GridChoice, DynamicChoice
derive JSONDecode		CheckMultiChoice
derive gDefault			ComboChoice, RadioChoice, TreeChoice, GridChoice, DynamicChoice
derive gDefault			CheckMultiChoice
derive gEq				ComboChoice, RadioChoice, TreeChoice, GridChoice, DynamicChoice
derive gEq				CheckMultiChoice
derive gVisualizeText	ComboChoice, RadioChoice, TreeChoice, GridChoice, DynamicChoice
derive gVisualizeText	CheckMultiChoice
derive gEditor	        ComboChoice, RadioChoice, TreeChoice, GridChoice, DynamicChoice
derive gEditor	        CheckMultiChoice
derive gEditMeta		ComboChoice, RadioChoice, TreeChoice, GridChoice, DynamicChoice
derive gEditMeta		CheckMultiChoice
derive gUpdate			ComboChoice, RadioChoice, TreeChoice, GridChoice, DynamicChoice
derive gUpdate			CheckMultiChoice
derive gVerify			ComboChoice, RadioChoice, TreeChoice, GridChoice, DynamicChoice
derive gVerify			CheckMultiChoice

/**
* Interface for types representing choices of one element out of a set of options.
* There are different kinds of containers for such options (e.g. lists, trees, ...).
* Each option consists of an actual value (o) & a view value shown to the user (v).
*/
class Choice t
where
	//* Gets the current selection's view if present
	getSelectionView		:: !(t v)						-> Maybe v
    //* Sets the selection based on the view value
    setSelectionView        :: !(Maybe v) !(t v)            -> t v | gEq {|*|} v
    //* Gets the index of the selection
    getSelectionIndex       :: !(t v)                       -> Maybe Int
    //* Sets the index of the selection
    setSelectionIndex       :: !(Maybe Int) !(t v)          -> t v

instance Choice ComboChoice,RadioChoice,TreeChoice,GridChoice,DynamicChoice

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

//****************************************************************************//
// Wrapper types for guiding the generic visualization process.
// These types can be used as annotations
//****************************************************************************//

:: VisualizationHint a 	= VHEditable a
					   	| VHDisplay a
					   	| VHHidden a

fromVisualizationHint	:: !(VisualizationHint .a) -> .a
toVisualizationHint		:: !.a -> VisualizationHint .a

//* Value is always rendered within a form as editor field
:: Editable a 			= Editable a		

fromEditable			:: !(Editable .a) -> .a
toEditable				:: !.a -> Editable .a

//* Value is always rendered within a form as a static element
:: Display a 			= Display a			

fromDisplay				:: !(Display .a) -> .a
toDisplay				:: !.a -> Display .a

//* Value is never rendered
:: Hidden a 			= Hidden a			

fromHidden				:: !(Hidden .a) -> .a
toHidden				:: !.a -> Hidden .a

derive JSONEncode		Hidden, Display, Editable, VisualizationHint
derive JSONDecode		Hidden, Display, Editable, VisualizationHint
derive gDefault			Hidden, Display, Editable, VisualizationHint
derive gEq				Hidden, Display, Editable, VisualizationHint
derive gVisualizeText	Hidden, Display, Editable, VisualizationHint
derive gEditor			Hidden, Display, Editable, VisualizationHint
derive gEditMeta			Hidden, Display, Editable, VisualizationHint
derive gUpdate			Hidden, Display, Editable, VisualizationHint
derive gVerify			Hidden, Display, Editable, VisualizationHint


//****************************************************************************//
// Framework types.
// These types define the user-facing representations of the iTasks framework
// It is generally not necessary to create values of these types yourself, but
// you may read them when interacting with the framework
//****************************************************************************//

//* Task results
:: TaskValue a		= NoValue				
					| Value !a !Stability 

StableValue   a :== Value a True
UnstableValue a :== Value a False

instance Functor TaskValue
			
:: TaskTime			:== Int

:: Stability		:== Bool

//* Each task instance can be identified by two numbers:
// - A unique number identifying the top-level state
// - A unique number the task within the the state
:: TaskId		= TaskId !InstanceNo !TaskNo
:: InstanceNo	:== Int
:: TaskNo		:== Int

:: SessionId	:== String

instance toString	TaskId
instance fromString	TaskId
instance ==			TaskId
instance <			TaskId

//* Meta-data of tasks
:: ManagementMeta =
	{ title				:: !Maybe String			//* Title to identify the task
	, worker			:: !UserConstraint			//* Who has to do the task?
	, role				:: !Maybe Role				//* What role does a worker need to do the task
	, startAt			:: !Maybe DateTime			//* When is the task supposed to start
	, completeBefore	:: !Maybe DateTime			//* When does the task need to be completed
	, notifyAt			:: !Maybe DateTime			//* When would you like to be notified about the task
	, priority			:: !TaskPriority			//* What is the current priority of this task?
    , tags              :: ![String]                //* Custom labels for categorizing
    , properties        :: !Map String String       //* Custom untyped properties
	}
		
:: ProgressMeta =
	{ issuedAt			:: !DateTime				//* When was the task created
	, issuedBy			:: !User					//* By whom was the task created
	, stable			:: !Stability				//* Is a maintask stable
	, firstEvent		:: !Maybe DateTime			//* When was the first work done on this task
	, latestEvent		:: !Maybe DateTime			//* When was the latest event on this task	
	}

//* Tasks can have three levels of priority
:: TaskPriority		= LowPriority					
					| NormalPriority
					| HighPriority

instance toString	TaskPriority

//* Representations of task lists
:: TaskListId s
	= TopLevelTaskList			//*The top-level list of task instances
	| ParallelTaskList !TaskId	//*The list of task instances of a parallel task

instance toString (TaskListId s)

:: TaskList a =
	{ listId	:: !(TaskListId a)
	, items		:: ![TaskListItem a]
    , selfId    :: !TaskId
	}

:: TaskListItem a =
	{ taskId			:: !TaskId
    , listId            :: !TaskId
    , name              :: !Maybe String            //Optional identifier, for example for adding tasks just once
	, value				:: !TaskValue a
	, managementMeta	:: !Maybe ManagementMeta	//Only for detached tasks
	, progressMeta		:: !Maybe ProgressMeta		//Only for detached tasks
	}

:: SharedTaskList a	:==	ReadWriteShared (TaskList a) [(TaskId,ManagementMeta)]

:: ParallelTaskType	
	= Embedded                                    //Simplest embedded
    | NamedEmbedded !String                       //Embedded with name
	| Detached !ManagementMeta !Bool              //Management meta and flag whether the task should be started at once
    | NamedDetached !String !ManagementMeta !Bool //Detached with name

:: ParallelTask a	:== (SharedTaskList a) -> Task a

/** Interaction masks contain information about a value as it is being edited
*   in an interactive task.
*/  
:: InteractionMask
	= Untouched								//The value has not been touched by the user
	| Touched								//The value has been touched by the user, now it makes sense to check the input
    | TouchedUnparsed !JSONNode              //The user has edited the value to something that cannot be parsed to a valid value
	| TouchedWithState !JSONNode			//Some components need to keep local state that can't be encoded in the value
	| Blanked								//The value was previously touched, but has been made blank again
	| CompoundMask ![InteractionMask]	    //The value is a compound structure of which some parts are, and some aren't touched

:: MaskedValue a :== (a,InteractionMask)

subMasks	:: !Int InteractionMask -> [InteractionMask]
toPairMask	:: !Int !InteractionMask -> InteractionMask
isTouched	:: !InteractionMask -> Bool

:: Verification
    = CorrectValue !(Maybe String)
    | WarningValue !String
    | IncorrectValue !String
    | UnparsableValue
    | MissingValue
    | CompoundVerification [Verification]

:: VerifiedValue a :== (a,InteractionMask,Verification)

subVerifications :: !Int Verification -> [Verification]
toPairVerification :: !Int !Verification -> Verification
fromPairVerification :: !Int !Verification -> Verification

derive JSONEncode InteractionMask, Verification
derive JSONDecode InteractionMask, Verification

//* Datapaths identify sub structures in a composite structure
:: DataPath :== [Int]

//Generate the editorId string for a given datapath
editorId 				:: !DataPath 		-> String
s2dp					:: !String			-> DataPath

//* User identification
:: User
    = SystemUser                                            //* The global user that is used when events do not originate from a session
	| AnonymousUser !SessionId								//* An anonymous user identified only by a session id
	| AuthenticatedUser !UserId ![Role] !(Maybe UserTitle)	//* An authenticated user

instance toString	User
instance ==			User
instance <			User

//* User constraints which indicate who can work on a task
:: UserConstraint
	= AnyUser
	| UserWithId !UserId
	| UserWithRole !Role

class toUserConstraint a
where
	toUserConstraint :: !a -> UserConstraint

instance toUserConstraint UserConstraint
instance toUserConstraint User
instance toUserConstraint UserId

:: UserId		:== String
:: Role			:== String
:: UserTitle	:== String			//* A descriptive name of a user (not used for identification)
	
//* Framework configuration
:: Config =
	{ sessionTime		:: !Int		//* Time (in seconds) before inactive sessions are garbage collected. Default is 3600 (one hour).
	, smtpServer		:: !String	//* The smtp server to use for sending e-mails
	}

//* External (operating system) process status
:: ProcessStatus
	= RunningProcess !String
	| CompletedProcess !Int

//* Next task actions
:: Action	= Action !ActionName ![ActionOption]

:: ActionName	:== String	//Locally unique identifier for actions
:: ActionOption
	= ActionKey		!Hotkey		//Specifies a hotkey for the action. 
	| ActionWeight	!Int		//Specifies a weight for specific sorting in menus
	| ActionIcon	!String		//Specifies a symbolic icon name e.g. 'close' or 'ok' (the application styling dereferences this to an image)
	| ActionTrigger	!Trigger	//Special event that triggers this action (other than clicking the action button/menu item)

actionName		:: !Action -> ActionName
actionIcon 		:: !Action -> Maybe String
actionWeight	:: !Action -> Int			//Default weight is 0
			
:: Hotkey =	{ key	:: !Key
			, ctrl	:: !Bool
			, alt	:: !Bool
			, shift	:: !Bool
			}

:: Key :== Int //Key code

:: Trigger	= DoubleClick	//Currently only doubleclick is supported as special trigger

//Common action constants with predefined options
ActionOk		:== Action "Ok"				[ActionIcon "ok", ActionKey (unmodified KEY_ENTER)]
ActionCancel	:==	Action "Cancel"			[ActionIcon "cancel", ActionKey (unmodified KEY_ESC)]
ActionYes		:== Action "Yes"			[ActionIcon "yes"]
ActionNo		:== Action "No"				[ActionIcon "no"]
ActionNext		:== Action "Next"			[ActionIcon "next"]
ActionPrevious	:== Action "Previous"		[ActionIcon "previous"]
ActionFinish	:== Action "Finish"			[ActionIcon "finish"]
ActionContinue	:==	Action "Continue"		[ActionIcon "continue", ActionKey (unmodified KEY_ENTER)]
ActionOpen		:== Action "/File/Open"		[ActionIcon "open", ActionKey (ctrl KEY_O)]
ActionSave		:== Action "/File/Save" 	[ActionIcon "save", ActionKey (ctrl KEY_S)]
ActionSaveAs 	:== Action "/File/Save as"	[ActionIcon "save"]
ActionQuit		:== Action "/File/Quit"		[ActionIcon "quit"]
ActionHelp		:==	Action "/Help/Help"		[ActionIcon "help"]
ActionAbout		:== Action "/Help/About"	[ActionIcon "about"]
ActionFind		:== Action "/Edit/Find"		[ActionIcon "find", ActionKey (ctrl KEY_F)]
ActionNew		:== Action "New"			[ActionIcon "new", ActionKey (ctrl KEY_N)]
ActionEdit		:== Action "Edit"			[ActionIcon "edit"]
ActionDelete	:== Action "Delete"			[ActionIcon "delete", ActionKey (unmodified KEY_DELETE)]
ActionRefresh	:== Action "Refresh"		[ActionIcon "refresh", ActionKey (unmodified KEY_F5)]
ActionClose		:==	Action "Close"			[ActionIcon "close", ActionKey (unmodified KEY_ESC)]
	
//Common key codes
KEY_ENTER		:== 13
KEY_ESC			:== 27
KEY_BACKSPACE	:== 8
KEY_DELETE		:== 46
KEY_LEFT		:== 37
KEY_UP			:== 38
KEY_RIGHT		:== 39
KEY_DOWN		:== 40

KEY_A		:== 65
KEY_B		:== 66
KEY_C		:== 67
KEY_D		:== 68
KEY_E		:== 69
KEY_F		:== 70
KEY_G		:== 71
KEY_H		:== 72
KEY_I		:== 73
KEY_J		:== 74
KEY_K		:== 75
KEY_L		:== 76
KEY_M		:== 77
KEY_N		:== 78
KEY_O		:== 79
KEY_P		:== 80
KEY_Q		:== 81
KEY_R		:== 82
KEY_S		:== 83
KEY_T		:== 84
KEY_U		:== 85
KEY_V		:== 86
KEY_W		:== 87
KEY_X		:== 88
KEY_Y		:== 89
KEY_Z		:== 90

KEY_F1		:== 112
KEY_F2		:== 113
KEY_F3		:== 114
KEY_F4		:== 115
KEY_F5		:== 116
KEY_F6		:== 117
KEY_F7		:== 118
KEY_F8		:== 119
KEY_F9		:== 120
KEY_F10		:== 121
KEY_F11		:== 122
KEY_F12		:== 123

//Common modifiers
unmodified key	:== {key=key,ctrl=False,alt=False,shift=False}
ctrl key		:== {key=key,ctrl=True,alt=False,shift=False}
alt key			:== {key=key,ctrl=False,alt=True,shift=False}
shift key		:== {key=key,ctrl=False,alt=False,shift=True}

derive JSONEncode		TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action, ActionOption, Hotkey, Trigger
derive JSONDecode		TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action, ActionOption, Hotkey, Trigger
derive gDefault			TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action, ActionOption, Hotkey, Trigger
derive gEq				TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action, ActionOption, Hotkey, Trigger

derive gVisualizeText	TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action, ActionOption, Hotkey, Trigger
derive gEditor			TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action, ActionOption, Hotkey, Trigger
derive gEditMeta		TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action, ActionOption, Hotkey, Trigger
derive gUpdate			TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action, ActionOption, Hotkey, Trigger
derive gVerify			TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action, ActionOption, Hotkey, Trigger

derive class iTask		TaskId, Config, ProcessStatus

//****************************************************************************//
// Types for task meta-data
//****************************************************************************//

//* Task prompt attributes
:: Attribute	= Attribute !String !String
				| TaskAttribute !String
				| TitleAttribute !String
				| HintAttribute !String
				| ValidAttribute
				| ErrorAttribute !String
				| IconAttribute !String
				| CreatedAtAttribute !TaskTime
				| LastEventAttribute !TaskTime
				| FloatAttribute
				
//Define initial meta attributes
TASK_ATTRIBUTE			:== "task"
VALUE_ATTRIBUTE			:== "value"
TITLE_ATTRIBUTE			:== "title"
HINT_ATTRIBUTE			:== "hint"
VALID_ATTRIBUTE			:== "valid"
WARNING_ATTRIBUTE		:== "warning"
ERROR_ATTRIBUTE			:== "error"
LABEL_ATTRIBUTE			:== "label"
PREFIX_ATTRIBUTE		:== "prefix"
POSTFIX_ATTRIBUTE		:== "postfix"
ICON_ATTRIBUTE			:== "icon"
SCREEN_ATTRIBUTE        :== "screen"
CREATED_AT_ATTRIBUTE	:== "createdate"//Creation task time, used for ordering but not real time
LAST_EVENT_ATTRIBUTE	:== "lastevent"	//Last event task time, used for ordering but not real time

//Preferred container attribute for abstract containers. Does not have to be honoured by layouts
CONTAINER_ATTRIBUTE		:==	"container"	//Container preference for layout functions. Possible preferences: "container", "panel", or "window"

:: Att				= E.a: Att !a & descr a

:: Title			= Title !String
:: Hint				= Hint !String

:: Icon				= Icon !String
					| IconView
					| IconEdit



//Make the UI definition of the interaction prompt
class descr d
where
	toPrompt		:: !d -> UIDef

instance descr Void							//No prompt
instance descr String						//Simple instruction
instance descr (!String, !String)			//Title attribute + instruction
instance descr (!Icon, !String, !String)	//Icon attribute, title attribute, and instruction
//instance descr (!Icon, !Title)				//Icon attribute, title attribute 
instance descr Title
instance descr Hint
instance descr Icon
instance descr Attribute

instance descr Att
instance descr [d] | descr d

derive JSONEncode		Icon
derive JSONDecode		Icon
derive gDefault			Icon
derive gEq				Icon
derive gVisualizeText	Icon
derive gEditor          Icon	
derive gEditMeta		Icon
derive gUpdate			Icon
derive gVerify			Icon



//Task evaluation tuning directives, for increasing performance
:: LazyRefresh = LazyRefresh //If you tune a task in a parallel set with this directive, it not be evaluated unless its focused

//****************************************************************************//
// Generic instances for common library types
//****************************************************************************//

derive JSONEncode		HtmlTag, Map, Void, Either, Timestamp
derive JSONDecode		HtmlTag, Map, Void, Either, Timestamp
derive gEq				HtmlTag, Map, Void, Either, Timestamp, Maybe, JSONNode, (->), Dynamic
