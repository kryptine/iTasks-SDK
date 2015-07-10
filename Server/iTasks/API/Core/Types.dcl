definition module iTasks.API.Core.Types
/**
* This module provides types for all the globally shared concepts
* of the iTasks framework.
*/
//import GenEq
//import Data.Maybe, Text.JSON, Data.Void, Data.Either, System.FilePath, Text.HTML, Data.Error, System.File, System.OS

import iTasks._Framework.Store

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
from GenLexOrd import generic gLexOrd, :: LexOrd
from Data.Map 				import :: Map
from Data.Map 				import qualified get
from Text.HTML 				import class html
from System.Time				import :: Timestamp
from iTasks._Framework.IWorld			import :: IWorld
from iTasks._Framework.UIDefinition		import :: UIDef, :: UIForm, :: UIControl, :: UISize, :: UIDirection, :: UISideSizes, :: UIBound, :: UIAttributes
from iTasks._Framework.Task				import :: Task, :: TaskId
from iTasks._Framework.Generic				import class iTask
from iTasks._Framework.Generic.Interaction	import generic gEditor, generic gEditMeta, generic gVerify, generic gUpdate, :: VSt, ::USt, :: VisualizationResult,:: EditMeta, :: VerifyOptions
from iTasks._Framework.Generic.Visualization	import generic gText, :: TextFormat(..), toMultiLineText
from iTasks._Framework.Generic.Defaults		import generic gDefault
from iTasks._Framework.SDS import :: ReadWriteShared, :: ReadOnlyShared, :: RWShared
from iTasks.API.Core.Client.Interface	import :: JSWorld, :: JSVal
from iTasks.API.Core.LayoutCombinators	import :: LayoutRules
import iTasks._Framework.Serialization

class TFunctor f where
    tmap :: (a -> b) (f a) -> f b | iTask a & iTask b

(@$) infixl 1 :: (a -> b) (f a) -> f b | iTask a & iTask b & TFunctor f

class TApplicative f | TFunctor f where
    (<#>)  :: (f (a -> b)) (f a) -> f b | iTask a & iTask b
    return :: a -> f a | iTask a

class TMonad m | TApplicative m where
    (>>=) infixl 1 :: (m a) (a -> m b) -> m b | iTask a & iTask b

instance TFunctor Task
instance TApplicative Task
instance TMonad Task

instance TFunctor Maybe
instance TApplicative Maybe
instance TMonad Maybe

instance TFunctor []
instance TApplicative []
instance TMonad []

instance TFunctor (Either e)
instance TApplicative (Either e)
instance TMonad (Either e)

//****************************************************************************//
// Common data types that have specialized user interfaces
//****************************************************************************//

//* E-mail addresses
:: EmailAddress	= EmailAddress !String
instance toString	EmailAddress
instance html		EmailAddress

//* Phone number
:: PhoneNumber = PhoneNumber !String
instance toString	PhoneNumber
instance html		PhoneNumber

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

//Format datetime as padded string: YYYYmmddHHMMss
paddedDateTimeString :: DateTime -> String

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

derive JSONEncode		EmailAddress, PhoneNumber, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document 
derive JSONDecode		EmailAddress, PhoneNumber, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document
derive gDefault			EmailAddress, PhoneNumber, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document
derive gEq				EmailAddress, PhoneNumber, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document

derive gText	        EmailAddress, PhoneNumber, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document
derive gEditor 			EmailAddress, PhoneNumber, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document
derive gEditMeta		EmailAddress, PhoneNumber, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document
derive gUpdate			EmailAddress, PhoneNumber, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document 
derive gVerify			EmailAddress, PhoneNumber, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document

//* Common exceptions used by API tasks

:: FileException		= FileException !FilePath !FileError
:: ParseException		= CannotParse !String
:: CallException		= CallFailed !OSError
:: SharedException		= SharedException !String
:: RPCException			= RPCException !String
:: OSException			= OSException !OSError
:: WorkOnException		= WorkOnNotFound | WorkOnEvalError | WorkOnDependencyCycle //FIXME: Rename to attachment exceptions

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

toTable	:: ![a] -> Table | gText{|*|} a

//* More elaborate tree type for grouping elements hierarchically from a choice
:: ChoiceTree v =
    { label     :: v
    , icon      :: Maybe String
    , value     :: ChoiceTreeValue
    , type      :: ChoiceTreeType v
    }
//There are two types of nodes in a choice tree
//Some nodes represent an element of the list of choices (ChoiceNode) and you
//indicate which element by its index in the list of options.
//Other nodes are only used for grouping the available options (GroupNode) and don't indicate
//a choice. For these nodes a unique name must be supplied to make it possible to
//track if the node is collapsed or expanded
:: ChoiceTreeValue
    = ChoiceNode !Int
    | GroupNode !String

:: ChoiceTreeType v = LeafNode | CollapsedNode [ChoiceTree v] | ExpandedNode [ChoiceTree v]

choiceTreeChildren :: (ChoiceTree v) -> [ChoiceTree v]
ifExpandedGroup :: String [ChoiceTreeValue] [ChoiceTree v] -> ChoiceTreeType v
ifExpandedChoice :: Int [ChoiceTreeValue] [ChoiceTree v] -> ChoiceTreeType v

instance == ChoiceTreeValue

instance Functor ChoiceTree

derive JSONEncode		Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive JSONDecode		Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gDefault			Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gEq				Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gText	        Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gEditor	        Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gEditMeta		Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gUpdate			Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gVerify			Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, ChoiceTree, ChoiceTreeValue, ChoiceTreeType

//* Represents the choice of one element from a list represented as combo box
:: ComboChoice v = ComboChoice ![v] !(Maybe Int)

//* Represents the choice of one element from a list represented as radio buttons
:: RadioChoice v = RadioChoice ![v] !(Maybe Int)

//* Represents the choice of one element from a list represented as a list of html buttons
:: ListChoice v = ListChoice ![v] !(Maybe Int)

//* Bundles a tree with options with a selection
:: TreeChoice v = TreeChoice ![ChoiceTree v] !(Maybe Int)

//* Represents the choice of one element from a list represented as grid
//* (typically v is a record which's labels are used as headers)
:: GridChoice v = GridChoice ![v] !(Maybe Int)

//* Represents the choice of one element from a set with a dynamic representation
:: DynamicChoice v
	= DCCombo (ComboChoice v)
	| DCRadio (RadioChoice v)
	| DCList  (ListChoice v)
	| DCTree  (TreeChoice v)
	| DCGrid  (GridChoice v)

derive JSONEncode		ComboChoice, RadioChoice, ListChoice, TreeChoice, GridChoice, DynamicChoice
derive JSONEncode		CheckMultiChoice
derive JSONDecode		ComboChoice, RadioChoice, ListChoice, TreeChoice, GridChoice, DynamicChoice
derive JSONDecode		CheckMultiChoice
derive gDefault			ComboChoice, RadioChoice, ListChoice, TreeChoice, GridChoice, DynamicChoice
derive gDefault			CheckMultiChoice
derive gEq				ComboChoice, RadioChoice, ListChoice, TreeChoice, GridChoice, DynamicChoice
derive gEq				CheckMultiChoice
derive gText	        ComboChoice, RadioChoice, ListChoice, TreeChoice, GridChoice, DynamicChoice
derive gText	        CheckMultiChoice
derive gEditor	        ComboChoice, RadioChoice, ListChoice, TreeChoice, GridChoice, DynamicChoice
derive gEditor	        CheckMultiChoice
derive gEditMeta		ComboChoice, RadioChoice, ListChoice, TreeChoice, GridChoice, DynamicChoice
derive gEditMeta		CheckMultiChoice
derive gUpdate			ComboChoice, RadioChoice, ListChoice, TreeChoice, GridChoice, DynamicChoice
derive gUpdate			CheckMultiChoice
derive gVerify			ComboChoice, RadioChoice, ListChoice, TreeChoice, GridChoice, DynamicChoice
derive gVerify			CheckMultiChoice

/**
* Interface for types representing choices of one element out of a set of options.
* There are different kinds of containers for such options (e.g. lists, trees, ...).
* Each option consists of an actual value (o) & a view value shown to the user (v).
*/
class Choice t
where
	// Gets the current selection's view if present
	getSelectionView		:: !(t v)						-> Maybe v
    // Sets the selection based on the view value
    setSelectionView        :: !(Maybe v) !(t v)            -> t v | gEq {|*|} v
    // Gets the index of the selection
    getSelectionIndex       :: !(t v)                       -> Maybe Int
    // Sets the index of the selection
    setSelectionIndex       :: !(Maybe Int) !(t v)          -> t v

instance Choice ComboChoice,RadioChoice,ListChoice,TreeChoice,GridChoice,DynamicChoice

//* Represents the choice of a number of items from a list
:: CheckMultiChoice v o = CheckMultiChoice ![(!v,!o)] ![Int]

/**
* Interface for types representing choices a number of elements out of a set of options.
* There are different kinds of containers for such options (e.g. lists, trees, ...).
* Each option consists of an actual value (o) & a view value shown to the user (v).
*/
class MultiChoice choiceType
where
	// Selects the given options, selections not present in list of options are ignored
	selectOptions			:: ![o] !(choiceType v o)					-> choiceType v o | gEq{|*|} o
	// Gets the current selections
	getSelections			:: !(choiceType v o)						-> [o]
	// Gets the current selection's views
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

//* Setting layout directions
:: Row a                = Row a  //Horizontal
:: Col a                = Col a  //Vertical

//* Editing lists
:: EditableList a       =
    { items     :: [a]
    , add       :: !EditableListAdd a
    , remove    :: !Bool
    , reorder   :: !Bool
    , count     :: !Bool
    }
:: EditableListAdd a
    = ELNoAdd | ELAddBlank | ELAddValue ([a] -> a)

derive JSONEncode		Hidden, Display, Editable, VisualizationHint, Row, Col, EditableList, EditableListAdd
derive JSONDecode		Hidden, Display, Editable, VisualizationHint, Row, Col, EditableList, EditableListAdd
derive gDefault			Hidden, Display, Editable, VisualizationHint, Row, Col, EditableList, EditableListAdd
derive gEq				Hidden, Display, Editable, VisualizationHint, Row, Col, EditableList, EditableListAdd
derive gText	        Hidden, Display, Editable, VisualizationHint, Row, Col, EditableList, EditableListAdd
derive gEditor			Hidden, Display, Editable, VisualizationHint, Row, Col
derive gEditMeta		Hidden, Display, Editable, VisualizationHint, Row, Col
derive gUpdate			Hidden, Display, Editable, VisualizationHint, Row, Col
derive gVerify			Hidden, Display, Editable, VisualizationHint, Row, Col


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
:: InstanceKey  :== String

:: SessionId	:== String

instance toString	TaskId
instance fromString	TaskId
instance ==			TaskId
instance <			TaskId

// Instance data which does not change after creation (except when a task is replaced)
:: InstanceConstants =
    { instanceKey   :: !InstanceKey         //* Random string that a client needs to provide to access the task instance
	, listId        :: !TaskId              //* Reference to parent tasklist
    , session       :: !Bool                //* True for sessions (instances that automatically get garbage collected)
    , build         :: !String              //* Application build version when the instance was created
    , issuedAt		:: !DateTime			//* When was the task created
    }

:: InstanceProgress =
	{ value             :: !ValueStatus             //* Status of the task value
    , attachedTo        :: ![TaskId] 				//* Chain of tasks through which this instance was attached
	, firstEvent		:: !Maybe DateTime			//* When was the first work done on this task
	, lastEvent		    :: !Maybe DateTime			//* When was the latest event on this task	
    , connectedTo       :: !Maybe String            //* Is there an active client connection for this task
    , lastIO            :: !Maybe DateTime          //* When was the last input received or output sent to a client
	}

:: ValueStatus
    = None
    | Unstable
    | Stable
    | Exception

//* Access to parallel task lists

:: TaskList a :== (!TaskId,![TaskListItem a])
:: SharedTaskList a	:==	RWShared TaskListFilter (!TaskId,![TaskListItem a]) [(!TaskId,!TaskAttributes)]

:: TaskListItem a =
	{ taskId			:: !TaskId
    , listId            :: !TaskId
    , detached          :: !Bool
    , self              :: !Bool
	, value				:: !TaskValue a
	, attributes        :: !TaskAttributes
	, progress		    :: !Maybe InstanceProgress //Only possible for detached tasks
	}

:: TaskListFilter =
    //Which rows to filter
    { onlyIndex         :: !Maybe [Int]
    , onlyTaskId        :: !Maybe [TaskId]
    , onlySelf          :: !Bool
    //What to include
    , includeValue      :: !Bool
    , includeAttributes :: !Bool
    , includeProgress   :: !Bool
    }

:: TaskAttributes :== Map String String

:: ParallelTaskType	
	= Embedded                                    //Simplest embedded
    | NamedEmbedded !String                       //Embedded with name
	| Detached !TaskAttributes !Bool              //Management meta and flag whether the task should be started at once
    | NamedDetached !String !TaskAttributes !Bool //Detached with name

:: ParallelTask a	:== (SharedTaskList a) -> Task a

//* Types to view the server's internal table of running task instances
:: TaskInstance =
	{ instanceNo	    :: !InstanceNo			//* Unique global identification
    , instanceKey       :: !InstanceKey         //* Random string that a client needs to provide to access the task instance
    , session           :: !Bool                //* Is this a session
	, listId            :: !TaskId              //* Reference to parent tasklist
    , build             :: !String              //* Application build version when the instance was created
    , issuedAt			:: !DateTime			//* When was the task created
	, attributes        :: !TaskAttributes      //* Arbitrary meta-data
	, value             :: !ValueStatus         //* Status of the task value
	, firstEvent		:: !Maybe DateTime		//* When was the first work done on this task
	, lastEvent		    :: !Maybe DateTime		//* When was the last event on this task	
    , connectedTo       :: !Maybe String        //* Is there an active client connection for this task
    , lastIO            :: !Maybe DateTime      //* When was the last input received or output sent to a client
	}

derive class iTask TaskListFilter

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
ActionContinue	:==	Action "Continue"		[ActionIcon "continue"]
//ActionContinue	:==	Action "Continue"		[ActionIcon "continue", ActionKey (unmodified KEY_ENTER)]
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

derive JSONEncode		TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action, ActionOption, Hotkey, Trigger
derive JSONDecode		TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action, ActionOption, Hotkey, Trigger
derive gDefault			TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action, ActionOption, Hotkey, Trigger
derive gEq				TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action, ActionOption, Hotkey, Trigger

derive gText	        TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action, ActionOption, Hotkey, Trigger
derive gEditor			TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action, ActionOption, Hotkey, Trigger
derive gEditMeta		TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action, ActionOption, Hotkey, Trigger
derive gUpdate			TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action, ActionOption, Hotkey, Trigger
derive gVerify			TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action, ActionOption, Hotkey, Trigger

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
LAST_FOCUS_ATTRIBUTE    :== "lastfocus" //Last focus, also used for ordering

//Preferred container attribute for abstract containers. Does not have to be honoured by layouts
CONTAINER_ATTRIBUTE		:==	"container"	//Container preference for layout functions. Possible preferences: "container", "panel", or "window"

:: Att				= E.a: Att !a & descr a

:: Title			= Title !String
:: Label            = Label !String
:: Hint				= Hint !String

:: Icon				= Icon !String
					| IconView
					| IconEdit


//Make the UI definition of the interaction prompt
class descr d
where
	toPrompt		:: !d -> UIAttributes

instance descr ()                           //No prompt
instance descr Void							//No prompt
instance descr String						//Simple instruction
instance descr (!String, !String)			//Title attribute + instruction
instance descr (!Icon, !String, !String)	//Icon attribute, title attribute, and instruction
//instance descr (!Icon, !Title)			//Icon attribute, title attribute 
instance descr Title
instance descr Label
instance descr Hint
instance descr Icon
instance descr Attribute

instance descr Att
instance descr [d] | descr d

derive JSONEncode		Icon
derive JSONDecode		Icon
derive gDefault			Icon
derive gEq				Icon
derive gText	        Icon
derive gEditor          Icon	
derive gEditMeta		Icon
derive gUpdate			Icon
derive gVerify			Icon



//Task evaluation tuning directives, for increasing performance
:: LazyRefresh = LazyRefresh //If you tune a task in a parallel set with this directive, it not be evaluated unless its focused

//****************************************************************************//
// Generic instances for common library types
//****************************************************************************//

derive JSONEncode		(), HtmlTag, Void, Either, MaybeError, Timestamp
derive JSONDecode		(), HtmlTag, Void, Either, MaybeError, Timestamp
derive gEq				(), HtmlTag, Void, Either, MaybeError, Timestamp, JSONNode, (->), Dynamic

derive JSONEncode		RWShared
derive JSONDecode		RWShared
derive gEq				RWShared
derive gDefault   RWShared

derive JSONEncode SVGElt, SVGAttr, HtmlAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive JSONDecode SVGElt, SVGAttr, HtmlAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gEq        SVGElt, SVGAttr, HtmlAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan

derive gDefault   {}
derive gEditMeta  {}
derive gEditor    {}
derive gText      {}
derive gUpdate    {}
derive gVerify    {}
