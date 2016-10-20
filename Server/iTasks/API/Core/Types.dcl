definition module iTasks.API.Core.Types
/**
* This module provides types for all the globally shared concepts
* of the iTasks framework.
*/
from Data.Maybe import :: Maybe
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from Data.Either import :: Either
from System.FilePath import :: FilePath
from Text.HTML import :: HtmlTag, :: HtmlAttr
import Data.Functor
from Data.Error import :: MaybeError, :: MaybeErrorString
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
from iTasks.UI.Definition		import :: UI, :: UINodeType, :: UISize, :: UIDirection, :: UISideSizes, :: UIBound, :: UIAttributes
from iTasks.UI.Editor 			import :: Editor, :: EditMask, :: Masked
from iTasks.UI.Editor.Generic   import generic gEditor, :: VSt
from iTasks._Framework.Task				import :: Task, :: TaskId
from iTasks._Framework.Generic				import class iTask
from iTasks._Framework.Generic.Visualization	import generic gText, :: TextFormat(..), toMultiLineText
from iTasks._Framework.Generic.Defaults		import generic gDefault
from iTasks._Framework.SDS import :: ReadWriteShared, :: ReadOnlyShared, :: RWShared
from iTasks.UI.JS.Interface	import :: JSWorld, :: JSVal
from iTasks.UI.Prompt import class toPrompt

from Text.HTML import :: SVGElt, :: SVGAttr, :: SVGAlign, :: SVGColor, :: SVGDefer, :: SVGFillOpacity, :: SVGFuncIRI, :: SVGLengthAdjust
from Text.HTML import :: SVGLengthUnit, :: SVGLineCap, :: SVGFillRule, :: SVGLineJoin, :: SVGMeetOrSlice, :: SVGStrokeMiterLimit, :: SVGPaint
from Text.HTML import :: SVGStrokeDashArray, :: SVGStrokeDashOffset, :: SVGStrokeWidth, :: SVGTransform, :: SVGZoomAndPan

class TApplicative f | Functor f where
    (<#>)  :: (f (a -> b)) (f a) -> f b | iTask a & iTask b
    return :: a -> f a | iTask a

class TMonad m | TApplicative m where
    (>>=) infixl 1 :: (m a) (a -> m b) -> m b | iTask a & iTask b
    (>>|) infixl 1 :: (m a) (     m b) -> m b | iTask a & iTask b

instance Functor Task
instance TApplicative Task
instance TMonad Task

instance TApplicative Maybe
instance TMonad Maybe

instance TApplicative []
instance TMonad []

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

//* local date and time
:: Date	=
	{ year	:: !Int
	, mon	:: !Int
	, day	:: !Int
	}

:: Time =
	{ hour	:: !Int
	, min	:: !Int
	, sec	:: !Int
	}

:: DateTime =
	{ year	:: !Int
	, mon	:: !Int 
	, day	:: !Int
	, hour	:: !Int
	, min	:: !Int
	, sec	:: !Int
	}

//Conversion
toTime :: DateTime -> Time
toDate :: DateTime -> Date
toDateTime :: Date Time -> DateTime

//Printing and parsing

instance toString	Date, Time, DateTime

parseDate :: String -> MaybeErrorString Date         //Expected format: "yyyy-mm-dd"
parseTime :: String -> MaybeErrorString Time         //Expected format: "hh:mm:ss"
parseDateTime :: String -> MaybeErrorString DateTime //Expected format: "yyyy-mm-dd hh:mm:ss"

instance fromString	Date, Time, DateTime //Assumes parse* succeeds

//Comparison
instance ==	Date, Time, DateTime
instance <	Date, Time, DateTime

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

derive JSONEncode		EmailAddress, PhoneNumber, URL, EUR, USD, Date, Time, DateTime, Document 
derive JSONDecode		EmailAddress, PhoneNumber, URL, EUR, USD, Date, Time, DateTime, Document
derive gDefault			EmailAddress, PhoneNumber, URL, EUR, USD, Date, Time, DateTime, Document
derive gEq				EmailAddress, PhoneNumber, URL, EUR, USD, Date, Time, DateTime, Document

derive gText	        EmailAddress, PhoneNumber, URL, EUR, USD, Date, Time, DateTime, Document
derive gEditor 			EmailAddress, PhoneNumber, URL, EUR, USD, Date, Time, DateTime, Document

//* Common exceptions used by API tasks

:: FileException		= FileException !FilePath !FileError
:: ParseException		= CannotParse !String
:: CallException		= CallFailed !OSError
:: SharedException		= SharedException !String
:: RPCException			= RPCException !String
:: OSException			= OSException !OSError
:: AttachException		= InstanceNotFound | InstanceEvalError 

derive class iTask	FileException, ParseException, CallException, SharedException, RPCException, OSException, AttachException
instance toString	FileException, ParseException, CallException, SharedException, RPCException, OSException, AttachException

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

//* Table consisting of headers, the displayed data cells & possibly a selection
:: Table = Table ![String] ![[HtmlTag]] !(Maybe Int)

toTable	:: ![a] -> Table | gText{|*|} a

derive JSONEncode		Scale, Progress, ProgressAmount, HtmlInclude, Table
derive JSONDecode		Scale, Progress, ProgressAmount, HtmlInclude, Table
derive gDefault			Scale, Progress, ProgressAmount, HtmlInclude, Table
derive gEq				Scale, Progress, ProgressAmount, HtmlInclude, Table
derive gText	        Scale, Progress, ProgressAmount, HtmlInclude, Table
derive gEditor	        Scale, Progress, ProgressAmount, HtmlInclude, Table

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
	, lastEvent		    :: !Maybe DateTime			//* When was the latest event on this task (excluding Refresh events)
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
	}

derive class iTask TaskListFilter

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
:: Action	= Action !String //Locally unique identifier for actions

actionName		:: !Action -> String

//Common action constants with predefined options
ActionOk		:== Action "Ok"
ActionCancel	:==	Action "Cancel"
ActionYes		:== Action "Yes"
ActionNo		:== Action "No"
ActionNext		:== Action "Next"
ActionPrevious	:== Action "Previous"
ActionFinish	:== Action "Finish"
ActionContinue	:==	Action "Continue"
ActionOpen		:== Action "/File/Open"
ActionSave		:== Action "/File/Save"
ActionSaveAs 	:== Action "/File/Save as"
ActionQuit		:== Action "/File/Quit"
ActionHelp		:==	Action "/Help/Help"
ActionAbout		:== Action "/Help/About"
ActionFind		:== Action "/Edit/Find"
ActionNew		:== Action "New"
ActionEdit		:== Action "Edit"
ActionDelete	:== Action "Delete"
ActionRefresh	:== Action "Refresh"
ActionClose		:==	Action "Close"
	
derive JSONEncode		TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action
derive JSONDecode		TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action
derive gDefault			TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action
derive gEq				TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action

derive gText	        TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action
derive gEditor			TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action

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

SCREEN_ATTRIBUTE        :== "screen"
CREATED_AT_ATTRIBUTE	:== "createdate"//Creation task time, used for ordering but not real time
LAST_EVENT_ATTRIBUTE	:== "lastevent"	//Last event task time, used for ordering but not real time
LAST_FOCUS_ATTRIBUTE    :== "lastfocus" //Last focus, also used for ordering

//Preferred container attribute for abstract containers. Does not have to be honoured by layouts
CONTAINER_ATTRIBUTE		:==	"container"	//Container preference for layout functions. Possible preferences: "container", "panel", or "window"

:: Att				= E.a: Att !a & toPrompt a

:: Title			= Title !String
:: Label            = Label !String
:: Hint				= Hint !String

:: Icon				= Icon !String
					| IconView
					| IconEdit


instance toPrompt (!Icon, !String, !String)	//Icon attribute, title attribute, and instruction
//instance toPrompt (!Icon, !Title)			//Icon attribute, title attribute 
instance toPrompt Title
instance toPrompt Label
instance toPrompt Hint
instance toPrompt Icon
instance toPrompt Attribute

instance toPrompt Att
instance toPrompt [d] | toPrompt d

derive JSONEncode		Icon
derive JSONDecode		Icon
derive gDefault			Icon
derive gEq				Icon
derive gText	        Icon
derive gEditor          Icon	

//Task evaluation tuning directives, for increasing performance
:: LazyRefresh = LazyRefresh //If you tune a task in a parallel set with this directive, it not be evaluated unless its focused

//****************************************************************************//
// Generic instances for common library types
//****************************************************************************//

derive JSONEncode		(), HtmlTag, HtmlAttr, Either, MaybeError, Timestamp
derive JSONDecode		(), HtmlTag, HtmlAttr, Either, MaybeError, Timestamp
derive gEq				(), HtmlTag, HtmlAttr, Either, MaybeError, Timestamp, JSONNode, (->), Dynamic
derive gDefault   HtmlAttr
derive gEditor    HtmlAttr
derive gText      HtmlAttr

derive JSONEncode		RWShared
derive JSONDecode		RWShared
derive gEq				RWShared
derive gDefault   RWShared

derive JSONEncode SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive JSONDecode SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gEq        SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gDefault   SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gEditor    SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gText      SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan

derive gDefault   {}
derive gEditor    {}
derive gText      {}
