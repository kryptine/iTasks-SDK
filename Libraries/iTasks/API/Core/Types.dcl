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
from iTasks.WF.Definition		import :: Task, :: TaskId, :: InstanceNo, :: TaskValue, :: Stability, :: TaskAttributes, class iTask
from iTasks.WF.Combinators.Core import :: Action(..), :: TaskListItem(..), :: TaskListFilter(..), :: InstanceKey
from iTasks.WF.Combinators.Core import :: InstanceConstants(..), :: InstanceProgress(..), :: ValueStatus(..)

from iTasks.SDS.Definition import :: SDS, :: ReadWriteShared, :: ReadOnlyShared
from iTasks.SDS.Sources.System import :: TaskInstance

from iTasks.UI.JS.Interface	import :: JSWorld, :: JSVal
from iTasks.UI.Prompt import class toPrompt

from Text.HTML import :: SVGElt, :: SVGAttr, :: SVGAlign, :: SVGColor, :: SVGDefer, :: SVGFillOpacity, :: SVGFuncIRI, :: SVGLengthAdjust
from Text.HTML import :: SVGLengthUnit, :: SVGLineCap, :: SVGFillRule, :: SVGLineJoin, :: SVGMeetOrSlice, :: SVGStrokeMiterLimit, :: SVGPaint
from Text.HTML import :: SVGStrokeDashArray, :: SVGStrokeDashOffset, :: SVGStrokeWidth, :: SVGTransform, :: SVGZoomAndPan

from iTasks.UI.Editor import :: Editor
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks._Framework.Generic.Visualization import generic gText, :: TextFormat
from iTasks._Framework.Generic.Defaults import generic gDefault
from Text.JSON import generic JSONEncode, generic JSONDecode
from GenEq import generic gEq

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

derive JSONEncode		Date, Time, DateTime, Document 
derive JSONDecode		Date, Time, DateTime, Document
derive gDefault			Date, Time, DateTime, Document
derive gEq				Date, Time, DateTime, Document
derive gText	        Date, Time, DateTime, Document
derive gEditor 			Date, Time, DateTime, Document

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
// Framework types.
// These types define the user-facing representations of the iTasks framework
// It is generally not necessary to create values of these types yourself, but
// you may read them when interacting with the framework
//****************************************************************************//

instance Functor TaskValue

:: SessionId	:== String

class toInstanceNo t :: t -> InstanceNo
instance toInstanceNo InstanceNo
instance toInstanceNo TaskId

instance toString	TaskId
instance fromString	TaskId
instance ==			TaskId
instance <			TaskId

//* Access to parallel task lists

derive class iTask TaskListFilter

//* Framework configuration
:: Config =
	{ sessionTime		:: !Int		//* Time (in seconds) before inactive sessions are garbage collected. Default is 3600 (one hour).
	, smtpServer		:: !String	//* The smtp server to use for sending e-mails
	}

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

derive class iTask		TaskId, Config

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
				| CreatedAtAttribute !Int
				| LastEventAttribute !Int
				| FloatAttribute
				
//Define initial meta attributes
TASK_ATTRIBUTE			:== "task"

SCREEN_ATTRIBUTE        :== "screen"
CREATED_AT_ATTRIBUTE	:== "createdate"//Creation task time, used for ordering but not real time
LAST_EVENT_ATTRIBUTE	:== "lastevent"	//Last event task time, used for ordering but not real time
LAST_FOCUS_ATTRIBUTE    :== "lastfocus" //Last focus, also used for ordering

//****************************************************************************//
// Generic instances for common library types
//****************************************************************************//

derive JSONEncode		(), HtmlTag, HtmlAttr, Either, MaybeError, Timestamp
derive JSONDecode		(), HtmlTag, HtmlAttr, Either, MaybeError, Timestamp
derive gEq				(), HtmlTag, HtmlAttr, Either, MaybeError, Timestamp, JSONNode, (->), Dynamic
derive gDefault   HtmlAttr
derive gEditor    HtmlAttr
derive gText      HtmlAttr

derive JSONEncode		SDS
derive JSONDecode		SDS
derive gEq				SDS
derive gDefault   SDS

derive JSONEncode SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive JSONDecode SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gEq        SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gDefault   SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gEditor    SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gText      SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan

derive gDefault   {}
derive gEditor    {}
derive gText      {}
