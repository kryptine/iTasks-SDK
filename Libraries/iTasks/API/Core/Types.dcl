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

//* Common exceptions used by API tasks
:: FileException		= FileException !FilePath !FileError
:: ParseException		= CannotParse !String
:: CallException		= CallFailed !OSError
:: RPCException			= RPCException !String
:: OSException			= OSException !OSError

derive class iTask	FileException, ParseException, CallException, RPCException, OSException
instance toString	FileException, ParseException, CallException, RPCException, OSException

//****************************************************************************//
// Framework types.
// These types define the user-facing representations of the iTasks framework
// It is generally not necessary to create values of these types yourself, but
// you may read them when interacting with the framework
//****************************************************************************//

//* Access to parallel task lists

derive class iTask TaskListFilter

derive JSONEncode		TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action
derive JSONDecode		TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action
derive gDefault			TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action
derive gEq				TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action

derive gText	        TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action
derive gEditor			TaskValue, TaskListItem, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, Action

derive class iTask		TaskId

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
