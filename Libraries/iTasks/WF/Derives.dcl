definition module iTasks.WF.Derives
/**
* This module provides derived instances for common types from StdEnv and Platform
* such that you don't have to derive them when you use these libraries.
*/
import iTasks.WF.Definition

from iTasks.WF.Combinators.Core import :: Action, :: TaskListItem, :: TaskListFilter, :: AttachmentStatus
from iTasks.Internal.IWorld import :: ClockParameter
from iTasks.SDS.Sources.System import :: TaskInstance, :: ValueStatus

import Data.Either.GenJSON, Data.Error.GenJSON, Data.Map.GenJSON
import Text.HTML.GenJSON
import System.Time.GenJSON

from Text.HTML import :: HtmlAttr
from Text.HTML import :: SVGElt, :: SVGAttr, :: SVGAlign, :: SVGColor, :: SVGDefer, :: SVGFillOpacity, :: SVGFuncIRI, :: SVGLengthAdjust, :: SVGLength, :: SVGICCColor, :: SVGNumber
from Text.HTML import :: SVGLengthUnit, :: SVGLineCap, :: SVGFillRule, :: SVGLineJoin, :: SVGMeetOrSlice, :: SVGStrokeMiterLimit, :: SVGPaint
from Text.HTML import :: SVGStrokeDashArray, :: SVGStrokeDashOffset, :: SVGStrokeWidth, :: SVGTransform, :: SVGZoomAndPan

//Common library types
derive gEq	    (->), Dynamic
derive gEditor  HtmlAttr
derive gText    HtmlAttr

derive gEditor    SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan, SVGLength, SVGICCColor
derive gText      SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan, SVGLength, SVGICCColor

//Common iTasks system types
derive class iTask TaskId, TaskListFilter, AttachmentStatus

derive JSONEncode		TaskValue, TaskListItem, TaskInstance, ValueStatus, Action, Timespec, ClockParameter
derive JSONDecode		TaskValue, TaskListItem, TaskInstance, ValueStatus, Action, Timespec, ClockParameter
derive gEq				TaskValue, TaskListItem, TaskInstance, ValueStatus, Action, Timespec, ClockParameter

derive gText	        TaskValue, TaskListItem, TaskInstance, ValueStatus, Action
derive gEditor			TaskValue, TaskListItem, TaskInstance, ValueStatus, Action, Timespec, ClockParameter

