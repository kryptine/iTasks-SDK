implementation module iTasks.WF.Derives

import iTasks.WF.Definition
import iTasks.WF.Combinators.Core
import iTasks.UI.Editor.Common
import iTasks.SDS.Sources.System
import iTasks.Internal.IWorld

import Data.Either
import Data.Error
import Data.Map.GenJSON
import System.Time
import Text.HTML
import Text.GenJSON
import StdArray 

gEq{|(->)|} _ _ fa fb		= False // HACK: Compare string representations of graphs functions are never equal
gEq{|Dynamic|} _ _			= False	// dynamics are never equal

derive gEditor    SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan, SVGLength, SVGICCColor
derive gText      SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan, SVGLength, SVGICCColor
derive gEditor    HtmlAttr
derive gText      HtmlAttr

derive JSONEncode		TaskValue, TaskInstance, TaskListItem, ValueStatus, Action, Timespec, ClockParameter
derive JSONDecode		TaskValue, TaskInstance, TaskListItem, ValueStatus, Action, Timespec, ClockParameter
derive gEq				TaskValue, TaskInstance, TaskListItem, ValueStatus, Action, Timespec, ClockParameter
derive gText	        TaskValue, TaskInstance, TaskListItem, ValueStatus, Action
derive gEditor			TaskValue, TaskInstance, TaskListItem, ValueStatus, Action, Timespec, ClockParameter

derive class iTask TaskId, TaskListFilter, AttachmentStatus
