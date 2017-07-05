implementation module iTasks.WF.Derives

import iTasks.WF.Definition
import iTasks.WF.Combinators.Core
import iTasks.UI.Editor.Common
import iTasks.SDS.Definition
import iTasks.SDS.Sources.System

import Data.Either
import Data.Error
import System.Time
import Text.HTML
import Text.JSON
import StdArray 

// Generic instances for common library types
derive JSONEncode		Either, MaybeError, HtmlTag, HtmlAttr
derive JSONDecode		Either, MaybeError, HtmlTag, HtmlAttr
derive gEq				Either, MaybeError, HtmlTag, HtmlAttr, Timestamp, JSONNode

gEq{|()|} _ _ = True
JSONEncode{|()|} _ () = [JSONNull]
JSONDecode{|()|} _ [JSONNull:c]		= (Just (), c)
JSONDecode{|()|} _ [JSONObject []:c]= (Just (), c)
JSONDecode{|()|} _ c				= (Nothing, c)

JSONEncode{|Timestamp|} _ (Timestamp t)	= [JSONInt t]
JSONDecode{|Timestamp|} _ [JSONInt t:c]	= (Just (Timestamp t), c)
JSONDecode{|Timestamp|} _ c				= (Nothing, c)

gEq{|(->)|} _ _ fa fb		= False // HACK: Compare string representations of graphs functions are never equal
gEq{|Dynamic|} _ _			= False	// dynamics are never equal

gDefault{|{}|} fx = createArray 0 fx
gEditor{|{}|} _ _ _ _ _ = emptyEditor
gText{|{}|} _ _ _ = []

derive JSONEncode SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive JSONDecode SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gEq        SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gDefault   SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gEditor    SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gText      SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gDefault   HtmlAttr
derive gEditor    HtmlAttr
derive gText      HtmlAttr

JSONEncode{|SDS|} _ _ _ _ s = []
JSONDecode{|SDS|} _ _ _ _ s = (Nothing, s)
gEq{|SDS|} _ _ _ _ _ = False
gDefault{|SDS|} _ _ _ = SDSSource { SDSSource
                                  | name  = "gDefault RWShared"
                                  , read  = \_ w -> (Error (dynamic "", "No gDefault RWShared implementation"), w)
                                  , write = \_ _ w -> (Error (dynamic "", "No gDefault RWShared implementation"), w)}

derive JSONEncode		TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action
derive JSONDecode		TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action
derive gDefault			TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action
derive gEq				TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action
derive gText	        TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action
derive gEditor			TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action

derive class iTask TaskId, TaskListFilter, AttachmentStatus
