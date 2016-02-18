definition module iTasks.API.Extensions.SVG.SVGlet

import Graphics.Scalable
import Graphics.Scalable.Internal
from Text.JSON import generic JSONEncode, generic JSONDecode
from GenEq import generic gEq
import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Types

imageUpdate :: !(s -> v) !(s v *TagSource -> Image v) !(s v -> s) !(s v -> v)
               !(Conflict s -> Maybe s)
               !(s s -> s`)
            -> UpdateOption s s` | iTask s & iTask v

:: SVGClSt s v
:: SVGDiff s

:: ActionState a s = { state   :: s
                     , action  :: Maybe a
                     }

derive class iTask ActionState, SVGClSt, SVGDiff

doAction :: !(a (ActionState a s) -> b) !(TaskValue (ActionState a s)) -> Maybe b

ifAction :: !(a -> Bool) !(a s -> s) !(a (ActionState a s) -> b) !(TaskValue (ActionState a s)) -> Maybe b

derive class iTask Image, Span, LookupSpan, FontDef, ImageTransform, ImageAttr
derive class iTask ImageContent, BasicImage, CompositeImage, LineImage, Markers
derive class iTask LineContent, Compose, XAlign, YAlign, OnMouseOutAttr, OnMouseMoveAttr
derive class iTask OpacityAttr, FillAttr, XRadiusAttr, YRadiusAttr, StrokeWidthAttr, StrokeAttr
derive class iTask Slash, DraggableAttr, OnMouseOverAttr, OnMouseUpAttr, DashAttr
derive class iTask OnMouseDownAttr, OnClickAttr, Angle
