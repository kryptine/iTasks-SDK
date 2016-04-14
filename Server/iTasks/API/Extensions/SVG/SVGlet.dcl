definition module iTasks.API.Extensions.SVG.SVGlet

import Graphics.Scalable
import Graphics.Scalable.Internal
from Text.JSON import generic JSONEncode, generic JSONDecode
from GenEq import generic gEq
import iTasks
import iTasks.UI.Editor
import iTasks.API.Core.Types

//An SVGLet let's you specify an editor as an interactive SVG image
:: SVGLet s =
	{ toImage :: s *TagSource -> Image s
	, resolve ::   Conflict s -> Maybe s
	}

fromSVGLet :: (SVGLet s) -> Editor s | gEq{|*|} s & gDefault{|*|} s & JSONEncode{|*|} s & JSONDecode{|*|} s

//SHOULD BE DEPRECATED...

imageUpdate :: !(s -> v) !(v *TagSource -> Image v) !(Conflict v -> Maybe v)
               !(s v -> s`)
            -> UpdateOption s s` | iTask v

:: ActionState a s = { state   :: s
                     , action  :: Maybe a
                     }

derive class iTask ActionState

doAction :: !(a (ActionState a s) -> b) !(TaskValue (ActionState a s)) -> Maybe b

ifAction :: !(a -> Bool) !(a s -> s) !(a (ActionState a s) -> b) !(TaskValue (ActionState a s)) -> Maybe b

derive class iTask Image, Span, LookupSpan, FontDef, ImageTransform, ImageAttr
derive class iTask ImageContent, BasicImage, CompositeImage, LineImage, Markers
derive class iTask LineContent, Compose, XAlign, YAlign, OnMouseOutAttr, OnMouseMoveAttr
derive class iTask OpacityAttr, FillAttr, XRadiusAttr, YRadiusAttr, StrokeWidthAttr, StrokeAttr
derive class iTask Slash, DraggableAttr, OnMouseOverAttr, OnMouseUpAttr, DashAttr
derive class iTask OnMouseDownAttr, OnClickAttr, Angle
