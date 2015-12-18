definition module iTasks.API.Extensions.SVG.SVGlet

import Graphics.Scalable
import Graphics.Scalable.Internal
from Text.JSON import generic JSONEncode, generic JSONDecode
from GenEq import generic gEq
import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Types

imageView   ::           !(s *TagSource -> Image s) !(Conflict s -> Maybe s)              -> ViewOption s | iTask s

imageUpdate :: !(s -> v) !(v *TagSource -> Image v) !(Conflict v -> Maybe v) !(s v -> s`) -> UpdateOption s s` |  iTask v

:: SVGSrvSt s =
  { svgSrvSt :: !s
  }
:: SVGClSt s
:: SVGDiff s

svgRenderer :: !(Conflict s -> Maybe s) !s !(s *TagSource -> Image s)
            -> Editlet (SVGSrvSt s) (SVGDiff s) (SVGClSt s) | iTask s

:: ActionState a s = { state   :: s
                     , action  :: Maybe a
                     }

derive class iTask ActionState, SVGClSt, SVGSrvSt, SVGDiff

doAction :: !(a (ActionState a s) -> b) !(TaskValue (ActionState a s)) -> Maybe b

ifAction :: !(a -> Bool) !(a s -> s) !(a (ActionState a s) -> b) !(TaskValue (ActionState a s)) -> Maybe b

derive class iTask Image, Span, LookupSpan, FontDef, ImageTransform, ImageAttr
derive class iTask ImageContent, BasicImage, CompositeImage, LineImage, Markers
derive class iTask LineContent, Compose, XAlign, YAlign, OnMouseOutAttr, OnMouseMoveAttr
derive class iTask OpacityAttr, FillAttr, XRadiusAttr, YRadiusAttr, StrokeWidthAttr, StrokeAttr
derive class iTask Slash, DraggableAttr, OnMouseOverAttr, OnMouseUpAttr, DashAttr
derive class iTask OnMouseDownAttr, OnClickAttr, Angle
