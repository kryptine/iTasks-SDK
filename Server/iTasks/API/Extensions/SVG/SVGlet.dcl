definition module iTasks.API.Extensions.SVG.SVGlet

import Graphics.Scalable
import Graphics.Scalable.Internal
from Text.JSON import generic JSONEncode, generic JSONDecode
from GenEq import generic gEq
import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Types
import iTasks.API.Extensions.Platform

imageUpdate :: !(s -> v)                    // Initialize the client state, given a server state
               !(s v *TagSource -> Image v) // Given both a server and client state, generate an image
               !(s v -> s)                  // Update the server state, given the current server state and the current client state
               !(s v -> v)                  // Update the client state, given the current server state and the current client state
               !(Conflict s -> Maybe s)     // Resolve a conflict (TODO: Do we need include v as well?
               !(s s -> s`)                 // Given an old editor value and the latest (?), produce a result value
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
derive class iTask OnMouseDownAttr, OnClickAttr
