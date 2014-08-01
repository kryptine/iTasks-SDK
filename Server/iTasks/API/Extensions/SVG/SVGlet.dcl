definition module iTasks.API.Extensions.SVG.SVGlet

import Graphics.Scalable
from Text.JSON import generic JSONEncode, generic JSONDecode
from GenEq import generic gEq
import iTasks
import iTasks.API.Core.Client.Editlet


derive JSONEncode Image
derive JSONDecode Image
derive gEq        Image
derive gEditMeta  Image, SVGColor
derive gVerify    Image, SVGColor
derive gDefault   Image, SVGColor
derive gText      Image, SVGColor
derive gEditor    Image, SVGColor
derive gUpdate    Image, SVGColor

derive class iTask ImageTag, ImageTransform, Span, LookupSpan, ImageAttr,
  ImageContent, BasicImage, ImageSpan, CompositeImage, Slash, FontDef, Compose,
  OpacityAttr, FillAttr, StrokeWidthAttr, StrokeAttr, OnClickAttr, XAlign,
  YAlign, SVGletDiff

:: SVGletDiff m
  =  Redraw (Image m)

svglet :: (Image m) -> Editlet (Image m) [SVGletDiff m] | iTask m

fixSpans :: (Image m) -> SrvSt (Image m, ImageSpan) | iTask m
:: ServerState =
  { srvTaggedSpanEnv   :: Map (Set ImageTag) CachedSpan
  }
:: CachedSpan
  = CachedGridSpan [[ImageSpan]]
  | CachedImageSpan ImageSpan

:: SrvSt a :== State ServerState a // ServerState -> (a, ServerState)

:: State s a :== s -> (a, s)
