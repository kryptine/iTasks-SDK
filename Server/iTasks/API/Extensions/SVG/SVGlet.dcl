definition module iTasks.API.Extensions.SVG.SVGlet

import Graphics.Scalable
import Graphics.Scalable.Internal
import iTasks
import iTasks.UI.Editor
import iTasks.API.Core.Types

//An SVGLet let's you specify an editor as an interactive SVG image
:: SVGLet m v =
	{ initView    :: m -> v                     //Initialize a 'view' value that holds temporary data while editing
    , renderImage :: m v *TagSource -> Image v  //Render an interactive image that 
	, updView     :: m v -> v                   //When the model is externally updated, the view needs to be updated too
	, updModel    :: m v -> m                   //When the view is updated (using the image), the change needs to be merged back into the view
	}

fromSVGLet :: (SVGLet s v) -> Editor s | iTask s

derive class iTask Image, Span, LookupSpan, FontDef, ImageTransform, ImageAttr
derive class iTask ImageContent, BasicImage, CompositeImage, LineImage, Markers
derive class iTask LineContent, Compose, XAlign, YAlign, OnMouseOutAttr, OnMouseMoveAttr
derive class iTask OpacityAttr, FillAttr, XRadiusAttr, YRadiusAttr, StrokeWidthAttr, StrokeAttr
derive class iTask Slash, DraggableAttr, OnMouseOverAttr, OnMouseUpAttr, DashAttr
derive class iTask OnMouseDownAttr, OnClickAttr, Angle
