definition module iTasks.Extensions.SVG.SVGEditor

import Graphics.Scalable.Image
from   iTasks.UI.Editor import :: Editor
import iTasks.UI.JS.Encoding

// An SVGEditor let's you specify an editor as an interactive SVG image (Graphics.Scalable.Image)
:: SVGEditor m v =
	{ initView    :: m -> v                      // Generate the view value from the current model value
	, renderImage :: m v *TagSource -> Image v   // Render an interactive image from the current model and view value
	, updModel    :: m v -> m                    // When the view is updated (using the interactive image), update the model
	}

fromSVGEditor :: !(SVGEditor s v) -> Editor s | iTask, JSEncode{|*|}, JSDecode{|*|} s
