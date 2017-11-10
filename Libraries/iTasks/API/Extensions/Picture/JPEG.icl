implementation module iTasks.API.Extensions.Picture.JPEG

import StdString

from iTasks._Framework.IWorld import :: IWorld 
from Data.Maybe import :: Maybe(..)
from iTasks.API.Core.Types import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor, :: TaskAttributes, :: DateTime, instance toString DateTime
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks._Framework.Generic.Visualization import :: TextFormat(..)
from Data.Maybe import :: Maybe
from Data.Map import :: Map, newMap

from iTasks.UI.Editor.Combinators import liftEditor
from iTasks.UI.Editor.Builtin import htmlView
from iTasks.UI.Definition import :: UIAttributes
from Text.HTML import :: HtmlTag(ImgTag), :: HtmlAttr(SrcAttr,StyleAttr,AltAttr)

from StdFunc import const

derive gText JPEGPicture
derive JSONEncode JPEGPicture
derive JSONDecode JPEGPicture
derive gDefault JPEGPicture
derive gEq JPEGPicture

gEditor{|JPEGPicture|} = liftEditor toView (const (JPEGPicture "")) (htmlView newMap) // (htmlView newMap)
where
	toView :: JPEGPicture -> HtmlTag
	toView (JPEGPicture val) = ImgTag 
		[ SrcAttr ("data:image/jpg;base64," +++ val)
		, StyleAttr ("max-width: 200px; max-height: 200px;")
		, AltAttr "no photo"
		] 
