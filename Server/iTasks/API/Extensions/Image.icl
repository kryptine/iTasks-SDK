implementation module iTasks.API.Extensions.Image
import iTasks
import iTasks.UI.Editor.Builtin, iTasks.UI.Editor.Combinators
import Text.HTML

gEditor{|WebImage|} = liftEditor (\{WebImage|src,alt,width,height} -> ImgTag [SrcAttr src,AltAttr alt,WidthAttr (toString width), HeightAttr (toString height)])
                                 (const defaultValue) htmlView

derive gText	        WebImage
derive JSONEncode		WebImage
derive JSONDecode		WebImage
derive gDefault			WebImage
derive gEq				WebImage
