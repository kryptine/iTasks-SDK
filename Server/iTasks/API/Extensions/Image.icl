implementation module iTasks.API.Extensions.Image
import iTasks
import iTasks.UI.Editor.Builtin, iTasks.UI.Editor.Combinators
import Text.HTML

gEditor{|WebImage|} = liftEditor (\(WebImage url) -> ImgTag [SrcAttr url]) (const (WebImage "")) htmlView

derive gText	        WebImage
derive JSONEncode		WebImage
derive JSONDecode		WebImage
derive gDefault			WebImage
derive gEq				WebImage
