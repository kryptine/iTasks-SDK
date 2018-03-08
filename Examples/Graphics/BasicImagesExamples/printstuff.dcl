definition module printstuff

import StdInt, StdOverloaded, StdReal, StdString
import Graphics.Scalable.Internal.Image`

instance toString Span
instance toString ImageTag
instance toString (a,b) | toString a & toString b

indentImg :: !String !Img -> String

(<+) infixl 1 :: !String !a -> String | toString a

showl :: ![a] -> String | toString a
