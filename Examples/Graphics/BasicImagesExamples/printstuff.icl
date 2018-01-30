implementation module printstuff

import StdInt, StdList, StdOverloaded, StdReal, StdString
import Math.Geometry
import Data.Maybe
from Data.Set  import toList, toAscList, fold
from Text.HTML import :: SVGColor (..)
import Graphics.Scalable.Internal.Image`

instance toString Span
   where toString (PxSpan r) = br ("px " <+ r)
         toString (LookupSpan l) = case l of
                                     ColumnXSpan t n = br ("columnspan " <+ t <+ " " <+ n)
                                     RowYSpan    t n = br ("rowspan "    <+ t <+ " " <+ n)
                                     ImageXSpan  t   = br ("imagexspan " <+ t)
                                     ImageYSpan  t   = br ("imageyspan " <+ t)
                                     TextXSpan   f t = br ("textxspan "  <+ f.FontDef.fontfamily <+ " " <+ f.FontDef.fontysize)
                                     PathXSpan   t   = br ("pathxspan "  <+ t)
                                     PathYSpan   t   = br ("pathyspan "  <+ t)
         toString (AddSpan a b) = br ("" <+ a <+ " + " <+ b)
         toString (SubSpan a b) = br ("" <+ a <+ " - " <+ b)
         toString (MulSpan a b) = br ("" <+ a <+ " * " <+ b)
         toString (DivSpan a b) = br ("" <+ a <+ " / " <+ b)
         toString (AbsSpan a)   = br ("abs " <+ a)
         toString (MinSpan as)  = br ("minSpan " +++ showl as)
         toString (MaxSpan as)  = br ("maxSpan " +++ showl as)
instance toString ImageTag
   where toString (ImageTagUser no _) = "u" <+ no
         toString (ImageTagSystem no) = "s" <+ no
instance toString (a,b) | toString a & toString b
   where toString (a,b) = "(" <+ a <+ "," <+ b <+ ")"
instance toString ImgTransform
   where toString (RotateImg a)     = br ("rotate " <+ a)
         toString (SkewXImg  a)     = br ("skewx "  <+ a)
         toString (SkewYImg  a)     = br ("skewy "  <+ a)
         toString (FitImg    w h)   = br ("fit "  <+ br (toString w) <+ " " <+ br (toString h))
         toString (FitXImg   w)     = br ("fitx " <+ br (toString w))
         toString (FitYImg   h)     = br ("fity " <+ br (toString h))
         toString (ScaleImg  rx ry) = br ("scale " <+ rx <+ " " <+ ry)
         toString  FlipXImg         = "flipx"
         toString  FlipYImg         = "flipy"
         toString (MaskImg   no)    = br ("mask " <+ no)
instance toString BasicImg
   where toString EmptyImg           = "empty"
         toString (TextImg font str) = "text " <+ font.FontDef.fontfamily <+ " " <+ font.FontDef.fontysize
         toString CircleImg          = "circle"
         toString RectImg            = "rect"
         toString EllipseImg         = "ellipse"
         toString PolylineImg        = "polyline"
         toString PolygonImg         = "polygon"
instance toString BasicImgAttr
   where toString (BasicImgDashAttr          ds) = "{dash = " <+ showl ds <+ "}"
         toString (BasicImgFillAttr          c)  = "{fill = " <+ c <+ "}"
         toString (BasicImgFillOpacityAttr   r)  = "{opacity = " <+ r <+ "}"
         toString (BasicImgStrokeAttr        c)  = "{stroke = " <+ c <+ "}"
         toString (BasicImgStrokeOpacityAttr r)  = "{strokeopacity = " <+ r <+ "}"
         toString (BasicImgStrokeWidthAttr   s)  = "{strokewidth = " <+ s <+ "}"
         toString (BasicImgXRadiusAttr       s)  = "{xradius = " <+ s <+ "}"
         toString (BasicImgYRadiusAttr       s)  = "{yradius = " <+ s <+ "}"
instance toString Angle
   where toString (Deg r) = br (toString r +++ " degree")
         toString (Rad r) = br (toString r +++ " rad")
instance toString SVGColor
   where toString (SVGRGB r g b) = showl [r,g,b]
         toString (SVGColorText name) = name
br str = "(" +++ str +++ ")"

(<+) infixl 1 :: !String !a -> String | toString a
(<+) str a = str +++ toString a

showl :: ![a] -> String | toString a
showl []     = "[]"
showl [x]    = "[" <+ x <+ "]"
showl [x:xs] = foldl (\str x -> str <+ "," <+ x) ("[" <+ x) xs <+ "]"

indentl :: !String !(String a -> String) ![a] -> String
indentl prefix print []     = prefix <+ "[]"
//indentl prefix print [x]    = prefix <+ "[" <+ print prefix x <+ "]"
indentl prefix print [x:xs] = foldl (\str x -> str <+ "\n" <+ prefix <+ "," <+ print prefix x) (prefix <+ "[" <+ print prefix x) xs <+ "\n" <+ prefix <+ "]"

indentImg :: !String !Img -> String
indentImg prefix {Img | uniqId,host,transform,overlays,offsets}
	= prefix <+ "{uniqId = " <+ uniqId <+ "\n" <+
	  if (isEmpty overlays)
	     ""
	     (prefix <+ ",overlays = " <+ "\n" <+ indentl (prefix +++ tab) indentImg overlays <+ "\n") <+
	  prefix <+ ",host = " <+ indentHost (prefix +++ tab) host <+ "\n" <+
	  if (isNothing transform)
	     ""
	     (prefix <+ ",transform = " <+ toString (fromJust transform) <+ "\n") <+
	  if (isEmpty offsets)
	     ""
	     (prefix <+ ",offsets = " <+ showl offsets <+ "\n") <+
	  prefix <+ "}"
	  
indentHost :: !String !HostImg -> String
indentHost prefix (BasicHostImg img attrs)
	= toString img <+ if (isEmpty (toList attrs)) "" (" <@< " <+ showl (toList attrs))
indentHost prefix (RawHostImg str)
	= str
indentHost prefix (CompositeImg img)
	= "Composite" <+ "\n" <+ indentImg prefix img

tab :== "  "
