module Character

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.Common
import iTasks.UI.Prompt
import Graphics.Scalable.Image
import iTasks.Extensions.SVG.SVGEditor
from   StdFunc import id

Start :: *World -> *World
Start world
	= startEngine [publish "/" (const (viewInformation "A char" [ViewUsing id (fromSVGEditor
	                                                                            { initView    = id
	                                                                            , renderImage = const char
	                                                                            , updView     = \m _ = m
	                                                                            , updModel    = \_ v = v
	                                                                            })] 'F'))] world

char :: Char *TagSource -> Image Char
char c tags
	= margin (px 20.0) (
         text (normalFontDef "Times New Roman" 72.0) (toString c)
	  )
