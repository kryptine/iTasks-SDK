module Character

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.UI.Prompt
import iTasks.Extensions.SVG.SVGEditor
import StdFunctions

Start :: *World -> *World
Start world
	= startEngine [publish "/" (const (viewInformation "A char" [ViewUsing id (fromSVGEditor
	                                                                            { initView    = id
	                                                                            , renderImage = const char
	                                                                            , updModel    = \_ v = v
	                                                                            })] 'F'))] world

char :: Char *TagSource -> Image Char
char c tags
	= margin (px 20) (
         text (normalFontDef "Times New Roman" 72) (toString c)
	  )
