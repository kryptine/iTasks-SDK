module CharacterShare

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.Common
import iTasks.WF.Combinators.SDS
import iTasks.UI.Prompt
import iTasks.Extensions.SVG.SVGEditor
from   StdFunc import id, const
from   iTasks import instance Identifiable SDSLens, instance Modifiable SDSLens, instance Registrable SDSLens, instance Readable SDSLens, instance Writeable SDSLens

Start :: *World -> *World
Start world
	= startEngine [publish "/" (const 
	                  (withShared 'F' (\share ->
	                       viewSharedInformation "A char" [ViewUsing id (fromSVGEditor
	                                                                       { initView    = id
	                                                                       , renderImage = const char
	                                                                       , updModel    = \_ v = v
	                                                                       })] share
	                       -||-
	                       updateSharedInformation "This char" [] share
	                   )))] world

char :: Char *TagSource -> Image Char
char c tags
	= margin (px 100.0) (
         text (normalFontDef "Times New Roman" 72.0) (toString c)
	  )
