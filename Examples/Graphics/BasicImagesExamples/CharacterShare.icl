module CharacterShare

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.Common
import iTasks.WF.Combinators.SDS
import iTasks.UI.Definition, iTasks.UI.Tune
import iTasks.Extensions.SVG.SVGEditor
import StdFunctions
from   iTasks import instance Identifiable SDSLens, instance Modifiable SDSLens, instance Registrable SDSLens, instance Readable SDSLens, instance Writeable SDSLens

Start :: *World -> *World
Start world
	= startEngine [publish "/" (const 
	                  (withShared 'F' (\share ->
	                       Title "A char" @>> viewSharedInformation  [ViewUsing id (fromSVGEditor
	                                                                       { initView    = id
	                                                                       , renderImage = const char
	                                                                       , updModel    = \_ v = v
	                                                                       })] share
	                       -||-
	                       (Title "This char" @>> updateSharedInformation  [] share)
	                   )))] world

char :: Char *TagSource -> Image Char
char c tags
	= margin (px 100) (
         text (normalFontDef "Times New Roman" 72) (toString c)
	  )
