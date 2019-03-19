module Rotates

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.Common
import iTasks.UI.Prompt
import iTasks.Extensions.SVG.SVGEditor
import StdEnum, StdReal
from   StdFunc import id

Start :: *World -> *World
Start world
	= doTasks (viewInformation "Rotates"
		[ViewUsing id (fromSVGEditor
	    	{ initView    = id
	        , renderImage = const rotates
	        , updModel    = \_ v = v
	        })] 0) world

/** rotates model tags = image:
	@image displays a sequence of rotated rectangles
*/
rotates :: m *TagSource -> Image m
rotates _ _
	= beside [] [] Nothing []
	     [  rotate (deg angle)
	            (rect (px 30.0) (px 60.0) <@< {fill = toSVGColor {r=150,g=toInt (255.0 * (angle / 360.0)),b=toInt (255.0 * (360.0-angle / 360.0))}}
	                                      <@< {stroke = toSVGColor "none"}
	            )
	     \\ angle <- [0.0, 30.0 .. 360.0]
	     ] NoHost
