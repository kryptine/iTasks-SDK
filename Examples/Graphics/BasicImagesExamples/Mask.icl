module Mask

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.UI.Prompt
import Graphics.Scalable.Image
import iTasks.Extensions.SVG.SVGEditor
import StdReal
from   StdFunc import const, id

Start :: *World -> *World
Start world
	= doTasks (viewInformation "Mask"
		[ViewUsing id (fromSVGEditor
			{ initView    = id
			, renderImage = const image
			, updModel    = \_ v = v
			})] 0) world

/**	image model tags = image:
	@image displays a triangle that is masked with a circle.
*/
image :: m *TagSource -> Image m
image model tags
	= flipy (polygon [(zero,zero),(d,zero),(d /. 2, d)])
	      <@< {mask = margin (d *. 0.3,px zero,px zero,d *. 0.1) (circle (d *. 0.8) <@< {fill = toSVGColor "white"})}
where
	d = px 88.0
	m = px 10.0
