module TonsOfCircles

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.UI.Prompt
import iTasks.Extensions.SVG.SVGEditor
import StdEnum, StdFunctions, StdReal

white = toSVGColor "white"
none  = toSVGColor "none"

Start :: *World -> *World
Start world
	= doTasks (viewInformation "TonsOfCircles" [ViewUsing id (fromSVGEditor
																{ initView    = id
																, renderImage = const tons_of_circles
																, updModel    = \_ v = v
																})] 0) world

/**	tons_of_circles model tags = image:
	@image shows a collage of circles
*/
tons_of_circles :: m *TagSource -> Image m
tons_of_circles model tags
	= margin (px 10.0) (
		collage [  (px x,px y) \\ x <- [0.0, 8.0 .. 800.0], y <- [0.0, 8.0 .. 800.0]]
		        [  circle (px 6.0) <@< {stroke = none}
		                           <@< {fill   = toSVGColor {r=i*2,g=(i*j) rem 200+50,b=(i+j) rem 200+50}}
		        \\ i <- [0..100], j <- [0..100]
		        ]
		        host
	  )
where
	host	= Host (rect (px 808.0) (px 808.0) <@< {fill=white} <@< {onclick= \_ m = m,local=False})
