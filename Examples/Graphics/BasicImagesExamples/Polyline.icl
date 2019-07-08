module Polyline

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.UI.Prompt
import iTasks.Extensions.SVG.SVGEditor
import StdFunctions

//	shorthand definitions for the used colours in these examples
white			= toSVGColor "white"

Start :: *World -> *World
Start world
	= doTasks (viewInformation "Polyline" [ViewUsing id (fromSVGEditor
														{ initView    = id
														, renderImage = const polyline_in_host
														, updModel    = \_ v = v
														})] 0) world

/** polyline_in_host model tags = image:
	@image shows a polyline within a host.
*/
polyline_in_host :: m *TagSource -> Image m
polyline_in_host _ _
	= overlay [] [(px 10,px 10)]
			[polyline [(px 0,px 0),(px 25,px 25),(px 50,px 0),(px 75,px 25),(px 100,px 0),(px 85,px 100),(px 15,px 100),(px 0,px 0)]
			    <@< {stroke = white} <@< {strokewidth = px 3}
			]
			(Host (rect (px 120) (px 120)))
