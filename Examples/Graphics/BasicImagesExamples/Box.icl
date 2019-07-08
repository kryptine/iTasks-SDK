module Box

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.UI.Prompt
import iTasks.Extensions.SVG.SVGEditor
import StdFunctions

//	shorthand definitions for the used colours in these examples
none			= toSVGColor "none"

Start :: *World -> *World
Start world
	= doTasks (viewInformation "Box"
		[ViewUsing id (fromSVGEditor
			{ initView    = id
			, renderImage = const box2
			, updModel    = \_ v = v
			})] 0) world

/** box2 model tags = image:
	@image uses the pair function that uses @tags to display two images inside a rectangle that depends on each others dimensions.
*/
box2 :: m *TagSource -> Image m
box2 _ tags = pair (arrow, arrow`) tags
where
	arrow  = polygon [(px  0,px -10),(px 55,px -10),(px 50,px -30),(px 85,px 0)
	                 ,(px 50,px  30),(px 55,px  10),(px  0,px  10)
	                 ]
	arrow` = polygon [(px -10,px  0),(px -10,px 55),(px -30,px 50),(px 0,px 85)
	                 ,(px  30,px 50),(px  10,px 55),(px  10,px  0)
	                 ]

/**	pair (img1,img2) tags = image:
	@image uses @tags to put @img1 and @img2 beside each other on a host that fits their width and height.
*/
pair :: (Image m,Image m) *TagSource -> Image m
pair (img1,img2) [(t1,ut1),(t2,ut2):tags]
	= beside [] [] Nothing []
	      [ overlay [(AtMiddleX,AtMiddleY)] [] [tag ut1 img1] host
	      , overlay [(AtMiddleX,AtMiddleY)] [] [tag ut2 img2] host
	      ] NoHost
where
	(w1,h1) = (imagexspan t1,imageyspan t1)
	(w2,h2) = (imagexspan t2,imageyspan t2)
	host    = Host (rect (maxSpan [w1,w2]) (maxSpan [h1,h2]) <@< {fill = none})
