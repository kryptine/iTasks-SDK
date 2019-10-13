module IncrementingSequence

/**	Test program that creates an incrementing sequence of images at each mouse click.
*/

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.Extensions.SVG.SVGEditor
import StdEnum, StdFunctions, StdList, StdReal

white = toSVGColor "white"
none  = toSVGColor "none"

Start :: *World -> *World
Start world
	= doTasks (viewInformation [ViewUsing id (fromSVGEditor
												{ initView    = id
												, renderImage = const sequence
												, updModel    = \_ v = v
												})] 1) world

/**	sequence model tags = image:
	@image shows a grid of circles, initially just 1.
	At each mouse click the current number of circles is doubled.
*/
sequence :: Int *TagSource -> Image Int
sequence n tags
	= margin (px 10) (
		grid (Columns 100) (RowMajor,LeftToRight,TopToBottom) (repeat (AtMiddleX,AtMiddleY)) (repeat (r+m)) (repeat (r+m)) []
			(take n
		        [  circle r <@< {stroke = none}
		                    <@< {fill   = toSVGColor {r=i*2,g=(i*j) rem 200+50,b=(i+j) rem 200+50}}
		        \\ i <- [0..100], j <- [0..100]
		        ]
		    ) host
//	  ) <@< {onclick=((*) 2),local=False}
	  ) <@< {onclick = \_ n = c+n,local=True}
where
	c    = 100
	r    = px 6
	m    = px 2
	host = Host (rect (px 800) (px (8*(n/100+1))) <@< {fill=white})
