module IncrementingSequence

/**	Test program that creates an incrementing sequence of images at each mouse click.
*/

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.UI.Prompt
import iTasks.Extensions.SVG.SVGEditor
import StdEnum, StdFunctions, StdList, StdReal

white = toSVGColor "white"
none  = toSVGColor "none"

Start :: *World -> *World
Start world
	= doTasks (viewInformation "IncrementingSequence" [ViewUsing id (fromSVGEditor
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
	= margin (px 10.0) (
		grid (Columns 100) (RowMajor,LeftToRight,TopToBottom) (repeat (AtMiddleX,AtMiddleY)) (repeat (r+m)) (repeat (r+m)) []
			(take n
		        [  circle r <@< {stroke = none}
		                    <@< {fill   = toSVGColor {r=i*2,g=(i*j) rem 200+50,b=(i+j) rem 200+50}}
		        \\ i <- [0..100], j <- [0..100]
		        ]
		    ) host
//	  ) <@< {onclick=((*) 2),local=False}
	  ) <@< {onclick=((+) c),local=False}
where
	c    = 100
	r    = px 6.0
	m    = px 2.0
	host = Host (rect (px 800.0) (px (toReal (8*(n/100+1)))) <@< {fill=white})
