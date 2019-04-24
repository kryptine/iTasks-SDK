module OnClick

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.UI.Prompt
import iTasks.Extensions.SVG.SVGEditor
import StdArray, StdClass, StdFunctions, StdInt, StdList, StdReal
import Text

//	shorthand definitions for the used fonts in these examples
times			= normalFontDef "Times New Roman"

//	shorthand definitions for the used colours in these examples
white			= toSVGColor "white"

Start :: *World -> *World
Start world
	= doTasks (updateInformation "On Click"
		[UpdateUsing id (\_ v = v) (fromSVGEditor
			{ initView    = id
			, renderImage = const count
			, updModel    = \_ v = v
			})] 0) world

/**	count n tags = image:
	@image displays the number of times that you've clicked on the text. The initial value is @n.
*/
count :: Int *TagSource -> Image Int
count n _ = margin (px 20.0) (beside [] [] Nothing [] (map digit (digits n)) NoHost <@< {onclick = (+), local = False})

digits :: Int -> [Int]
digits n = [toInt c - toInt '0' \\ c <-: toString n]

digit :: Int -> Image Int
digit n = overlay [(AtMiddleX,AtMiddleY)] []
             [ text font (toString n) <@< {fill = white}]
             (Host (rect (textxspan font (toString n) + px m) (px (h+m))))
where
	font = times h
	h    = 100.0
	m    = 6.0
