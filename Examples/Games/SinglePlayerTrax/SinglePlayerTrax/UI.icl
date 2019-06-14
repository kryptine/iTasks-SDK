implementation module SinglePlayerTrax.UI

import StdBool, StdFunctions, StdList
from   Data.List import lookup
import iTasks.WF.Tasks.Interaction
import iTasks.Extensions.SVG.SVGEditor
import SinglePlayerTrax.UoD

:: RenderMode = ViewMode | PlayMode

updateTraxEditor :: UpdateOption TraxSt TraxSt
updateTraxEditor = UpdateUsing id (const id) (fromSVGEditor
									{ initView    = id
									, renderImage = \_ -> toImage PlayMode
									, updModel    = flip const
									})

viewTraxEditor :: ViewOption TraxSt
viewTraxEditor = ViewUsing id (fromSVGEditor
									{ initView    = id
									, renderImage = \_ -> toImage ViewMode
									, updModel    = flip const
									})

whiteColor       = toSVGColor "white"
redColor         = toSVGColor "red"
freeTileColor    = toSVGColor "lightgrey"
transparentColor = toSVGColor "none"

toImage :: RenderMode TraxSt *TagSource -> Image TraxSt
toImage ViewMode st _
	= board tileSize st
toImage PlayMode st=:{turn} _
	= above (repeat AtMiddleX) [] Nothing [] [text font (toString turn), board tileSize st] NoHost

board :: Span TraxSt -> Image TraxSt
board d st=:{trax}
| no_of_tiles trax == zero
	= grid (Rows 2) (RowMajor, LeftToRight, TopToBottom) [] [] [] []
		   [  tileImage d tile <@< {onclick = const (start_with_this tile), local = False}
		   \\ tile <- gFDomain{|*|}
		   ] NoHost
| otherwise
	= grid (Rows (maxy - miny + 3)) (RowMajor, LeftToRight, TopToBottom) (repeat (AtMiddleX,AtMiddleY)) [] [] []
	       [  case tile_at trax coord of
	              Nothing   = if (isMember coord free_coords) (freeImage d coord st) (voidImage d)
	              Just tile = tileImage d tile
	       \\ row <- [miny - 1 .. maxy + 1]
	        , col <- [minx - 1 .. maxx + 1]
	        , let coord = fromTuple (col,row)
	       ] NoHost
where
	((minx,maxx),(miny,maxy))	= bounds trax
	(o_x, o_y)					= (abs (min 0 (minx-1)), abs (min 0 (miny-1)))
	free_coords					= free_coordinates trax

voidImage :: Span -> Image a
voidImage d						= empty d d

freeImage :: Span Coordinate TraxSt -> Image TraxSt
freeImage d coord {trax,choice}
| maybe True (\c -> coord <> c) choice
								= unselected
| otherwise						= above (repeat AtMiddleX) [] (Just d) []
								        [tileImage (d /. nr_of_candidates) tile <@< {onclick = const (settile coord tile), local = False} \\ tile <- candidates]
								        (Host unselected)
where
	candidates					= possible_tiles (linecolors trax coord)
	nr_of_candidates			= length candidates
	unselected					= tileShape d <@< {fill = freeTileColor} <@< {onclick = const (setcell coord), local = False}	

tileImage :: Span TraxTile -> Image a
tileImage d tile				= fromJust (lookup tile [ (horizontal,rotate (deg   0.0) horizontal_tile)
					                                    , (vertical,  rotate (deg  90.0) horizontal_tile)
					                                    , (northwest, rotate (deg   0.0) northwest_tile)
					                                    , (northeast, rotate (deg  90.0) northwest_tile)
					                                    , (southeast, rotate (deg 180.0) northwest_tile)
					                                    , (southwest, rotate (deg 270.0) northwest_tile)
					                                    ])
where
	brick						= Host (tileShape d <@< {stroke = whiteColor} <@< {strokewidth = d /. 20})
	horizontal_tile				= overlay (repeat (AtMiddleX,AtMiddleY)) [] [bar yline whiteColor, bar xline redColor] brick
	northwest_tile				= (overlay [] [(d /. 2, d /. 2),(d /. -2, d /. -2)]
								           [ arc whiteColor, arc redColor ]
								           brick
								  ) <@< { MaskAttr | mask = tileShape d <@< {fill = whiteColor}}
	bar line c					= line   d <@< {stroke = c} <@< {strokewidth = d /. 5}
	arc c						= circle d <@< {stroke = c} <@< {strokewidth = d /. 5} <@< {fill = transparentColor}

tileShape :: Span -> Image a
tileShape d						= square d <@< {xradius = d /. 10} <@< {yradius = d /. 10}

font							= normalFontDef "Arial" 14.0
tileSize						= px 50.0
