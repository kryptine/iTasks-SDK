implementation module Trax.UI

import StdBool, StdList
from   StdFunc import const, flip, id
import PlatformExts.List
import iTasks.WF.Tasks.Interaction
import iTasks.Extensions.SVG.SVGEditor
import Graphics.Scalable
import Trax.UoD

derive JSEncode TraxSt, User, Trax, TraxTile, TileEdge, /*Coordinate,*/ Maybe

updateTraxEditor :: Bool -> UpdateOption TraxSt TraxSt
updateTraxEditor flag = UpdateUsing id (const id) (fromSVGEditor
									{ initView    = id
									, renderImage = \_ -> toImage flag
									, updView     = const id
									, updModel    = flip const
									})

viewTraxEditor :: ViewOption TraxSt
viewTraxEditor = ViewUsing id (fromSVGEditor 
									{ initView    = id
									, renderImage = \_ -> toImage False
									, updView     = const id
									, updModel    = flip const
									})

whiteColor       = toSVGColor "white"
redColor         = toSVGColor "red"
freeTileColor    = toSVGColor "lightgrey"
transparentColor = toSVGColor "none"

toImage :: Bool TraxSt *TagSource -> Image TraxSt
toImage my_turn st=:{trax,names=[me,you],turn} _
	= above (repeat AtMiddleX) [] [text font message, board it_is_my_turn d st] NoHost
where
	it_is_my_turn				= my_turn == turn
	message						= if it_is_my_turn "Select a tile" "Wait for other player..."
	d							= px 50.0

board :: Bool Span TraxSt -> Image TraxSt
board it_is_my_turn d st=:{trax}
| nr_of_tiles trax == zero
	| it_is_my_turn				= grid (Rows 2) (RowMajor, LeftToRight, TopToBottom) [] [] 
							           [tileImage d tile <@< {onclick = const (start_with_this tile), local = False} \\ tile <- gFDomain{|*|}] NoHost
	| otherwise					= voidImage d
| otherwise						= grid (Rows (maxy - miny + 3)) (RowMajor, LeftToRight, TopToBottom) (repeat (AtMiddleX,AtMiddleY)) []
							           [  case tile_at trax coord of
							                 Nothing   = if (it_is_my_turn && isMember coord free_coords) (freeImage d coord st) (voidImage d)
							                 Just tile = tileImage d tile
							           \\ row <- [miny - 1 .. maxy + 1]
							            , col <- [minx - 1 .. maxx + 1]
							            , let coord = /*fromTuple*/ (col,row)
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
| otherwise						= above (repeat AtMiddleX) [] 
								        [tileImage (d /. nr_of_candidates) tile <@< {onclick = const (settile coord tile), local = False} \\ tile <- candidates]
								        (Host unselected)
where
	candidates					= possible_tiles (linecolors trax coord)
	nr_of_candidates			= length candidates
	unselected					= tileShape d <@< {fill = freeTileColor} <@< {onclick = const (setcell coord), local = False}

tileImage :: Span TraxTile -> Image a
tileImage d tile				= fromJust (lookup tile [ (horizontal,rotate (deg 0.0)   horizontal_tile)
					                                    , (vertical,  rotate (deg 90.0)  horizontal_tile)
					                                    , (northwest, rotate (deg 0.0)   northwest_tile)
					                                    , (northeast, rotate (deg 90.0 ) northwest_tile)
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
	bar line c					= line Nothing d <@< {stroke = c} <@< {strokewidth = d /. 5}
	arc c						= circle d <@< {stroke = c} <@< {strokewidth = d /. 5} <@< {fill = transparentColor}

tileShape :: Span -> Image a
tileShape d						= rect d d <@< {xradius = d /. 10} <@< {yradius = d /. 10}

font							= { fontfamily  = "Arial"
							      , fontysize   = 14.0
							      , fontstretch = ""
							      , fontstyle   = ""
							      , fontvariant = ""
							      , fontweight  = ""
							      }
