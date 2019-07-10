implementation module Trax.UI

import StdBool, StdList
from   StdFunc import const, flip, id
from   Data.List import lookup
import iTasks.WF.Tasks.Interaction
import iTasks.Extensions.SVG.SVGEditor
import Trax.UoD

:: RenderMode = ViewMode | PlayMode

updateTraxEditor :: Bool -> UpdateOption TraxSt TraxSt
updateTraxEditor turn				= UpdateUsing id (const id) (fromSVGEditor
													{ initView    = id
													, renderImage = \_ -> toImage PlayMode turn
													, updModel    = flip const
													})

viewTraxEditor :: ViewOption TraxSt
viewTraxEditor						= ViewUsing id (fromSVGEditor
													{ initView    = id
													, renderImage = \_ -> toImage ViewMode False
													, updModel    = flip const
													})

whiteColor							= toSVGColor "white"
redColor							= toSVGColor "red"
freeTileColor						= toSVGColor "lightgrey"
transparentColor					= toSVGColor "none"

font								= normalFontDef "Arial" 14.0
tileSize							= px 50.0

toImage :: RenderMode Bool TraxSt *TagSource -> Image TraxSt
toImage ViewMode _ st _
	= board False tileSize st
toImage PlayMode my_turn st=:{turn} _
	= above (repeat AtMiddleX) [] Nothing [] [text font message, board it_is_my_turn tileSize st] NoHost
where
	it_is_my_turn					= my_turn == turn
	message							= if it_is_my_turn "Select a tile" "Wait for other player..."

board :: Bool Span TraxSt -> Image TraxSt
board it_is_my_turn d st=:{trax}
| no_of_tiles trax == zero
	| it_is_my_turn					= grid (Rows 2) (RowMajor, LeftToRight, TopToBottom) [] [] [] []
									   [  tileImage d tile <@< {onclick = start_with_this tile, local = False}
									   \\ tile <- gFDomain{|*|}
									   ] NoHost
	| otherwise						= voidImage d
| otherwise							= grid (Rows (maxy - miny + 3)) (RowMajor, LeftToRight, TopToBottom) (repeat (AtMiddleX,AtMiddleY)) [] [] []
								       [  case tile_at trax coord of
								              Nothing   = if (it_is_my_turn && isMember coord free_coords) (freeImage d coord st) (voidImage d)
								              Just tile = tileImage d tile
								       \\ row <- [miny - 1 .. maxy + 1]
								        , col <- [minx - 1 .. maxx + 1]
								        , let coord = fromTuple (col,row)
								       ] NoHost
where
	((minx,maxx),(miny,maxy))		= bounds trax
	(o_x, o_y)						= (abs (min 0 (minx-1)), abs (min 0 (miny-1)))
	free_coords						= free_coordinates trax

voidImage :: Span -> Image a
voidImage d							= empty d d

illegalImage :: Span -> Image a
illegalImage d						= tileShape d <@< {fill = transparentColor}

unselectedImage :: Span -> Image a
unselectedImage d					= tileShape d <@< {fill = freeTileColor}

freeImage :: Span Coordinate TraxSt -> Image TraxSt
freeImage d coord {trax,choice}
| isEmpty candidates				= illegalImage d
| maybe True ((<>) coord) choice	= unselectedImage d <@< {onclick = setcell coord, local = False}
| otherwise							= above (repeat AtMiddleX) [] (Just d) []
									        [tileImage (d /. no_of_candidates) tile <@< {onclick = settile coord tile, local = False} \\ tile <- candidates]
									        (Host (unselectedImage d))
where
	candidates						= [tile \\ tile <- possible_tiles trax coord | isJust (mandatory_moves (add_tile coord tile trax) coord)]
	no_of_candidates				= length candidates

tileImage :: Span TraxTile -> Image a
tileImage d tile					= fromJust (lookup tile [ (horizontal,rotate (deg 0.0)   horizontal_tile)
						                                    , (vertical,  rotate (deg 90.0)  horizontal_tile)
						                                    , (northwest, rotate (deg 0.0)   northwest_tile)
						                                    , (northeast, rotate (deg 90.0 ) northwest_tile)
						                                    , (southeast, rotate (deg 180.0) northwest_tile)
						                                    , (southwest, rotate (deg 270.0) northwest_tile)
						                                    ])
where
	brick							= Host (tileShape d <@< {stroke = whiteColor} <@< {strokewidth = d /. 20})
	horizontal_tile					= overlay (repeat (AtMiddleX,AtMiddleY)) [] [bar yline whiteColor, bar xline redColor] brick
	northwest_tile					= (overlay [] [(d /. 2, d /. 2),(d /. -2, d /. -2)]
									           [ arc whiteColor, arc redColor ]
									           brick
									  ) <@< { MaskAttr | mask = tileShape d <@< {fill = whiteColor}}
	bar line c						= line   d <@< {stroke = c} <@< {strokewidth = d /. 5}
	arc c							= circle d <@< {stroke = c} <@< {strokewidth = d /. 5} <@< {fill = transparentColor}

tileShape :: Span -> Image a
tileShape d							= square d <@< {xradius = d /. 10} <@< {yradius = d /. 10}
