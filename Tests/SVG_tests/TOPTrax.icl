module TOPTrax

// compile with Clean 2.4 + environment iTasks
import StdMisc
//import GenPrint
//import trax_without_generics
import trax
//import game2
import iTasks, MultiUser
import Data.List
from StdFunc import flip

derive class iTask TraxSt
derive gEditor    Coordinate, TraxTile, TileEdge, Trax
derive gEditMeta  Coordinate, TraxTile, TileEdge, Trax
derive gUpdate    Coordinate, TraxTile, TileEdge, Trax
derive gVerify    Coordinate, TraxTile, TileEdge, Trax
derive gText      Coordinate, TraxTile, TileEdge, Trax
derive JSONEncode Coordinate, TraxTile, TileEdge, Trax
derive JSONDecode Coordinate, TraxTile, TileEdge, Trax
derive gDefault   Coordinate, TraxTile, TileEdge, Trax

Start :: *World -> *World
Start world = StartMultiUserTasks [ /*workflow  "Original Trax" "Play trax"  	 play_trax
								  ,*/ workflow  "SVG Trax"      "Play SVG trax"  play_trax2
								  ] world

/*
toLineColor :: !Turn -> LineColor
toLineColor turn = if (match 0 turn) RedLine WhiteLine

player :: ![User] !Turn -> User
player [a,b] turn = if (match 0 turn) a b

play_trax :: Task Turn
play_trax = play_for_N 2 { game   = "Trax"
                         , state  = initial_state
                         , over   = game_over
                         , winner = declare_winner
                         , move   = make_a_move
                         , board  = show_board
                         }

initial_state :: ![User] -> TraxSt
initial_state users = { trax = zero, names = users }

game_over :: !(Turn,TraxSt) -> Bool
game_over (_,traxSt) = not (isEmpty (loops traxSt.trax ++ winning_lines traxSt.trax))

declare_winner :: (Turn,TraxSt) -> Task Turn
declare_winner (turn,traxSt=:{trax,names})
	= viewInformation "The winner is:" [ViewWith (toString o (player names))] winner
where
	winners        = loops trax ++ winning_lines trax
	last_player    = prev turn
	winner         = if (isMember (toLineColor last_player) (map fst winners)) last_player turn

make_a_move :: (Turn,TraxSt) -> Task TraxSt
make_a_move (turn,traxSt=:{trax})
	=              chooseCoordinate trax
      >>= \new  -> chooseTile   new trax
      >>= \tile -> return {traxSt & trax = mandatory_moves (add_tile new tile trax) new}
where
	chooseCoordinate :: !Trax -> Task Coordinate
	chooseCoordinate trax
	| nr_of_tiles trax == 0    = return zero
	| otherwise                = enterChoice "Choose coordinate:" [ChooseWith (ChooseFromComboBox toString)] (free_coordinates trax)
	
	chooseTile :: !Coordinate !Trax -> Task Tile
	chooseTile coordinate trax = enterChoice "Choose tile:" [ChooseWith (ChooseFromRadioButtons (TileTag (16,16)))] 
		                             (if (nr_of_tiles trax == 0) 
		                                 gFDomain{|*|} 
		                                 (possible_tiles (linecolors trax coordinate))
		                             )

show_board :: !(Turn,TraxSt) -> [HtmlTag]
show_board (turn,traxSt=:{trax,names})
| nr_of_tiles trax == 0 = [h3 ("Select any tile, " <+++ current_player)]
| otherwise				= [h3 current_player, board]
where
	board				= TableTag [BorderAttr "0"] 
						           [ tr [  cell {col=minx+x-1,row=miny+y-1} 
						                \\ x <- [0..nrcol+1]
						                ] 
						           \\ y <- [0..nrrow+1]
						           ]
	current_player      = player names turn
    cell coordinate     = case tile_at trax coordinate of
                             Nothing   = if (isMember coordinate free) (text coordinate) (text "")
                             Just tile = td [TileTag (42,42) tile]
    
    free                      = free_coordinates trax
    ((minx,maxx),(miny,maxy)) = bounds           trax
    (nrcol,nrrow)             = dimension        trax

TileTag :: !(!Int,!Int) !Tile -> HtmlTag
TileTag (width,height) tile
	= ImgTag [SrcAttr ("/" <+++ toString tile <+++ ".png"),WidthAttr (toString width),HeightAttr (toString height)]

// shorthands for HTML:
tr		= TrTag []
td		= TdTag []
text x	= TdTag [AlignAttr "center"] [Text (toString x)]
h3   x	= H3Tag [] [text x]
*/


//	SVG version of trax
import iTasks.API.Extensions.SVG.SVGlet


:: TraxSt
 = { trax   :: !Trax              // the current set of placed tiles
   , names  :: ![User]            // the current two players
   , turn   :: !Bool
   , choice :: !Maybe Coordinate
   }

play_trax2 :: Task User
play_trax2
	=             get currentUser
	  >>= \me  -> enterChoiceWithShared "Who do you want to play Trax with:" [] users
	  >>= \you -> playGame2 me you {trax=zero,names=[me,you],turn=True,choice=Nothing}
	  >>* [OnValue (ifValue game_over game_winner)]

game_over :: TraxSt -> Bool
game_over st=:{trax}
	= not (isEmpty winners)
where
	winners = loops trax ++ winning_lines trax

game_winner :: TraxSt -> Task User
game_winner st=:{trax,turn,names=[me,you]}
	= viewInformation "The winner is:" [] winner -&&- viewInformation "Final board:" [imageView (board False (px 50.0))] st @ fst
where
	winners				= loops trax ++ winning_lines trax
	prev_player_color	= if turn WhiteLine RedLine
	winner				= if (isMember prev_player_color (map fst winners)) (if turn you me) (if turn me you)

playGame2` :: User user TraxSt -> Task TraxSt
playGame2` me you traxSt
	= updateInformation "Play Trax" [imageViewUpdate id toImage` (flip const)] traxSt

playGame2 :: User User TraxSt -> Task TraxSt
playGame2 me you traxSt
	= withShared traxSt
	  (\share -> updateSharedInformation (toString me  +++ " plays with red")   [imageViewUpdate id (toImage True)  (flip const)] share
	             -&&-
	             updateSharedInformation (toString you +++ " plays with white") [imageViewUpdate id (toImage False) (flip const)] share
	  ) @ fst

start_with_this :: TraxTile TraxSt -> TraxSt
start_with_this tile st=:{trax,turn}
	= {st & trax = add_tile zero tile trax, turn = not turn}

setcell :: Coordinate TraxSt -> TraxSt
setcell coord st
	= {st & choice = Just coord}

settile :: Coordinate TraxTile TraxSt -> TraxSt
settile coord tile st=:{trax,turn}
	= {st & trax = mandatory_moves (add_tile coord tile trax) coord, choice = Nothing, turn = not turn}

toImage` :: TraxSt -> Image TraxSt
toImage` st=:{trax,names=[me,you],turn}
	= above (repeat AtMiddleX) [] [text font message, board True d st] Nothing
where
	message						= toString (if turn me you) +++ " plays with " +++ if turn "red" "white"
	d							= px 50.0

toImage :: Bool TraxSt -> Image TraxSt
toImage my_turn st=:{trax,names=[me,you],turn}
	= above (repeat AtMiddleX) [] [text font message, board it_is_my_turn d st] Nothing
where
	it_is_my_turn				= my_turn == turn
	message						= if it_is_my_turn "Select a tile" "Wait for other player..."
	d							= px 50.0

board :: Bool Span TraxSt -> Image TraxSt
board it_is_my_turn d st=:{trax}
| nr_of_tiles trax == zero
	| it_is_my_turn				= grid (Rows 2) (LeftToRight,TopToBottom) [] [] 
							           [tileImage d tile <@< {onclick = start_with_this tile} \\ tile <- gFDomain{|*|}] Nothing
	| otherwise					= voidImage d
| otherwise						= grid (Rows (maxy-miny+3)) (LeftToRight,TopToBottom) (repeat (AtMiddleX,AtMiddleY)) []
							           [  case tile_at trax coord of
							                 Nothing   = if (it_is_my_turn && isMember coord free_coords) (freeImage d coord st) (voidImage d)
							                 Just tile = tileImage d tile
							           \\ row <- [miny-1..maxy+1]
							            , col <- [minx-1..maxx+1]
							            , let coord = fromTuple (col,row)
							           ] Nothing
where
	((minx,maxx),(miny,maxy))	= bounds trax
	free_coords					= free_coordinates trax

voidImage :: Span -> Image a
voidImage d				= empty d d

freeImage :: Span Coordinate TraxSt -> Image TraxSt
freeImage d coord {trax,choice}
| maybe True (\c -> coord <> c) choice
						= unselected <@< {onclick = setcell coord}
| otherwise				= above [] [] [tileImage (d /. nr_of_candidates) tile <@< {onclick = settile coord tile} \\ tile <- candidates] Nothing
where
	candidates			= possible_tiles (linecolors trax coord)
	nr_of_candidates	= length candidates
	unselected			= tileShape d <@< {fill = toSVGColor "lightgrey"}

tileImage :: Span TraxTile -> Image a
tileImage d tile		= fromJust (lookup tile [ (horizontal,rotate (Deg 0.0)   horizontal_tile)
			                                    , (vertical,  rotate (Deg 90.0)  horizontal_tile)
			                                    , (northwest, rotate (Deg 0.0)   northwest_tile)
			                                    , (northeast, rotate (Deg 90.0 ) northwest_tile)
			                                    , (southeast, rotate (Deg 180.0) northwest_tile)
			                                    , (southwest, rotate (Deg 270.0) northwest_tile)
			                                    ])
where
	brick				= Just (tileShape d <@< {stroke = toSVGColor "white"} <@< {strokewidth = d /. 20})
	horizontal_tile		= overlay (repeat (AtMiddleX,AtMiddleY)) [] [ bar yline "white", bar xline "red" ] brick
	northwest_tile		= maskWith (overlay [] [(d /. 2, d /. 2),(d /. -2, d /. -2)] [ arc "white", arc "red" ] brick)
	                               (tileShape d <@< {fill = toSVGColor "white"})
	bar line c			= line Nothing d <@< {stroke = toSVGColor c} <@< {strokewidth = d /. 5}
	arc c				= circle d <@< {stroke = toSVGColor c} <@< {strokewidth = d /. 5} <@< {fill = toSVGColor "none"}

tileShape :: Span -> Image a
tileShape d				= rect d d <@< {xradius = d /. 10} <@< {yradius = d /. 10}

font	= { fontfamily  = "Arial"
	      , fontysize   = 14.0
	      , fontstretch = ""
	      , fontstyle   = ""
	      , fontvariant = ""
	      , fontweight  = ""
	      }
