module TOPTrax

// compile with Clean 2.4 + environment iTasks
import StdMisc
//import GenPrint
//import trax_without_generics
import trax
//import game2
import iTasks, MultiUser
import Data.List

derive class iTask TraxSt, TileChoice
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
   , choice :: !Maybe TileChoice
   }
:: TileChoice 
	= { location :: Coordinate
	  , tile     :: Maybe TraxTile
	  }

play_trax2 :: Task (TraxSt,TraxSt)
play_trax2
	=             get currentUser
	  >>= \me  -> enterChoiceWithShared "Who do you want to play Trax with:" [] users
	  >>= \you -> playGame2 me you {trax=loop,names=[me,you],turn=True,choice=Nothing}

loop = {tiles = [({col=0,row=0},southeast),({col=1,row=0},horizontal),({col=2,row=0},horizontal),({col=3,row=0},southwest)
                ,({col=0,row=1},vertical), ({col=1,row=1},southeast), ({col=2,row=1},horizontal),({col=3,row=1},northwest),({col=4,row=1},vertical)
                ,({col=0,row=2},northeast),({col=1,row=2},northwest), ({col=2,row=2},southeast)
                ]
       }

playGame2 me you traxSt
	= withShared traxSt
	  (\share -> updateSharedInformation (toString me)  [imageViewUpdate toAction (toImage False) fromAction] share
	             -&&-
	             updateSharedInformation (toString you) [imageViewUpdate toAction (toImage True)  fromAction] share
	  )

toAction :: TraxSt -> ActionState () TraxSt
toAction traxSt = {ActionState | state = traxSt, action = Nothing}

fromAction :: TraxSt (ActionState () TraxSt) -> TraxSt
fromAction _ {ActionState | state = st=:{choice = Just {location,tile=Just t}}}
	= {st & trax = add_tile location t st.trax, choice = Nothing}
fromAction _ {ActionState | state}
	= state

toImage :: Bool (ActionState () TraxSt) -> Image (ActionState () TraxSt)
toImage turn st=:{ActionState | state}
	= grid (Rows (maxy-miny+3)) (LeftToRight,TopToBottom) (repeat (AtMiddleX,AtMiddleY)) []
	       [  case tile_at state.trax coord of
	             Nothing   = if (isMember coord free_coords) (freeImage d coord st) (voidImage d)
	             Just tile = tileImage d tile
	       \\ row <- [miny-1..maxy+1]
	        , col <- [minx-1..maxx+1]
	        , let coord = fromTuple (col,row)
	       ]  Nothing
where
	((minx,maxx),(miny,maxy))	= bounds state.trax
	free_coords					= free_coordinates state.trax
	d							= px 50.0

voidImage :: Span -> Image a
voidImage d				= empty d d

freeImage :: Span Coordinate (ActionState () TraxSt) -> Image (ActionState () TraxSt)
freeImage d coord {ActionState | state={trax,choice}}
| isNothing choice || coord <> choice_coord
						= unselected <@< {onclick = setcell}
//| isNothing choice_tile	= above [] [] [tileImage (d /. nr_of_candidates) tile <@< {onclick = settile tile} \\ tile <- candidates] Nothing
| isNothing choice_tile	= above [] [] [tileImage (d /. nr_of_candidates) tile \\ tile <- candidates] Nothing
//| isNothing choice_tile	= fity d (above [] [] [tileImage d tile \\ tile <- candidates] Nothing)
| otherwise				= tileImage d (fromJust choice_tile)
where
	choice_coord		= (fromJust choice).location
	choice_tile			= (fromJust choice).tile
	setcell      st		= {ActionState | st & state = {st.ActionState.state & choice = Just {location = coord, tile = Nothing}}}
	settile tile st		= {ActionState | st & state = {st.ActionState.state & choice = Just {location = coord, tile = Just tile}}}
	candidates			= possible_tiles (linecolors trax coord)
	nr_of_candidates	= length candidates
	unselected			= rect d d <@< {fill = toSVGColor "lightgrey"}
	selected			= unselected <@< {stroke = toSVGColor "grey"} <@< {strokewidth = d /. 20}

tileImage :: Span TraxTile -> Image a
tileImage d tile		= maybe (abort "tileImage Nothing")
                                id (lookup tile [ (horizontal,crossed (yline,xline))
						                        , (vertical,  crossed (xline,yline))
						                        , (northwest, curved [se,nw])
						                        , (northeast, curved [sw,ne])
						                        , (southeast, curved [nw,se])
						                        , (southwest, curved [ne,sw])
						                        ])
where
	brick				= Just (rect d d <@< {xradius = d /. 10} <@< {yradius = d /. 10} <@< {stroke = toSVGColor "white"} <@< {strokewidth = d /. 20})
	crossed (white,red)	= overlay (repeat (AtMiddleX,AtMiddleY)) [] [ bar white "white", bar red "red"] brick
	bar line c			= line Nothing d <@< {stroke = toSVGColor c} <@< {strokewidth = d /. 5}
	curved cs			= maskWith (overlay [] cs [ arc "white", arc "red"] brick) (rect d d <@< {fill = toSVGColor "white"})
	arc c				= circle d <@< {stroke = toSVGColor c} <@< {strokewidth = d /. 5} <@< {fill = toSVGColor "none"}
	nw					= (d /. -2, d /. -2)
	ne					= (d /.  2, d /. -2)
	se					= (d /.  2, d /.  2)
	sw					= (d /. -2, d /.  2)
