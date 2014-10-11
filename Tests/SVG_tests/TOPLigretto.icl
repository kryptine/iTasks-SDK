module TOPLigretto

import ligrettoModel
import MultiUser
from   StdFunc import flip
from   StdMisc import abort
from   Control.Monad import replicateM

Start :: *World -> *World
Start world = StartMultiUserTasks [ workflow "SVG Ligretto" "Play SVG Ligretto" play_Ligretto ] world

//	SVG version of Ligretto
import iTasks.API.Extensions.SVG.SVGlet

//	Make iTask infrastructure available for Ligretto model data types:
derive class iTask GameSt, Player, Color, Hand, Card, SideUp

::	GameSt = { middle  :: Middle
             , players :: [Player]
             }

player_st :: Color [Player] -> Player
player_st color players
	= case [player \\ player <- players | player.color === color] of
	     [player : _] = player
	     ouch         = abort ("Could not find player with color " <+++ color)

player_upd :: Player [Player] -> [Player]
player_upd player players
	= [if (p.Player.color === player.Player.color) player p \\ p <- players]

player_expell :: Color [Player] -> [Player]
player_expell color players
	= [p \\ p <- players | p.Player.color =!= color]

replicateTask :: Int (Task a) -> Task [a] | iTask a		// probably similar task combinator already exists?
replicateTask n t
| n <= 0		= return []
| otherwise		= t >>= \a -> replicateTask (n-1) t >>= \as -> return [a:as]

play_Ligretto :: Task (Color,User)
play_Ligretto
	=               get currentUser
	>>= \me      -> invite_friends
	>>= \friends -> let nr_of_players = length friends + 1 
	                 in replicateTask nr_of_players (get randomInt)
	>>= \rs      -> let gameSt        = { middle  = repeatn 16 []
	                                    , players = [initial_player nr_of_players color (abs r) \\ color <- colors nr_of_players & r <- rs]
	                                    }
	                 in withShared gameSt (play_game nr_of_players [me : friends])

play_game :: Int [User] (Shared GameSt) -> Task (Color,User)
play_game nr_of_players player_ids game_st
	=               anyTask  [ player @: game nr_of_players (color,player) game_st \\ player <- player_ids & color <- colors nr_of_players ]
	>>= \winner ->  allTasks [ player @: viewInformation "The winner is:" [] winner >>= return \\ player <- player_ids ]
	>>| return winner

game :: Int (Color,User) (Shared GameSt) -> Task (Color,User)
game nr_of_players (color,user) game_st
	=   updateSharedInformation (toString user) [imageViewUpdate (get_player_middle color) player_perspective set_player_middle] game_st
	>>* [OnValue (player_wins (color,user))]

player_wins :: (Color,User) (TaskValue GameSt) -> Maybe (Task (Color,User))
player_wins (color,user) (Value {players} _)
| isEmpty (player_st color players).ligretto
	= Just (return (color,user))
player_wins _ _
	= Nothing

get_player_middle :: Color GameSt -> (Player,[Player],Middle)
get_player_middle color game_st=:{middle,players}
	= (player_st color players,player_expell color players,middle)

set_player_middle :: GameSt (Player,[Player],Middle) -> GameSt
set_player_middle game_st=:{players} (player,_,middle)
	= {GameSt | game_st & middle  = middle
	                    , players = player_upd player players
	  }

invite_friends :: Task [User]
invite_friends
	=               enterSharedMultipleChoice "Select friends to play with" [] users
	>>= \friends -> if (not (isMember (length friends) [1..3]))
	                   (viewInformation "Oops" [] "number of friends must be 1, 2, or 3" >>| invite_friends)
	                   (return friends)

play_concealed_pile :: Player -> Player
play_concealed_pile player
| isEmpty player.hand.conceal
							= shuffle_hand (sum [1,length player.ligretto,length player.hand.discard]) player	// ISSUE: random value should be obtained from randomInt SDS
| otherwise					= swap_discards player

play_hand :: (Player,[Player],Middle) -> (Player,[Player],Middle)
play_hand (player,players,middle)
| isNothing maybe_card		= (player,players,middle)
| isEmpty matching_piles	= (player,players,middle)
# (pilenr,pile)				= hd matching_piles
| otherwise					= (remove_top_of_discard player,players,updateAt pilenr [card:pile] middle)
where
	maybe_card				= top_discard player
	card					= fromJust maybe_card
	matching_piles			= [(pilenr,pile) \\ pile <- middle & pilenr <- [0..] | card_matches_top_of_pile card pile]

play_row_card :: Card Int (Player,[Player],Middle) -> (Player,[Player],Middle)
play_row_card card cardnr (player,players,middle)
| isEmpty matching_piles	= (player,players,middle)
# (pilenr,pile)				= hd matching_piles
| otherwise					= (move_ligretto_card_to_row cardnr player,players,updateAt pilenr [card:pile] middle)
where
	matching_piles			= [(pilenr,pile) \\ pile <- middle & pilenr <- [0..] | card_matches_top_of_pile card pile]


card_image :: SideUp Card -> Image m
card_image side card
| side === Front			= overlay [(AtMiddleX,AtTop),(AtMiddleX,AtBottom)] [] [nr, rotate (Deg 180.0) nr] host
| otherwise					= overlay [(AtLeft,AtBottom)] [] [ligretto] host
where
	cardcolor				= if (side === Front) (toSVGColor card.front) (toSVGColor "white")
	host					= Just back
	back					= card_shape <@< {fill = cardcolor}
	nr						= margin (px 5.0)
							  (text (cardfont 20.0) (toString card.nr) <@< {fill = toSVGColor "white"}
							                                           <@< {stroke = toSVGColor (nr_stroke_color card.front)}
							  )
	ligretto				= skewy (Deg -20.0) 
							  (text (cardfont (w / 5.0)) "Ligretto" <@< {stroke = toSVGColor card.back} <@< {fill = toSVGColor "none"})
	(w,h)					= card_size
	nr_stroke_color Red		= Blue
	nr_stroke_color Green	= Red
	nr_stroke_color Blue	= Yellow
	nr_stroke_color Yellow	= Green

card_shape :: Image m
card_shape					= rect (px w) (px h) <@< {xradius = px (h / 18.0)} <@< {yradius = px (h / 18.0)}
where
	(w,h)					= card_size

card_size :: (Real,Real)
card_size					= (58.5, 90.0)

no_card_image :: Image m
no_card_image				= overlay [(AtMiddleX,AtMiddleY)] [] [text (pilefont 12.0) "empty"] host	// BUG: "empty" text is not aligned properly
where
	host					= Just (card_shape <@< {fill = toSVGColor "lightgrey"})

hand_image :: Bool Hand -> Image (Player,[Player],Middle)
hand_image interactive {conceal,discard}
							= beside [] [] (if interactive [conceal_pile <@< {onclick = app3 (play_concealed_pile,id,id)}
							                               ,discard_pile <@< {onclick = play_hand}
							                               ]
							                               [conceal_pile
							                               ,discard_pile
							                               ]
							               ) Nothing
where
	conceal_pile			= pile_image Back  conceal
	discard_pile			= pile_image Front discard

pile_image :: SideUp Pile -> Image m
pile_image side pile
| nr_of_cards > 10			= above [AtMiddleX] [] [text (pilefont 10.0) (toString nr_of_cards),top_cards_image] Nothing
| otherwise					= top_cards_image
where
	nr_of_cards				= length pile
	top_cards				= take 10 pile
	nr_of_top_cards			= length top_cards
	top_cards_image			= overlay [] [(zero,px ((toReal dx)*h/18.0)) \\ dx <- [0..nr_of_top_cards-1]] 
							             (map (card_image side) (reverse top_cards)) (Just no_card_image)
	(_,h)					= card_size

row_image :: Bool RowPlayer -> Image (Player,[Player],Middle)
row_image interactive row	= beside [] [] 
							         [ let card = card_image Front row_card in
							            if interactive (card <@< {onclick = play_row_card row_card cardnr}) card
							         \\ row_card <- row 
							          & cardnr   <- [1..]
							         ]  Nothing

player_image :: Bool Player -> Image (Player,[Player],Middle)
player_image interactive player
							= beside [] [] [ row_image interactive player.row, empty (px w) zero
							               , pile_image Front player.ligretto, empty (px w) zero		// BUG: only margin (px zero,px w) around pile_image does not work
							               , hand_image interactive player.hand
							               ] Nothing
where
	(w,_)					= card_size

middle_image :: Middle -> Image m
middle_image middle			= grid (Rows 2) (LeftToRight,TopToBottom) [] [] (map (pile_image Front) middle) Nothing


player_perspective :: (Player,[Player],Middle) -> Image (Player,[Player],Middle)
player_perspective (player,opponents,middle)
							= margin (h,w,h,w) 
							  (overlay [(AtMiddleX,AtMiddleY),(AtMiddleX,AtMiddleY)] [] [circular 300.0 [player_image True player : map (player_image False) opponents],middle_image middle] Nothing)
where
	(w,h)					= card_size

circular :: Real [Image m] -> Image m
circular r imgs				= overlay (repeat (AtMiddleX,AtMiddleY)) 
							          [(px (~r * cos (i*alpha - pi/2.0)),px (~r * sin (i*alpha - pi/2.0))) \\ i <- [0.0, 1.0 ..] & img <- imgs] 
							          [rotate (Rad (i*alpha)) img \\ i <- [0.0, 1.0 ..] & img <- imgs] 
							          (Just (circle (px (2.0*r)) <@< {fill=toSVGColor "none"}))		// BUG: using Nothing creates incorrect image (offset to left)where
where
	n     				    = length imgs
	alpha					= 2.0 * pi / (toReal n)
	pi						= 3.1415926

instance toSVGColor Color where toSVGColor Red    = toSVGColor "darkred"
                                toSVGColor Green  = toSVGColor "darkgreen"
                                toSVGColor Blue   = toSVGColor "midnightblue"
                                toSVGColor Yellow = toSVGColor "gold"

cardfont :: !Real -> FontDef
cardfont size
	= { fontfamily  = "Verdana"
      , fontysize   = size
      , fontstretch = "normal"
      , fontstyle   = "normal"
      , fontvariant = "normal"
      , fontweight  = "bold"
      }

pilefont :: !Real -> FontDef
pilefont size
	= { fontfamily  = "Verdana"
      , fontysize   = size
      , fontstretch = "normal"
      , fontstyle   = "normal"
      , fontvariant = "normal"
      , fontweight  = "normal"
      }
