module TOPLigretto

import ligrettoModel
import iTasks, MultiUser
from   StdFunc import flip

Start :: *World -> *World
Start world = StartMultiUserTasks [ workflow "SVG Ligretto" "Play SVG Ligretto" play_Ligretto ] world

//	SVG version of Ligretto
import iTasks.API.Extensions.SVG.SVGlet

//	Make iTask infrastructure available for Ligretto model data types:
derive class iTask Player, Color, Hand, Card, SideUp


play_Ligretto :: Task (Color,User)
play_Ligretto
	=               get currentUser
	>>= \me      -> invite_friends 
	>>= \friends -> set (repeatn 16 []) middle_state
	>>|             let nr_of_players = length friends + 1 in
	                anyTask
	                 [  /*player @:*/ game nr_of_players (color,player)							// BUG: @: is a partial function
	                 \\ player <- [me:friends] & color <- colors nr_of_players
	                 ]
	>>= \winner  -> allTasks
	                 [  /*player @:*/ (viewInformation "The winner is:" [] winner >>= return)
	                 \\ player <- [me:friends]
	                 ]
	>>| return winner


invite_friends :: Task [User]
invite_friends
	=               enterSharedMultipleChoice "Select friends to play with" [] users
	>>= \friends -> if (not (isMember (length friends) [1..3]))
	                   (viewInformation "Oops" [] "number of friends must be 1, 2, or 3" >>| invite_friends)
	                   (return friends)


game :: Int (Color,User) -> Task (Color,User)
game nr_of_players (color,user)
	=           get randomInt
	  >>= \r -> let player = initial_player nr_of_players color (abs r) in
	            set player (player_state color)
	  >>|       updateSharedInformation (toString user) [imageViewUpdate id (game_image True) (flip const)] (player_middle_state color)
	  >>*		[OnValue (player_wins (color,user))]

player_wins :: (Color,User) (TaskValue (Player,Middle)) -> Maybe (Task (Color,User))
player_wins (color,user) (Value (player,middle) _)
| isEmpty player.ligretto	= Just (return (color,user))
| otherwise					= Nothing
player_wins _ _				= Nothing

play_concealed_pile :: Player -> Player
play_concealed_pile player
| isEmpty player.hand.conceal
							= shuffle_hand (sum [1,length player.ligretto,length player.hand.discard]) player	// ISSUE: random value should be obtained from randomInt SDS
| otherwise					= swap_discards player

play_hand :: (Player,Middle) -> (Player,Middle)
play_hand (player,middle)
| isNothing maybe_card		= (player,middle)
| isEmpty matching_piles	= (player,middle)
# (pilenr,pile)				= hd matching_piles
| otherwise					= (remove_top_of_discard player,updateAt pilenr [card:pile] middle)
where
	maybe_card				= top_discard player
	card					= fromJust maybe_card
	matching_piles			= [(pilenr,pile) \\ pile <- middle & pilenr <- [0..] | card_matches_top_of_pile card pile]

play_row_card :: Card Int (Player,Middle) -> (Player,Middle)
play_row_card card cardnr (player,middle)
| isEmpty matching_piles	= (player,middle)
# (pilenr,pile)				= hd matching_piles
| otherwise					= (move_ligretto_card_to_row cardnr player,updateAt pilenr [card:pile] middle)
where
	matching_piles			= [(pilenr,pile) \\ pile <- middle & pilenr <- [0..] | card_matches_top_of_pile card pile]


middle_state :: Shared Middle
middle_state				= sharedStore "middle" (repeatn 16 [])

player_state :: Color -> Shared Player
player_state color			= sharedStore ("player " <+++ color) {color=color,row=[],ligretto=[],hand={conceal=[],discard=[]}}

player_middle_state :: Color -> Shared (Player,Middle)
player_middle_state color	= player_state color >+< middle_state


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

hand_image :: Bool Hand -> Image (Player,Middle)
hand_image interactive {conceal,discard}
							= beside [] [] (if interactive [conceal_pile <@< {onclick = app2 (play_concealed_pile,id)}
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

row_image :: Bool RowPlayer -> Image (Player,Middle)
row_image interactive row	= beside [] [] 
							         [ let card = card_image Front row_card in
							            if interactive (card <@< {onclick = play_row_card row_card cardnr}) card
							         \\ row_card <- row 
							          & cardnr   <- [1..]
							         ]  Nothing

game_image :: Bool (Player,Middle) -> Image (Player,Middle)
game_image interactive (player,middle)
							= above [AtMiddleX,AtMiddleX] [] [middle_image middle,player_image interactive player] Nothing

player_image :: Bool Player -> Image (Player,Middle)
player_image interactive player
							= beside [] [] [ row_image interactive player.row, empty (px w) zero
							               , pile_image Front player.ligretto, empty (px w) zero		// BUG: only margin (px zero,px w) around pile_image does not work
							               , hand_image interactive player.hand
							               ] Nothing
where
	(w,_)					= card_size

middle_image :: Middle -> Image m
middle_image middle			= grid (Rows 2) (LeftToRight,TopToBottom) [] [] (map (pile_image Front) middle) Nothing

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
