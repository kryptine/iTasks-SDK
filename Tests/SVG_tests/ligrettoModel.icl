implementation module ligrettoModel

from   StdMisc import abort
import Math.Random		// for generating random numbers
import StdEnv
import Data.Maybe, GenEq
import iTasks.Framework.Generic.Visualization

derive gEq Card, SideUp, Color

no_of_cards_in_row :: !NrOfPlayers -> Int
no_of_cards_in_row 2 = 5
no_of_cards_in_row 3 = 4
no_of_cards_in_row 4 = 3
no_of_cards_in_row n = abort ("ligretto.no_of_cards_in_row: illegal integer argument (" +++ toString n +++ ").\n")

all_colors :: [Color]
all_colors = [Red,Green,Blue,Yellow]

colors :: !NrOfPlayers -> [Color]
colors no_of_players = take no_of_players all_colors

initial_player_cards :: !NrOfPlayers !Color -> Pile
initial_player_cards no_of_players back
	= [{back=back,front=color,no=no} \\ color <- all_colors, no <- [1..10]]

shuffle :: ![a] !Int -> [a]
shuffle xs seed
	= fst (unzip (sortBy (\(_,r1) (_,r2) -> (r1 < r2)) (zip2 xs (genRandInt (abs seed)))))

initial_player :: !NrOfPlayers !Color !Int -> Player
initial_player no_of_players back seed
	= { color = back, row = row, ligretto = ligretto, hand = { conceal = hand, discard = [] } }
where
	cards           = shuffle (initial_player_cards no_of_players back) seed
	(row,rest)      = splitAt (no_of_cards_in_row no_of_players) cards
	(ligretto,hand) = splitAt 10 rest

row_card :: !Int !Player -> Card
row_card row_no player=:{row}
| row_no <= 0 || row_no > length row
	= abort ("ligretto.row_card: illegal integer argument (" <+++ row_no <+++ ").\n")
| otherwise
	= row !! (row_no-1)

move_ligretto_card_to_row :: !Int !Player -> Player
move_ligretto_card_to_row row_no player=:{row,ligretto}
| row_no <= 0 || row_no > length row
	= abort ("ligretto.move_ligretto_card_to_row: illegal integer argument (" <+++ row_no <+++ ").\n")
| isEmpty ligretto
	= abort "ligretto.move_ligretto_card_to_row: trying to take card from empty ligretto.\n"
| otherwise
	= {player & row = updateAt (row_no-1) (hd ligretto) row, ligretto = tl ligretto}

top_discard :: !Player -> Maybe Card
top_discard {hand={discard}}
| isEmpty discard   = Nothing
| otherwise         = Just (hd discard)

shuffle_hand :: !Int !Player -> Player
shuffle_hand seed player=:{hand=hand=:{conceal,discard}}
| isEmpty conceal   = {player & hand = { hand & conceal = shuffle discard seed
                                              , discard = []
                      }                }
| otherwise         = abort ("ligretto.shuffle_hand: not allowed to shuffle non-empty concealed pile.\n")

remove_top_of_discard :: !Player -> Player
remove_top_of_discard player=:{hand=hand=:{conceal,discard}}
| isEmpty discard   = abort ("ligretto.remove_top_of_discard: no discarded card to pick.\n")
| otherwise         = {player & hand = { hand & discard = tl discard }}

swap_discards :: !Player -> Player
swap_discards player=:{hand=hand=:{conceal,discard}}
| isEmpty conceal   = abort ("ligretto:swap_discards: not allowed to take cards from an empty conceal pile.\n")
| otherwise         = { player & hand = { hand & conceal = rest
                                               , discard = reverse top3 ++ discard
                      }                 }
where
	(top3,rest)     = splitAt 3 conceal

card_matches_top_of_pile :: !Card !Pile -> Bool
card_matches_top_of_pile card pile
| isEmpty pile			= card.no == 1
| otherwise				= let top_card = hd pile in
						  card.front === top_card.front && card.no == top_card.no+1
