implementation module Ligretto.Tasks

import iTasks
import Ligretto.UoD
import Ligretto.UI
import Data.Maybe
from Control.Monad import class Monad(bind), `b`

//	Task description of Ligretto:
play_Ligretto :: Task (!Color,!String)
play_Ligretto
	=            get currentUser
	>>- \me   -> invite_friends
	>>= \them -> let us = zip2 (colors (1+length them)) [me : them]
	              in allTasks (repeatn (length us) (get randomInt))
	>>= \rs   -> let gameSt = init_gameSt us rs
	              in withShared gameSt (play_game us)

invite_friends :: Task [User]
invite_friends
	=   Hint "Select 1, 2, or 3 friends to play with"  @>> enterMultipleChoiceWithShared [] users
	>>* [OnAction ActionContinue (withValue (\them -> if (isMember (length them) [1..3]) (Just (return them)) Nothing))]

play_game :: ![(Color,User)] !(Shared sds GameSt) -> Task (Color,String) | RWShared sds
play_game users game_st
	= anyTask [  (u,"Play Ligretto") @: play (c,toString u) game_st
              \\ (c,u) <- users
              ]

play :: !(!Color,!String) !(Shared sds GameSt) -> Task (Color,String) | RWShared sds
play (me,name) game_st
    =   Hint name @>> updateSharedInformation [ligrettoEditor me] game_st
    >>* [OnValue (withValue (\gameSt -> determine_winner gameSt
                            `b` \winner -> Just (accolades winner me game_st >>| return winner)))]

show_winner :: Color (Shared sds GameSt) GameSt -> Task (Color,String) | RWShared sds
show_winner me game_st gameSt
	= accolades winner me game_st >>| return winner
where
	{color,name} = fromJust (and_the_winner_is gameSt)
	winner		 = (color,name)

game_over :: !Color !(Shared sds GameSt) !GameSt -> Maybe (Task (Color,String)) | RWShared sds
game_over me game_st gameSt
  =                    and_the_winner_is gameSt
  `b` \{color,name} -> (let winner = (color,name)
					   in Just (accolades winner me game_st >>| return winner))

accolades :: !(!Color,!String) !Color !(Shared sds GameSt) -> Task GameSt | RWShared sds
accolades winner me game_st
	= Hint ("The winner is " <+++ winner) @>> updateSharedInformation [accoladesEditor me] game_st
