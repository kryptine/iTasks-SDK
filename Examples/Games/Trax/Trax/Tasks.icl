implementation module Trax.Tasks

import iTasks
import Trax.UoD
import Trax.UI

play_trax :: Task User
play_trax
	=             get currentUser
	  >>= \me  -> Hint "Who do you want to play Trax with:" @>> enterChoiceWithShared [] users
	  >>= \you -> play_game me you {trax=zero,names=[me,you],turn=True,choice=Nothing}

play_game :: User User TraxSt -> Task User
play_game me you traxSt
	= withShared traxSt
	  (\share ->
	  	(me @: (  Hint (toString me  +++ " plays with red") @>> updateSharedInformation [updateTraxEditor True] share
                >>* [OnValue (ifValue game_over game_winner)])
        )
         -&&-
        (you @:(  Hint (toString you +++ " plays with white") @>> updateSharedInformation [updateTraxEditor False] share
                >>* [OnValue (ifValue game_over game_winner)])
        )
	  ) @ fst

game_winner :: TraxSt -> Task User
game_winner st=:{trax,turn,names=[me,you]}
  = (Hint "The winner is:" @>> viewInformation [] (toString winner))
      -&&-
    (Hint "Final board:" @>> viewInformation [viewTraxEditor] st)
  @ (const winner)
where
  winners			= loops trax ++ winning_lines trax
  prev_player_color	= if turn WhiteLine RedLine
  winner			= if (isMember prev_player_color (map fst winners)) (if turn you me) (if turn me you)
