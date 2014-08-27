module TicTacToe

import iTasks
import iTasks.API.Extensions.Admin.UserAdmin



Start :: *World -> *World
Start world = startTask [ workflow  "Tic-Tac-Toe" "Play tic-tac-toe"          tictactoe
						, workflow "Manage users"  "Manage system users..."   manageUsers
						] world


startTask taskList world
	= startEngine [ publish "/" (WebApp []) (\_-> browseExamples taskList)
				  ] world
where
	browseExamples taskList = forever (
		 	enterInformation "Enter your credentials and login or press continue to remain anonymous" []
		>>* [OnAction (Action "Login" [ActionIcon "login",ActionKey (unmodified KEY_ENTER)]) (hasValue (browseAuthenticated taskList))
			] )
	
	browseAuthenticated taskList {Credentials|username,password}
		= authenticateUser username password
		>>= \mbUser -> case mbUser of
			Just user 	= workAs user (manageWorklist taskList)
			Nothing		= viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return Void
	
// tic-tac-toe, simplistic


:: TicTacToe
	= { board   :: ![[Maybe TicTac]]
	  , player1 :: !User
	  , player2 :: !User
	  , turn    :: !Bool               // player 1 is playing
	  }
:: TicTac
	= Tic | Tac

instance ~ Bool where ~ b = not b
derive class iTask TicTacToe, TicTac
instance == TicTac where == t1 t2 = t1 === t2

// ttt utility functions

emptyBoard :: [[Maybe TicTac]]
emptyBoard = repeatn 3 (repeatn 3 Nothing)

add_cell :: !(!Int,!Int) !Bool ![[Maybe TicTac]] -> [[Maybe TicTac]]
add_cell new turn board
	= [ [  if (new == (x,y)) (Just (if turn Tic Tac)) cell
	    \\ cell <- row & x <- [1..]
	    ]
	  \\ row <- board & y <- [1..]
	  ]

game_over :: TicTacToe -> Bool
game_over {board}
	= not (isEmpty [hd nodups \\ candidate <- rows ++ columns ++ diags
	                           , nodups    <- [removeDup candidate]
	                           | length nodups == 1 && isJust (hd nodups)
	               ])
where
	rows	= board
	columns	= [[ board !! x !! y \\ x <- [0..2]] \\ y <- [0..2]]
	diags   = [[row !! i \\ row <- board & i <- [0..]], [row !! (2-i) \\ row <- board & i <- [0..]]]


on_turn :: Bool TicTacToe -> Bool
on_turn my_turn gameSt=:{turn}
	= turn == my_turn

free_coordinates :: [[Maybe a]] -> [(Int,Int)]
free_coordinates board
	= map fst (filter (isNothing o snd) (flatten [[((x,y),cell) \\ cell <- row & x <- [1..]] \\ row <- board & y <- [1..]]))

// ttt game playing

tictactoe :: Task String
tictactoe
	=             get currentUser
	  >>= \me  -> enterChoiceWithShared "Who do you want to play Tic-Tac-Toe with:" [] users
	  >>= \you -> playGame me you {board=emptyBoard,player1=me,player2=you,turn=True}
where
	playGame me you board 
	  =				withShared  board
	                         (\sharedGameSt ->
	                         (        (tictactoe_for_1 False me you sharedGameSt)
	                                       -||-
	                          (you @: (tictactoe_for_1 True you me sharedGameSt))
	                         ))
	  >>|			playGame me you {board	& turn = not board.turn}	

tictactoe_for_1 :: !Bool !User !User !(Shared TicTacToe) -> Task User
tictactoe_for_1 my_turn me you sharedGameSt
	= (viewSharedInformation ("Turn = " <+++ if my_turn me you  <+++ ", Board =") [ViewWith (\gameSt -> viewBoard (42,42) gameSt)] sharedGameSt) ||- play
where
	play= (updateSharedInformation "Play:" [UpdateWith Hidden (\gameSt _ -> gameSt)] sharedGameSt)
	      >>* [ OnValue (ifValue game_over declare_winner)
              , OnValue (ifValue (on_turn my_turn)  make_a_move)
              ]

	declare_winner gameSt
		= 	let winner = if gameSt.turn gameSt.player2 gameSt.player1 
			in 		viewInformation "And the winner is: " [] (toString winner)
				>>|	return winner
	
	make_a_move gameSt=:{board,turn}
		=              enterChoice "Choose coordinate:" [] (free_coordinates board)
		  >>= \new  -> let board`  = add_cell new turn board
		                   gameSt` = {gameSt & board = board`
		                                     , turn = ~turn
		                             }
		                in set gameSt` sharedGameSt >>| play

// showing the gameboard in SVG 

viewBoard :: !(!Int,!Int) !TicTacToe -> HtmlTag
viewBoard dimensions ttt
	= TableTag [ BorderAttr "0" ] 
	           [ tr [ td [TileTag dimensions (case cell of
												Nothing  = "empty"
												Just Tic = "cross"
												Just Tac = "circle")
						 ]
					\\ cell <- row
					] 
			    \\ row <- ttt.board
			    ]

// shorthands for HTML:
tr		= TrTag []
td		= TdTag []
text x	= TdTag [AlignAttr "center"] [Text (toString x)]

TileTag :: !(!Int,!Int) !String -> HtmlTag
TileTag (width,height) tile
	= ImgTag [SrcAttr ("/"<+++ tile <+++ ".png"),w,h]
where
	(w,h) = (WidthAttr (toString width),HeightAttr (toString height))



