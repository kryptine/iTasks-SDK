module TicTacToe

import iTasks
import iTasks.API.Extensions.Admin.UserAdmin



Start :: *World -> *World
Start world = startTask [ workflow  "Tic-Tac-Toe" "Play tic-tac-toe"          tictactoe
						, workflow "Manage users"  "Manage system users..."   manageUsers
						, workflow "test" "test" test2
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

// showing the gameboard in Html 

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

// SVG variant
/*
tictactoe_for_1` :: !Bool !User !User !(Shared TicTacToe) -> Task User
tictactoe_for_1` my_turn me you sharedGameSt
	= play
where
	play = (updateImageState "Play:" [UpdateWith (\gameSt -> {}) (\gameSt _ -> gameSt)] sharedGameSt)
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

*/
import iTasks.API.Extensions.SVG.SVGlet



updateSharedImageState :: !d  !(s -> Image s) (s -> s) (Shared s) -> Task s | iTask s & descr d 
updateSharedImageState d toImage handleAction sharedState   
	= updateSharedInformation d [UpdateWith	(\s -> svgRenderer s toImage) (\_ (Editlet s _ _) -> handleAction s)] sharedState //@ (\(Editlet s` _ _) -> s`)


:: TicTacToe2
	= { board2  :: [[Maybe TicTac]]
	  , turn2   :: Bool               // player 1 is playing
	  , action2	:: Maybe (Int,Int) 
	  }

derive class iTask TicTacToe2

initTicTacToe2 
	= { board2 	= repeatn 3 (repeatn 3 Nothing)
	  , turn2  	= False
	  , action2	= Nothing
	  }

test2 :: Task (TicTacToe2,TicTacToe2)
test2 = withShared initTicTacToe2
			(\share ->  updateSharedImageState "test1" (mkboard False) handleAction share 
						-&&-
						updateSharedImageState "test2" (mkboard True)  handleAction share
			)

handleAction:: TicTacToe2 -> TicTacToe2
handleAction ttt=:{turn2,board2,action2 = Just(i,j)} = {ttt & turn2 	= not turn2
															, board2 	= updateBoard board2 i j (if turn2 Tic Tac)
															, action2 	= Nothing}
where
	updateBoard board2 i2 j2 tictac = [[if (i==i2&&j==j2) (Just tictac) cell \\ cell <- row & j <- [0..]]
									  \\ row <- board2 & i <- [0..]
									  ]
handleAction ttt  = ttt


test =  updateImageState "test1" initTicTacToe2 (mkboard True) 

mkboard :: Bool TicTacToe2 -> Image TicTacToe2
mkboard turn ttt=:{board2,turn2} 
			= overlay 	[(AtLeft,AtTop),(AtMiddleX,AtTop),(AtRight,AtTop)
						,(AtLeft,AtMiddleY),(AtMiddleX,AtMiddleY),(AtRight,AtMiddleY)
						,(AtLeft,AtBottom),(AtMiddleX,AtBottom),(AtRight,AtBottom)
						,(AtLeft,AtTop)
						] 
						[(PxSpan 0.0,PxSpan 0.0), (PxSpan 30.0,PxSpan 0.0),(PxSpan 60.0,PxSpan 0.0)
						,(PxSpan 0.0,PxSpan 30.0),(PxSpan 30.0,PxSpan 30.0),(PxSpan 60.0,PxSpan 30.0)
						,(PxSpan 0.0,PxSpan 60.0),(PxSpan 30.0,PxSpan 60.0),(PxSpan 60.0,PxSpan 60.0)
						,(PxSpan 0.0,PxSpan 0.0)
						]
						([ mkTile i j (turn == turn2) cell \\ row <- board2 & i <- [0..], cell <- row & j <- [0..] ]
						++ 
						[field])
						Nothing 

mkTile i j _ (Just Tic)   = cross
mkTile i j _ (Just Tac)   = null
mkTile i j False Nothing  = blanc
mkTile i j True Nothing   = blanc <@< {onclick = \st -> {st & action2 = Just (i,j)}}

cross = overlay [] []
                 [ polyline Nothing  [(px 0.0, px 0.0),(px 30.0, px 30.0)] <@< {stroke = SVGColorText "red" }
                 , polyline Nothing  [(px 30.0, px 0.0),(px 0.0, px 30.0)] <@< {stroke = SVGColorText "red" }
                 ] Nothing
null  = circle (PxSpan 30.0) <@< {fill 			= SVGColorText "lightgrey"}
						     <@< {stroke    	= toSVGColor "green"}
						     <@< {strokewidth 	= px 1.0 }
blanc = empty (PxSpan 0.0) (PxSpan 30.0)

field = overlay [] [] 
				[ polyline Nothing  [(px 30.0, px 0.0),(px 30.0, px 90.0)] <@< {stroke = SVGColorText "blue" }
               	, polyline Nothing  [(px 60.0, px 0.0),(px 60.0, px 90.0)] <@< {stroke = SVGColorText "blue" }
				, polyline Nothing  [(px 0.0, px 30.0),(px 90.0, px 30.0)] <@< {stroke = SVGColorText "blue" }
               	, polyline Nothing  [(px 0.0, px 60.0),(px 90.0, px 60.0)] <@< {stroke = SVGColorText "blue" }
               	] Nothing	

