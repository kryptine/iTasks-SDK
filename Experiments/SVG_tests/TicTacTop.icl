module TicTacTop

import iTasks
import iTasks.API.Extensions.SVG.SVGlet

// Tic-Tac-Toe entities:
:: Game       = { board   :: Board
                , players :: Players
                , turn    :: TicTac
                }
:: Players    = { tic     :: User
                , tac     :: User 
                }
:: Name     :== String
:: Board    :== [[Tile]]
:: Tile     :== Maybe TicTac
:: TicTac     = Tic | Tac
:: Coordinate = { col :: Int, row :: Int }             

// Tic-Tac-Toe relations:
initial_game :: Players -> Game
initial_game players = { board   = repeatn 3 (repeatn 3 Nothing)
                       , players = players
                       , turn    = Tic
                       }

name_of :: TicTac Players -> Name
name_of Tic players = toString players.tic
name_of Tac players = toString players.tac

game_over :: Game -> Bool
game_over game = it_is_a_draw game || isJust (winner game)

it_is_a_draw :: Game -> Bool
it_is_a_draw game = all isJust (flatten game.board) && isNothing (winner game)

winner :: Game -> Maybe TicTac
winner game
	= hd ([hd nodups \\ candidate <- rows ++ columns ++ diags
	                  , nodups    <- [removeDup candidate]
	                  | length nodups == 1 && isJust (hd nodups)
	      ] ++ [Nothing])
where
	rows	= game.board
	columns	= [[ game.board !! y !! x \\ y <- [0..2]] \\ x <- [0..2]]
	diags   = [[row !! i \\ row <- game.board & i <- [0..]], [row !! (2-i) \\ row <- game.board & i <- [0..]]]

move :: Coordinate Game -> Game
move new game
	= {game & board = updateAt new.row (updateAt new.col (Just game.turn) (game.board!!new.row)) game.board, turn = ~game.turn}

// Tic-Tac-Toe coordination:
shared_game :: Players -> Shared Game
shared_game players
	= sharedStore ("TicTacTop: " <+++ name_of Tic players <+++ " vs " <+++ name_of Tac players) game
where
	game = initial_game players

find_players :: Task Players
find_players
	=           get currentUser
    >>= \me  -> updateChoiceWithShared "Find player" [] users me
    >>= \you -> return {tic=me,tac=you}

play_game :: TicTac Players -> Task Game
play_game who players
	= player @: (
	      updateSharedInformation (name_of who players) [imageUpdate toViewModel (render who) handleConflict fromViewModel] (shared_game players)
	  >>* [OnValue (ifValue game_over (accolades who))]
	  )
where
	player				= if (who === Tic) players.tic players.tac
	game				= initial_game players
	toViewModel			= id
	fromViewModel  _	= id
	handleConflict _ _	= Nothing

accolades :: TicTac Game -> Task Game
accolades me game
	= (case winner game of
	    Just wins    = if (wins == me)
	                      (viewInformation "Game over" [] "I won!")
	                      (viewInformation "Game over" [] "I lost!")
	    no_winner    = if (it_is_a_draw game)
	                      (viewInformation "No winner" [] "It is a draw")
	                      (viewInformation "Keep playing" [] "Game isn't over yet")
	  ) @! game

play_games :: Players -> Task ()
play_games players
	=   set (initial_game players) (shared_game players)
	>>| ((play_game Tic players -|| play_game Tac players)
	>>* [ OnAction (Action "/File/New"  [])          (always (play_games players))
        , OnAction (Action "/File/Quit" [ActionIcon "quit"]) (always (return ()))
        ])

play_tic_tac_toe :: Task ()
play_tic_tac_toe = find_players >>= play_games

// Tic-Tac-Toe rendering:
render :: TicTac Game *TagSource -> Image Game
render playing game tags
	= beside (repeat AtMiddleY) []
	     [ grid (Rows 3) (RowMajor,LeftToRight,TopToBottom) (repeat (AtMiddleX,AtMiddleY)) []
	          [  tuneIf (isNothing c && playing === game.turn)
	                    (cell c) {onclick = const (move {row=i,col=j}), local = False}
	          \\ row <- game.board 
	           & i   <- [0 ..]
	           , c   <- row
	           & j   <- [0 ..]
	          ] Nothing
	     , margin (px zero,px 10.0) (text (normalFontDef "Times New Roman" edge) (name_of game.turn game.players))
	     , margin (px zero,px 10.0) (cell (Just game.turn))
	     ] Nothing

cell :: (Maybe TicTac) -> Image Game
cell Nothing
	= rect (px edge) (px edge) <@< {fill = toSVGColor "white"}
cell (Just Tic)
	= circle (px (edge - pencil - 3.0)) <@< {fill = toSVGColor "none"} <@< {strokewidth = px pencil}
cell (Just Tac)
	= overlay (repeat (AtMiddleX,AtMiddleY)) [] 
	     [  line Nothing slash (px edge) (px edge) <@< {strokewidth = px pencil} 
	     \\ slash <- [Slash,Backslash]
	     ]  Nothing

edge   = 30.0
pencil = 4.0
font   = normalFontDef "Times New Roman" edge

// The iTask entry code:
import MultiUser	// for working with multiply registered users

Start :: *World -> *World
Start world
	= StartMultiUserTasks
	              [workflow "Tic-Tac-Top" "Play Tic-Tac-Toe" (play_tic_tac_toe                         <<@ FullScreen)]
	              [publish "/users" (WebApp []) (const (set_up_users                                   <<@ FullScreen))
	              ,publish "/show"  (WebApp []) (const (viewSharedInformation "Current users" [] users <<@ FullScreen))
	              ,publish "/find"  (WebApp []) (const (find_players                                   <<@ FullScreen))
	              ] world

derive class iTask Game, Players, TicTac
instance == TicTac where == t1 t2 = t1 === t2
instance ~  TicTac where ~ Tic = Tac
                         ~ Tac = Tic

// For TFP demonstration purposes only:
set_up_users :: Task ()
set_up_users =           get userAccounts
             >>= \all -> set (foldl (\xs x -> removeDup [x:xs]) all new) userAccounts
             >>| return ()
where
	new = [{UserAccount | credentials = { username = Username "peter", password = Password "peter" }
	                    , title       = Nothing
	                    , roles       = ["tic-tac-toe player"]
	       }
	      ,{UserAccount | credentials = { username = Username "pieter", password = Password "pieter" }
	                    , title       = Nothing
	                    , roles       = ["tic-tac-toe player"]
	       }
	      ,{UserAccount | credentials = { username = Username "jurrien", password = Password "jurrien" }
	                    , title       = Nothing
	                    , roles       = ["tic-tac-toe player"]
	       }
	      ]
instance == UserAccount where == u1 u2 = u1 === u2
