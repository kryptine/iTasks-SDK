module Editlet

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import iTasks.API.Extensions.CodeMirror
import iTasks.API.Extensions.JointJS.JointJS

import StdDebug

:: TimeDelta = SetSec !Int | SetMin !Int | SetHour !Int
derive class iTask TimeDelta

buienLet :: Editlet String Void
buienLet = {Editlet|value="Buienradar",html=const (RawText html), updateUI = \_ _ a st w = (a,st,w), handlers = \_ -> [], genDiff = \_ _ -> Nothing, appDiff = \_ v -> v}
where
	html = "<a href=\"http://www.buienradar.nl\" target=\"_blank\"><img border=\"0\" src=\"http://m.buienradar.nl/\"></a>"

:: StringDelta = {newString :: String}
derive class iTask StringDelta

stringlet :: Editlet String [String]
stringlet = {Editlet|value = "Hello world",html = \cid -> TextareaTag [IdAttr cid] []
			,updateUI = onUpdate
			,handlers = \cid -> [ComponentEvent cid "keyup" onChange]
			,genDiff  = \o n -> if (o == n) Nothing (Just [n,n])
			,appDiff  = \n _ -> hd n
			}
where
	onUpdate :: ComponentId (Maybe [String]) String (Maybe Void) *JSWorld -> (!String, Maybe Void, !*JSWorld)
	onUpdate id _ val st world
		# world	= setDomAttr id "value" (toJSVal val) world
		= (val,st,world)
	
	onChange  :: ComponentId (JSVal EditletEvent) String (Maybe Void) *JSWorld -> (!String, Maybe Void, !*JSWorld)
	onChange id event val st world
		= let (val, w) = getDomAttr id "value" world in (jsValToString val, st, w)
		
timelet :: Time -> Editlet Time [TimeDelta]
timelet t =	{Editlet
				|value		= t
				,html		= \id -> RawText ("<div style=\"font-size: 24pt;\" id=\"" +++ id +++ "\"></div>")
				,updateUI	= onUpdate				
				,handlers	= \_ -> []
				,genDiff	= genDiff
				,appDiff	= appDiff
				}
where
	onUpdate ::  ComponentId (Maybe [TimeDelta]) Time (Maybe Void) *JSWorld -> (!Time, Maybe Void, !*JSWorld)
	onUpdate id _ val st world
		# world = setDomAttr id "innerHTML" (toJSVal (toString val)) world
		# world	= setDomAttr id "style.color" (toJSVal (colors !! (val.Time.sec rem (length colors)))) world
		= (val,st,world)
		
	colors = ["#f0f","#00f","#f00","#30f","#ff0","#66f"]
	
	genDiff :: Time Time -> Maybe [TimeDelta]
	genDiff t1 t2 = case (  (if (t1.Time.sec == t2.Time.sec) [] [SetSec t2.Time.sec])
						 ++ (if (t1.Time.min == t2.Time.min) [] [SetMin t2.Time.min])
						 ++ (if (t1.Time.hour == t2.Time.hour) [] [SetHour t2.Time.hour])
						 ) of [] = Nothing ; delta = Just delta

	appDiff :: [TimeDelta] Time -> Time
	appDiff [] t = t
	appDiff [SetSec s:d] t = appDiff d {Time|t & sec = s}
	appDiff [SetMin m:d] t = appDiff d {Time|t & min = m}
	appDiff [SetHour h:d] t = appDiff d {Time|t & hour = h}	


clocklet :: Time -> Editlet Time Time
clocklet t =	{Editlet
				|value		= t
				,html		= \id -> RawText ("<canvas height=\"100%\" id=\"" +++ id +++ "\" class=\"CoolClock\"></canvas>")
				,updateUI	= onInit					
				,handlers	= \_ -> []
				,genDiff	= \t1 t2 -> if (t1 == t2) Nothing (Just t2)
				,appDiff	= \tn to -> tn
				}
where
	onInit :: ComponentId (Maybe Time) Time (Maybe Void) *JSWorld -> (!Time, Maybe Void, !*JSWorld)
	onInit id Nothing val st world
		# world				= addJSFromUrl "/coolclock.js" Nothing world
		# world				= addJSFromUrl "/moreskins.js" Nothing world
		= trace_n "onInit done" (val,st,world)

	// Update
	onInit id mbDiff val st world = (val, st, world)
	
	onLoad :: *JSWorld -> *JSWorld
	onLoad world
		# world	= trace_n "onLoad" world
		# (window,world)	= jsWindow world
		# (coolclock,world)	= jsGetObjectAttr "CoolClock" window world
		//# (coolclock,world)	= findObject "CoolClock" world
		# (err,world)		= jsIsUndefined coolclock world
		| err = trace_n "No CoolClock" world
		# (method,world)	= jsGetObjectAttr "findAndCreateClocks" coolclock world
		# (err,world)		= jsIsUndefined method world
		| err = trace_n "No findAndCreateClocks" world
		# (_,world)			= callObjectMethod "findAndCreateClocks" [] coolclock world
		= world
/*		
	onUpdate :: ComponentId (JSPtr JSObject) Time *JSWorld -> (!Time, !*JSWorld)
	onUpdate id event val=:{Time|hour,min,sec} world
		# (coolclock,world)	= findObject "CoolClock" world
		# (err,world)		= jsIsUndefined coolclock world
		# (method,world)	= jsGetObjectAttr "findAndCreateClocks" coolclock world
		# (err,world)		= jsIsUndefined method world
		# (_,world)			= jsCallObjectMethod "findAndCreateClocks" [] coolclock world			
		# (config,world)	= jsGetObjectAttr "config" coolclock world
		# (err,world)		= jsIsUndefined config world		
		# (tracker,world)	= jsGetObjectAttr "clockTracker" config world
		# (err,world)		= jsIsUndefined tracker world
		# (myclock,world)	= jsGetObjectAttr id tracker world
		# (err,world)		= jsIsUndefined myclock world	
		# (_,world)			= jsCallObjectMethod "setTime" [hour,min,sec] myclock world
		= (val,world)
*/

:: Game        = { board :: !TicTacToe     // the current board
                 , names :: !Players       // the current two players
                 , turn  :: !TicTac        // the player at turn
                 }
:: Players     = { tic   :: !Name          // the tic player is starting
                 , tac   :: !Name          // the tac player
                 }
:: Name      :== String
:: TicTacToe :== [[Tile]]
:: Tile        = Clear | Filled TicTac
:: TicTac      = Tic | Tac
:: Coordinate  = {col :: Int, row :: Int}  // 0 <= col <= 2 && 0 <= row <= 2

instance ~   TicTac     where ~  Tic     = Tac
                              ~  Tac     = Tic
derive class iTask Tile, TicTac, Coordinate

tictactoelet :: (TicTacToe,TicTac) -> Editlet (TicTacToe,TicTac) (TicTacToe,TicTac)
tictactoelet t=:(board,turn) =
	{Editlet
	|value		= t
	,html		= \id -> DivTag [IdAttr "tictactoe"] [init_board "tictactoe" t]
	,updateUI   = onUpdate
	,handlers	= \_ -> []
				  ++[ComponentEvent (cellId "tictactoe" c) "click" (onCellClick c) \\ c <- [{col=x,row=y} \\ x <- [0..2] & y <- [0..2] ]]
				 
	,genDiff	= \t1 t2 -> if (t1 === t2) Nothing (Just t2)
	,appDiff	= \tn to -> tn
	}
where
	//onInit :: ComponentId (JSPtr JSObject) (TicTacToe,TicTac) *JSWorld -> (!(TicTacToe,TicTac), !*JSWorld)
	//onInit editorId _ state world = (state,redraw "tictactoe" state world)

	onUpdate :: ComponentId (Maybe (TicTacToe,TicTac)) (TicTacToe,TicTac) (Maybe Void) *JSWorld -> (!(TicTacToe,TicTac), Maybe Void, !*JSWorld)
	onUpdate editorId _ state st world = (state,st,world) //(state,redraw "tictactoe" state world)

	onCellClick :: Coordinate ComponentId (JSVal EditletEvent) (TicTacToe,TicTac) (Maybe Void) *JSWorld -> (!(TicTacToe,TicTac), Maybe Void, !*JSWorld)
	onCellClick coord editorId event (board,turn) st world
		# state = (add_cell coord turn board, ~turn)
		= (state, st, redraw "tictactoe" state world)
		
	redraw	:: !String !(TicTacToe,TicTac) *JSWorld -> *JSWorld
	redraw editorId state world = setDomAttr editorId "innerHTML" (toJSVal (toString (init_board editorId state))) world
	
	init_board :: !String !(TicTacToe,TicTac) -> HtmlTag
	init_board editorId (board,turn)
		= TableTag [BorderAttr "0"] 
			           [ TrTag [] [  cell {col=x,row=y} \\ x <- [0..2] ] \\ y <- [0..2] ]
	where
		cell c	= case lookup1 c (tiles board) of
				    Filled t = TdTag [] [TileTag (64,64) t]
				    Clear    = TdTag [AlignAttr "center"] [ButtonTag [IdAttr (cellId editorId c)] [Text "Choose"]]

		TileTag (w,h) t	= ImgTag [ SrcAttr ("/" <+++ t <+++ ".png"), WidthAttr (toString w), HeightAttr (toString h) ]

	cellId editorId {col,row} = editorId <+++ "-" <+++ col <+++ row

	tiles :: !TicTacToe -> [(Coordinate,Tile)]
	tiles board
		= flatten [ [ ({col=x,row=y},cell)
		            \\ cell <- row & x <- [0..]
		            ]
		          \\ row <- board & y <- [0..]
		          ]
	add_cell :: !Coordinate !TicTac !TicTacToe -> TicTacToe
	add_cell new turn board
		= [ [  if (new === {col=x,row=y}) (Filled turn) cell
		    \\ cell <- row & x <- [0..]
		    ]
		  \\ row <- board & y <- [0..]
		  ]
		  
	lookup1 key table = hd [v \\ (k,v) <- table | k === key]


empty_board :: TicTacToe
empty_board = repeatn 3 (repeatn 3 Clear)

defcm = { configuration = [CMMode "javascript", CMLineNumbers True]
		, position = 0
		, selection = Nothing
        , source = "Buu"}
//test5 = updateInformation "CodeMirror" [] (codeMirrorEditlet "buu")

test5 :: Task CodeMirror
test5 = withShared defcm (\defcm -> updateSharedInformation "CodeMirror Settings" [] defcm
																-|| 
								   updateSharedInformation "CodeMirror Editor" 
								   				[UpdateWith (\cm -> codeMirrorEditlet cm []) 
								   							(\_ editlet -> editlet.Editlet.value)] defcm )        


        
//test5 = updateInformation "CodeMirror" [] (codeMirrorEditlet gDefault{|*|} [])

test4 = updateInformation "Tic tac toe" [] (tictactoelet (empty_board,Tic))
	
test2 = updateInformation "Test" [] (timelet (fromString "13:00:00"))

test3 = viewSharedInformation "Clock2" [] (mapRead (\t -> (timelet t,clocklet t)) currentTime)
		//||- viewInformation (Title "Buienradar") [] buienLet
		//<<@ AfterLayout (uiDefSetDirection Horizontal)
		
//test = viewSharedInformation "Clock" [ViewWith timeEditlet] currentTime

test = updateInformation "String" [] stringlet @ (\e -> e.Editlet.value) >&> viewSharedInformation "DEBUG" []

test6 = viewInformation "JointJS" [] (jointJSEditlet JointJS)

Start :: *World -> *World
Start world = startEngine test6 world

