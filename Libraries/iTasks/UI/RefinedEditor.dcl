definition module iTasks.UI.RefinedEditor

//TODO: Use (p r w) instead of v for external interface
//Tasks to handle side-effects just write commands to the share?

//Collaboration with a distributed message set and versions needs to be built into
//interact. The state and message queue will be shared between tasks instead of the editor value
CollabModel s :== (Versioned s, [Versioned EditorUpdate])
Versioned a :== (!Int,!a)

SDS Int (Either (Version s) [Version s])


//Definition of an editor
//Each editor defines a client and a server component with their own state.
//They communicate through a shared message type

:: [(String, c -> (c, Maybe msg))] -> EditorClient c msg

//v = Value the editor represents (print/parse to lower level)
//s = Raw server state, for example an unparsed string
//c = Client configuration, the initial setup of a client component
//m = Message type, used for communicating between client and server

:: Editor v s c m = 
	{ client :: EditorClient c m
	, server :: EditorServer v s c m
	}

:: EditorClient c m =
	{ init      :: c !JSObject *JSWorld -> *(!Maybe m, !*JSWorld)
	, destroy   :: !JSObject *JSWorld -> *JSWorld

	//Something happened in the javascript/DOM world
	, onEvent   :: !String !JSObject !JSObject !*JSWorld -> (!Maybe m, !*JSWorld)
	//A message from the server arrived
	, onMessage :: m !JSObject !*JSWorld -> *(!Maybe m, !*JSWorld)
	}

:: EditorServer v s c m = 
	{ configure :: s -> c //Derive the initial client configuration 

	, onRefresh :: s s -> (s, Maybe m)
	, onMessage :: m s -> (s, Maybe m)

	, value     :: ((EditorValue v) -> s, s -> EditorValue v) //How to get to/from the validated domain
	}

//Exposing editors valid/invalid/empty judgement
:: EditorValue a = ValidValue a | InvalidValue | EmptyValue

class EditorMessage m
where
	encodeEditorMessage :: m -> EditStateUpdate
	decodeEditorMessage :: EditStateUpdate -> m

class EditorState s
where
	encodeEditorState :: s -> EditState
	decodeEditorState :: EditState -> s

//Generic untyped edit state
:: EditState =
	{ attributes :: Map AttributeName AttributeValue
	, children   :: [EditState]
	}

:: AttributeName :== String
:: AttributeValue :== JSONNode

:: EditStateUpdate // (basically current UIChange type)
	= Replace EditState	
	| Change [AttributeUpdate] [ChildUpdate]
	| Nop 

:: AttributeUpdate = SetAttribute AttributeName AttributeValue | DelAttribute AttributeName
:: ChildUpdate = RemoveChild Int | InsertChild Int EditState | UpdateChild Int EditStateUpdate

:: ClientState :== EditState
:: ServerState :== EditState

//Editor composition
//- client side without communication
//- service

//Example: Dynamic [Int] editor built from textField, textView and button controls
// - Editor for each item in the list
// - Button to add a blank new item
// - Dynamic label that shows the number of items
// - Buttons with each item for moving i

listEditor :: (Editor a s c m) -> Editor [a] [s] [c] (Int,m)

label :: String -> Editor String () LabelDef
button :: (Bool -> ButtonDef) -> Editor Bool Bool ButtonDef Bool
textView :: Editor String TextViewDef TextViewDef String 
textField :: Editor String TextFieldDef TextFieldDef String

integerField :: Editor Int TextFieldDef TextFieldDef String

:: ButtonDef =
	{ clicked   :: Bool
	, label     :: Maybe String 
	, icon      :: Maybe String
	}
:: TextFieldDef =
	{ text    :: String
	, touched :: Bool
	}
:: TextViewDef =
	{ text :: String
	}
:: LabelDef =
	{ text :: String
	}

//Editor for numbers
//Editor for boolean (checkbox)
//Combination: for maybe number

//Compose by juxtaposition, no need to specify interdependency
glue :: (Editor v1 s1 c1 m1)
	(Editor v2 s2 c2 m2)
	->
	(Editor (EditorValue v1, EditorValue v2) (s1,s2) (c1,c2) (Maybe m1, Maybe m2))

//Define the dependencies by defining feedback on messages
link :: 
	((c1,c2) -> (c1,c2)) //Rewrite the initial client configuration
	((Maybe m1, Maybe m2) -> ((Maybe m1, Maybe m2),(Maybe m1, Maybe m2))) //Rewrite from server to client with feedback to server
	((Maybe m1, Maybe m2) -> ((Maybe m1, Maybe m2),(Maybe m1, Maybe m2))) //Rewrite from client to server with feedback to client
	(Editor v s (c1,c2) (Maybe m1, Maybe m2))
	->	
	(Editor v s (c1,c2) (Maybe m1, Maybe m2))

//Get rid of the tuples and combine the parts into a unified state, configuration and 
fuse :: 
	((c1,c2) -> c, c -> (c1,c2))                                 //Unify client configuration
	((s1,s2) -> s, s -> (s1,s2))                                 //Unify server state
	(m -> (Maybe m1, Maybe m2), (Maybe m1, Maybe m2) -> Maybe m) //Unify messages
	(EditorValue v -> (EditorValue v1, EditorValue v2), (EditorValue v1,EditorValue v2) -> EditorValue v) //Unity checked interface
	(Editor (EditorValue v1, EditorValue v2) (s1,s2) (c1,c2) (Maybe m1, Maybe m2))
	-> 
	(Editor v s c m)

//Example : Texfield with reset button
resettableField = fuseFst buttonconfig (link id serverlink clientlink (glue textField (button (const buttonconfig))))
where
  clientlink (_,Just True) = ((Just "",Nothing), (Just "",Just False)) //When button is clicked, reset both local and remote
  clientlink (mt,mb) = ((mt,mb),(Nothing,Nothing)

  serverlink m = (m,(Nothing,Nothing)) //No feedback serverside

  buttonconfig = {ButtonDef|clicked=False,label=Just "Reset",icon=Nothing}

  //Common pattern: expose first component
  fuseFst sndCfg editor = fuse clientconfig serverstate messages interface
  where
    clientconfig = (fst, \s -> (s,sndCfg))        //Reset button is statically configured
    serverstate  = (fst, \s -> (s,False))         //Only server state of the textfield is stored
    messages     = (\m -> (Just m,Nothing), fst)  //Drop messages for second part of the tuple
    interface    = (x -> (x,EmptyValue), fst)     //Only consider first part as editor

