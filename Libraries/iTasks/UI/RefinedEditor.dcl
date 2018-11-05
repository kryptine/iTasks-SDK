definition module iTasks.UI.RefinedEditor

//Collaboration with a distributed message set and versions needs to be built into
//interact. The state and message queue will be shared between tasks instead of the editor value
CollabModel s :== (Versioned s, [Versioned EditorUpdate])
Versioned a :== (!Int,!a)

SDS Int (Either (Version s) [Version s])

//Tasks to handle side-effects

//Definition of an editor
//Each editor defines a client and a server component with their own state.
//They communicate through a shared message type

:: [(String, c -> (c, Maybe msg))] -> EditorClient c msg

:: Editor v s c = E. msg:
	{ client :: EditorClient c msg
	, server :: EditorServer v s c msg
	}

:: EditorClient c msg =
	{ init      :: c !JSObject *JSWorld -> *(!Maybe msg, !*JSWorld)
	, destroy   :: !JSObject *JSWorld -> *JSWorld

	, onEvent   :: !String !JSObject !JSObject !*JSWorld -> (!Maybe msg, !*JSWorld)
	, onMessage :: msg !JSObject !*JSWorld -> *(!Maybe msg, !*JSWorld)

	, setState  :: c !JSObject !*JSWorld -> *(!Maybe msg, *JSWorld)
	, getState  :: !JSObject !*JSWorld -> *(!c, *JSWorld)
	}

:: EditorServer v s c msg = 
	{ init      :: s -> c //Derive the initial client state from the server state

	, onSet     :: s s -> (s, Maybe msg)
	, onMessage :: msg s -> (s, Maybe msg)

	, toValue   :: s -> EditorValue v 
	, fromValue :: (EditorValue v) -> s
	}

class EditorMessage m
where
	encodeEditorMessage :: m -> EditStateUpdate
	decodeEditorMessage :: EditStateUpdate -> m

class EditorState s
where
	encodeEditorState :: s -> EditState
	decodeEditorState :: EditState -> s

//Exposing editors valid/invalid/empty judgement
:: EditorValue a = ValidValue a | InvalidValue | EmptyValue

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

listEditor :: (Editor a s c) -> Editor [a] [s] [c]

label :: String -> Editor String () LabelDef
button :: String -> Editor Bool Bool ButtonDef
textView :: Editor String TextViewDef TextViewDef
textField :: Editor String TextFieldDef TextFieldDef

integerField :: Editor Int TextFieldDef TextFieldDef

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

//editor voor getal 
//editor voor boolean (checkbox)
//combinatie voor (maybe getal)
