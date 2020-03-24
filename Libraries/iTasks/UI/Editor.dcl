definition module iTasks.UI.Editor
/**
* This module defines the interfaces for task editors used in the interact task
* the interact core task uses these editors to generate and update the user interface
*/

from ABC.Interpreter import :: PrelinkedInterpretationEnvironment
from ABC.Interpreter.JavaScript import :: JSWorld, :: JSVal
from iTasks.UI.Definition import :: UI, :: UIAttributes, :: UIChange, :: UIAttributeChange, :: TaskId

from iTasks.Internal.IWorld import :: IWorld
from iTasks.Internal.Generic.Defaults import generic gDefault
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Map import :: Map
from Data.Error import :: MaybeError, :: MaybeErrorString
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from Data.GenEq import generic gEq
from StdOverloaded import class toString
from Control.GenBimap import generic bimap

/**
* Definition of an editor.
* EditMode and Maybe arguments/results are unique to allow deriving bimap for Editor type.
*
* @var r The type of data that the editor reads (when displaying or updating)
* @var w The type of data that the editor writes back after an event (when entering or updating)
*
*        For many editors, including the generic editor, w = Maybe r because while editing a value 
*        it can become temporary unavailable which is indicated by writing Nothing.
*
*        Some editors are guaranteed to always have a value (e.g. a slider of type Int).
*        In those cases w = r.
* 
*        Additionally to reduce communication, editors can use encoded partial values for reading
*        and writing.
*/
:: Editor r w =
	//Generating the initial UI
	{ genUI :: !UIAttributes DataPath *(EditMode r) *VSt ->
		*(*MaybeErrorString *(!UI, !EditState, !*Maybe w), *VSt)
	//React to edit events
	, onEdit :: !DataPath (!DataPath, !JSONNode) EditState *VSt ->
		*(*MaybeErrorString *(!UIChange, !EditState, !*Maybe w), *VSt)
	//React to a new model value
	, onRefresh :: !DataPath r EditState *VSt ->
		*(*MaybeErrorString *(!UIChange, !EditState, !*Maybe w), *VSt)
	//Get the typed 'read' value from the editor state, if the state represents a valid value
	, valueFromState :: !EditState -> *Maybe r
	}

/*
*	Definition of a leaf editor using a typed state and edit event.
*	This is an auxiliary type to define an `Editor` with an untyped state and edit events.
*
*   @var edit The type of the edit events
*   @var st   The type of the typed internal state the editor
*   @var r    Read type (see {{Editor}})
*   @var w    Write type (see {{Editor}})
*/
:: LeafEditor edit st r w =
	//Generating the initial UI
	{ genUI :: !UIAttributes DataPath (EditMode r) *VSt ->
		*(*MaybeErrorString *(!UI, !st, !*Maybe w), *VSt)
	//React to edit events
	, onEdit :: !DataPath (!DataPath, !edit) st *VSt ->
		*(*MaybeErrorString *(!UIChange, !st, !*Maybe w), *VSt)
	//React to a new model value
	, onRefresh :: !DataPath r st *VSt ->
		*(*MaybeErrorString *(!UIChange, !st, !*Maybe w), *VSt)
	//Get the typed value from the editor state, if the state represents a valid value
	, valueFromState :: !st -> Maybe r
	}

leafEditorToEditor :: !(LeafEditor edit st r w) -> Editor r w | JSONEncode{|*|}, JSONDecode{|*|} st & JSONDecode{|*|} edit

//Version without overloading, for use in generic case
//The first two argument should be JSONEncode{|*|} and JSONDecode{|*|} which cannot be used by overloading within generic functions
leafEditorToEditor_ :: !(Bool st -> [JSONNode]) !(Bool [JSONNode] -> (Maybe st, [JSONNode])) !(LeafEditor edit st r w)
                    -> Editor r w | JSONDecode{|*|} edit

/*
*	Definition of a compound editor using an additional typed state, next to the children's states.
*	This is an auxiliary type to define an `Editor` with an untyped state.
*	The function work on the typed additional state and the untyped children's states.
*
*   @var st   The type of the typed internal state the compound editor
*   @var r    Read type (see {{Editor}})
*   @var w    Write type (see {{Editor}})
*/
:: CompoundEditor st r w =
	//Generating the initial UI
	{ genUI :: !UIAttributes DataPath (EditMode r) *VSt ->
		*(MaybeErrorString (!UI, !st, ![EditState], !Maybe w), *VSt)
	//React to edit events
	, onEdit :: !DataPath (!DataPath, !JSONNode) st [EditState] *VSt ->
		*(MaybeErrorString (!UIChange, !st, ![EditState], !Maybe w), *VSt)
	//React to a new model value
	, onRefresh :: !DataPath r st [EditState] *VSt ->
		*(MaybeErrorString (!UIChange, !st, ![EditState], !Maybe w), *VSt)
	//Get the typed value from the editor state, if the state represents a valid value
	, valueFromState :: !st [EditState] -> Maybe r
	}

compoundEditorToEditor :: !(CompoundEditor st r w) -> Editor r w | JSONDecode{|*|}, JSONEncode{|*|} st

/*
*	Definition of an editor modifier using an additional typed state, next to the child state.
*	Modifiers without additional state can be directly defined in terms of the `Editor` type.
*	This is an auxiliary type to define an `Editor` with an untyped state.
*	The function work on the typed additional state and the untyped child state.
*
*   @var st   The type of the typed additional state of the wrapped editor
*   @var r    Read type (see {{Editor}})
*   @var w    Write type (see {{Editor}})
*/
:: EditorModifierWithState st r w =
	//Generating the initial UI
	{ genUI :: !UIAttributes DataPath (EditMode r) *VSt ->
		*(MaybeErrorString (!UI, !st, !EditState, !Maybe w), *VSt)
	//React to edit events
	, onEdit :: !DataPath (!DataPath, !JSONNode) st EditState *VSt ->
		*(MaybeErrorString (!UIChange, !st, !EditState, !Maybe w), *VSt)
	//React to a new model value
	, onRefresh :: !DataPath r st EditState *VSt ->
		*(MaybeErrorString (!UIChange, !st, !EditState, !Maybe w), *VSt)
	//Get the typed value from the editor state, if the state represents a valid value
	, valueFromState :: !st EditState -> Maybe r
	}

editorModifierWithStateToEditor :: !(EditorModifierWithState st r w) -> Editor r w | JSONDecode{|*|}, JSONEncode{|*|} st

//* Datapaths identify sub structures in a composite structure
:: DataPath :== [Int]

:: EditMode a = Enter | Update !a | View !a

editModeValue :: !(EditMode a) -> Maybe a
mapEditMode :: .(.x -> .y) !(EditMode .x) -> EditMode .y
derive bimap EditMode

/** Edit masks contain information about a value as it is being edited in an interactive task.
*   During editing, values can be in an inconsistent, or even untypable state
*/  
:: EditState
	= LeafState      !LeafState             //* Edit state of single fields/controls
	| CompoundState  !JSONNode ![EditState] //* Compound structure of multiple editors with additional extra state
	| AnnotatedState !JSONNode !EditState   //* Edit state annotated with additional information, used for modifiers

:: LeafState =
	{ touched :: !Bool
	, state   :: !JSONNode //Usually contains the (serialized) value
	}

:: *VSt =
	{ taskId			:: !String   //* The id of the task the visualisation belongs to
	, optional			:: !Bool     //* Create optional form fields
	, selectedConsIndex	:: !Int      //* Index of the selected constructor in an OBJECT
	, pathInEditMode    :: [Bool]    //* Path of LEFT/RIGHT choices used when UI is generated in edit mode
	, abcInterpreterEnv :: !PrelinkedInterpretationEnvironment //* Used to serialize expressions for the client
	}

withVSt :: !TaskId !.(*VSt -> (a, *VSt)) !*IWorld -> (!a, !*IWorld)

derive JSONEncode EditState, LeafState, EditMode
derive JSONDecode EditState, LeafState, EditMode
derive gEq        EditState, LeafState

newLeafState :: EditState

//Generate the editorId string for a given datapath
editorId :: !DataPath -> String
s2dp     :: !String -> DataPath

isTouched :: !EditState -> Bool
isCompound :: !EditState -> Bool

//Add client-side initialization to the generation of an initial UI
withClientSideInit ::
	!(JSVal *JSWorld -> *JSWorld)
	!(UIAttributes DataPath a *VSt -> *(*MaybeErrorString (!UI, !st, !*Maybe w), *VSt))
	!UIAttributes !DataPath !a !*VSt ->
		*(!*MaybeErrorString (!UI, !st, !*Maybe w), !*VSt)
