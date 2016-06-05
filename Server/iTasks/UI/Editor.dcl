definition module iTasks.UI.Editor
/**
* This module defines the interfaces for task editors used in the interact task
* the interact core task uses these editors to generate and update the user interface
*/

from iTasks.UI.Definition import :: UI, :: UIAttributes, :: UIChange, :: UIAttributeChange
from iTasks.UI.JS.Interface import :: JSWorld, :: JSObj, :: JSVal, :: JSObject

from iTasks._Framework.IWorld import :: IWorld
from iTasks._Framework.Generic.Defaults import generic gDefault
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Map import :: Map
from Data.Error import :: MaybeError, :: MaybeErrorString
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from GenEq import generic gEq

/*
*	Definition of an editor editor
*/
:: Editor a = 
	{ genUI  :: DataPath a *VSt                     -> *(!MaybeErrorString (!UI, !EditMask), !*VSt)           //Generating the initial UI
	, onEdit :: DataPath JSONNode a EditMask *VSt   -> *(!MaybeErrorString (!UIChange, !EditMask), !a, !*VSt) //React to edit events
	, updUI  :: DataPath a EditMask a EditMask *VSt -> *(!MaybeErrorString UIChange, !*VSt) 		          //React to a new model value 
	}

//* Datapaths identify sub structures in a composite structure
:: DataPath :== [Int]

:: EditMode = Enter | Update | View

/** Edit masks contain information about a value as it is being edited in an interactive task.
*   During editing, values can be in an inconsistent, or even untypable state
*/  
:: EditMask
	= FieldMask !FieldMask 		
	| CompoundMask ![EditMask]

:: FieldMask = 
	{ touched :: !Bool
	//, version :: !Int
	, valid   :: !Bool
	, state   :: !JSONNode
	}

:: Masked a :== (a,EditMask)

:: *VSt =
	{ taskId			:: !String           // The id of the task the visualisation belongs to
	, mode              :: !EditMode         // If we are entering, updating or viewing the data
	, optional			:: !Bool             // Create optional form fields
	, selectedConsIndex	:: !Int              // Index of the selected constructor in an OBJECT
	, iworld			:: !*IWorld	         // The iworld, used for example if external tools are needed to create editors
	}

derive JSONEncode EditMask, FieldMask
derive JSONDecode EditMask, FieldMask
derive gEq        EditMask, FieldMask

newFieldMask :: EditMask
newCompoundMask :: EditMask

//Generate the editorId string for a given datapath
editorId 				:: !DataPath 		-> String
s2dp					:: !String			-> DataPath

subMasks	:: !Int EditMask -> [EditMask]
toPairMask	:: !Int !EditMask -> EditMask
isTouched	:: !EditMask -> Bool

containsInvalidFields :: !EditMask -> Bool

//Utility functions making specializations of gEditor
checkMask			:: !EditMask a -> Maybe a
checkMaskValue      :: !EditMask a -> Maybe JSONNode | JSONEncode{|*|} a

stdAttributes 		:: String Bool EditMask -> UIAttributes
stdAttributeChanges :: String Bool EditMask EditMask -> [UIAttributeChange]

basicEdit :: !(upd a -> Maybe a) !DataPath !JSONNode !a !EditMask !*VSt -> *(!MaybeErrorString (!UIChange,!EditMask), !a, !*VSt) | JSONDecode{|*|} upd
basicEditSimple :: !DataPath !JSONNode !a !EditMask !*VSt -> *(!MaybeErrorString (!UIChange,!EditMask),!a,!*VSt) | JSONDecode{|*|} a

//****************************************************************************//
// Alternative wrapper type for defining custom editor components that can process events
// that are defined server-side but run client-side
//****************************************************************************//
:: Editlet a
  =
  { genUI   :: DataPath a *VSt -> *(!MaybeErrorString (!UI,!EditMask), !*VSt)
  , initUI  :: (JSObj ()) *JSWorld -> *JSWorld
  , onEdit  :: DataPath JSONNode a EditMask *VSt -> *(!MaybeErrorString (!UIChange,!EditMask), !a, !*VSt)   //React to edit events
  , updUI   :: DataPath a EditMask a EditMask *VSt -> *(!MaybeErrorString UIChange, !*VSt)
  }

fromEditlet :: (Editlet a) -> (Editor a) | JSONEncode{|*|} a & JSONDecode{|*|} a & gDefault{|*|} a
