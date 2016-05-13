definition module iTasks.UI.Editor
/**
* This module defines the interfaces for task editors used in the interact task
* the interact core task uses these editors to generate and update the user interface
*/

from iTasks.UI.Definition import :: UIAttributes, :: UIChange

import iTasks.UI.JS.Interface

from iTasks._Framework.IWorld import :: IWorld
from iTasks.API.Core.Types import :: DataPath, :: Verification, :: VerifiedValue
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Map import :: Map
from Data.Error import :: MaybeErrorString
from Text.JSON import :: JSONNode

/*
*	Standard editor
*/
:: Editor a = 
	{ genUI  :: DataPath a EditMask *VSt -> *(!MaybeErrorString UI,!*VSt)
	, updUI  :: DataPath a EditMask a EditMask *VSt -> *(!MaybeErrorString UIChange, !*VSt)
	, onEdit :: DataPath JSONNode a EditMask *USt -> *(!a, !EditMask, !*USt)
	}

/** Edit masks contain information about a value as it is being edited in an interactive task.
*   During editing, values can be in an inconsistent, or even untypable state
*/  
:: EditMask
	= Untouched								//The value has not been touched by the user
	| Touched								//The value has been touched by the user, now it makes sense to check the input
    | TouchedUnparsed !JSONNode              //The user has edited the value to something that cannot be parsed to a valid value
	| TouchedWithState !JSONNode			//Some components need to keep local state that can't be encoded in the value
	| Blanked								//The value was previously touched, but has been made blank again
	| CompoundMask ![EditMask]	    		//The value is a compound structure of which some parts are, and some aren't touched

:: Masked a :== (a,EditMask)

subMasks	:: !Int EditMask -> [EditMask]
toPairMask	:: !Int !EditMask -> EditMask
isTouched	:: !EditMask -> Bool

:: *VSt =
	{ selectedConsIndex	:: !Int              // Index of the selected constructor in an Object
	, optional			:: !Bool             // Create optional form fields
	, disabled			:: !Bool             // If true the editor is not editable
	, taskId			:: !String           // The id of the task the visualisation belongs to
	, iworld			:: !*IWorld	         // The iworld, used for example if external tools are needed to create editors
	}

:: *USt =
    { taskId            :: !String
    , iworld            :: !*IWorld
    }

//****************************************************************************//
// Alternative wrapper type for defining custom editor components that can process events
// that are defined server-side but run client-side
//****************************************************************************//
:: Editlet a
  =
  { genUI   :: DataPath a EditMask *VSt -> *(!MaybeErrorString UI, !*VSt)
  , initUI  :: (JSObj ()) *JSWorld -> *JSWorld
  , updUI   :: DataPath a EditMask a EditMask *VSt -> *(!MaybeErrorString UIChange, !*VSt)
  , onEdit  :: DataPath JSONNode a EditMask *USt -> *(!a, !EditMask, !*USt)
  }

fromEditlet :: (Editlet a) -> (Editor a) | JSONEncode{|*|} a & JSONDecode{|*|} a & gDefault{|*|} a
