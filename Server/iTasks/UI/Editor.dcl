definition module iTasks.UI.Editor

from iTasks.UI.Layout import :: LayoutRules
from iTasks.UI.Definition import :: UIControl, :: UIAttributes

from iTasks._Framework.IWorld import :: IWorld
from iTasks.API.Core.Types import :: DataPath, :: Verification, :: VerifiedValue, :: InteractionMask
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Map import :: Map
from Text.JSON import :: JSONNode

/**
* This module defines the interface for task editors
* the interact core task uses these editors to generate and update the user interface
*/

:: Editor a = 
	{ //render :: DataPath (VerifiedValue a) [EditMeta] *VSt2 -> *(!VisualizationResult,!*VSt2)
	  //render :: DataPath (a,InteractionMask,Verification) [EditMeta] *VSt -> *(!VisualizationResult,!*VSt)
	  render :: DataPath a InteractionMask Verification [EditMeta] *VSt -> *(!VisualizationResult,!*VSt)
	, edit :: DataPath JSONNode a InteractionMask *USt -> *(!a, !InteractionMask, !*USt)
	}

:: *VSt =
	{ selectedConsIndex	:: !Int													// Index of the selected constructor in an Object
	, optional			:: !Bool												// Create optional form fields
	, disabled			:: !Bool												// If true the editor is not editable
	, taskId			:: !String												// The id of the task the visualisation belongs to
	, layout			:: !LayoutRules											// Layout rules for composite structures
	, iworld			:: !*IWorld												// The iworld, used for example if external tools are needed to create editors
	}

:: VisualizationResult
		= NormalEditor [(!UIControl,!UIAttributes)]
		| OptionalEditor [(!UIControl,!UIAttributes)]
		| HiddenEditor

:: EditMeta
	= { label	:: Maybe String
	  , hint	:: Maybe String
      , unit    :: Maybe (Either String String)
	  }

:: *USt =
    { taskId            :: !String
    , editorId          :: !String
    , iworld            :: !*IWorld
    }

