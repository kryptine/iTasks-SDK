definition module iTasks.UI.Editor
/**
* This module defines the interfaces for task editors used in the interact task
* the interact core task uses these editors to generate and update the user interface
*/

from iTasks.UI.Layout import :: LayoutRules
from iTasks.UI.Definition import :: UIControl, :: UIAttributes

import iTasks.UI.Component
import iTasks.UI.JS.Interface

from iTasks._Framework.IWorld import :: IWorld
from iTasks.API.Core.Types import :: DataPath, :: Verification, :: VerifiedValue, :: InteractionMask
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Map import :: Map
from Text.JSON import :: JSONNode

/*
*	Standard editor
*/
:: Editor a = 
	{ genUI  	:: DataPath a InteractionMask Verification [EditMeta] *VSt -> *(!VisualizationResult,!*VSt)
	//, genDiff	:: DataPath a a -> [UIUpdate] //Eventually diffs can use the value and don't need the previous UI
	//, genDiff 	:: UIDef UIDef -> UIDiffResult
	, appDiff 	:: DataPath JSONNode a InteractionMask *USt -> *(!a, !InteractionMask, !*USt)
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

editorControls :: !VisualizationResult -> [(UIControl,UIAttributes)]

//****************************************************************************//
// Wrapper types for defining custom editor components that can process events
// that are defined server-side but run client-side
//****************************************************************************//

:: EditletEventHandlerFunc d a :== ComponentEventHandlerFunc d a
:: EditletEvent d a            :== ComponentEvent d a
:: EditletHTML                 :== ComponentHTML

:: Editlet sv d cl
  =
  { genUI      :: ComponentId sv *World -> *(EditletHTML, *World)
  , initClient :: sv ((EditletEventHandlerFunc d cl) ComponentId -> JSFun ()) ComponentId *JSWorld -> *(cl, *JSWorld)
  , appDiffClt :: ((EditletEventHandlerFunc d cl) ComponentId -> JSFun ()) ComponentId d cl *JSWorld -> *(cl, *JSWorld)
  , genDiffSrv :: sv sv -> Maybe d
  , appDiffSrv :: d  sv -> sv
  }

fromEditlet :: (Editlet a d cl) -> (Editor a) | JSONEncode{|*|} a & JSONDecode{|*|} a & gDefault{|*|} a & JSONDecode{|*|} d

