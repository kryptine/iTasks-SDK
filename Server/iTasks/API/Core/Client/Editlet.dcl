definition module iTasks.API.Core.Client.Editlet

import iTasks, iTasks.API.Core.Client.Interface

//****************************************************************************//
// Wrapper types for defining custom editor components that can process events
// that are defined server-side but run client-side
//****************************************************************************//

:: Editlet a d = E.st:
	{	value		:: a 
	,	html		:: ComponentId -> HtmlTag
	,	handlers	:: [ComponentEvent a st]
	//	Functions for efficient bidirectional synchronisation of the editlet value
	,	genDiff		:: a a -> Maybe d
	,	appDiff		:: d a -> a
	}

:: EditletEvent = EditletEvent

:: ComponentId :== String
:: ComponentEventName :== String
:: ComponentEvent a st = ComponentEvent !ComponentId !ComponentEventName (ComponentEventHandlerFunc a st)
:: ComponentEventHandlerFunc a st :== ComponentId (JSVal EditletEvent) a (Maybe st) *JSWorld -> *(!a,!Maybe st,!*JSWorld)

//createEditletEventHandler :: (ComponentEventHandlerFunc a st) !ComponentId -> (JSVal (JSFunction b)) 

derive JSONEncode		Editlet
derive JSONDecode		Editlet
derive gDefault			Editlet
derive gEq				Editlet
derive gVisualizeText	Editlet
derive gEditor	Editlet
derive gEditMeta			Editlet
derive gUpdate			Editlet
derive gVerify			Editlet



