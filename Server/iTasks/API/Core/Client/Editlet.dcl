definition module iTasks.API.Core.Client.Editlet

import iTasks

//****************************************************************************//
// Wrapper types for defining custom editor components that can process events
// that are defined server-side but run client-side
//****************************************************************************//

:: Editlet a d =
	{	value		:: a 
	,	html		:: ComponentId -> HtmlTag
	,	handlers	:: [ComponentEvent a]
	//	Functions for efficient bidirectional synchronisation of the editlet value
	,	genDiff		:: a a -> Maybe d
	,	appDiff		:: d a -> a
	}

:: EditletEvent = EditletEvent

:: ComponentId :== String
:: ComponentEventName :== String
:: ComponentEvent a = ComponentEvent !ComponentId !ComponentEventName (ComponentEventHandlerFunc a)
:: ComponentEventHandlerFunc a :== ComponentId (JSVal EditletEvent) a *JSWorld -> *(!a,!*JSWorld)

derive JSONEncode		Editlet
derive JSONDecode		Editlet
derive gDefault			Editlet
derive gEq				Editlet
derive gVisualizeText	Editlet
derive gEditor	Editlet
derive gEditMeta			Editlet
derive gUpdate			Editlet
derive gVerify			Editlet



