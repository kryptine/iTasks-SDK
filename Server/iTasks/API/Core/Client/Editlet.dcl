definition module iTasks.API.Core.Client.Editlet

import iTasks
import iTasks.API.Core.Client.Interface
import iTasks.API.Core.Client.Component

//****************************************************************************//
// Wrapper types for defining custom editor components that can process events
// that are defined server-side but run client-side
//****************************************************************************//

:: EditletEventHandlerFunc a 	:== ComponentEventHandlerFunc ComponentId a
:: EditletEvent a 				:== ComponentEvent ComponentId a
:: EditletHTML a 				:== ComponentHTML ComponentId a

:: Editlet sv d = E.cl: Editlet sv (EditletServerDef sv cl d) (EditletClientDef cl d)

:: EditletServerDef sv cl d =
	{	genUI		:: ComponentId *World -> *(!EditletHTML cl, !*World)
	,	defVal		:: sv
	,	genDiff		:: sv sv -> Maybe d
	,	appDiff		:: d  sv -> sv
	}

:: EditletClientDef cl d =
	{	updateUI    :: ComponentId (Maybe d) cl *JSWorld -> *(!cl, !*JSWorld)
	,	defVal		:: cl
	,	genDiff		:: cl cl -> Maybe d
	,	appDiff		:: d  cl -> cl
	}

:: EditletSimpl a d = EditletSimpl a (EditletSimplDef a d)

:: EditletSimplDef a d =
	{	genUI		:: ComponentId *World -> *(!EditletHTML a, !*World)
	,	updateUI    :: ComponentId (Maybe d) a *JSWorld -> *(!a, !*JSWorld)
	,	genDiff		:: a a -> Maybe d
	,	appDiff		:: d a -> a
	}

toEditlet :: (EditletSimpl a d) -> (Editlet a d) | iTask a

createEditletEventHandler :: (EditletEventHandlerFunc a) !ComponentId -> JSFun b

derive JSONEncode		Editlet
derive JSONDecode		Editlet
derive gDefault			Editlet
derive gEq				Editlet
derive gText	        Editlet
derive gEditor	        Editlet
derive gEditMeta		Editlet
derive gUpdate			Editlet
derive gVerify			Editlet

