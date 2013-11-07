definition module iTasks.API.Core.Client.Component

import iTasks.API.Core.Client.Interface
import iTasks.Framework.UIDefinition

:: ComponentId :== String
:: ComponentEventName :== String
:: ComponentEvent idtype a = ComponentEvent !ComponentId !ComponentEventName (ComponentEventHandlerFunc idtype a)
:: ComponentEventHandlerFunc idtype a :== idtype (JSVal JSEvent) a *JSWorld -> *(!a, !*JSWorld)

:: ComponentHTML idtype st = 
	{ width 			:: !UISize
	, height			:: !UISize
	, html				:: !HtmlTag
	, eventHandlers		:: ![ComponentEvent idtype st] 
	} 
