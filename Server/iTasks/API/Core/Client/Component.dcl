definition module iTasks.API.Core.Client.Component

import iTasks.API.Core.Client.Interface
import iTasks.UI.Definition

:: ComponentId :== String
:: ComponentEventName :== String
:: ComponentEvent d a = ComponentEvent !ComponentId !ComponentEventName (ComponentEventHandlerFunc d a)
:: ComponentEventHandlerFunc d a
	:== ComponentId {JSObj JSEvent} a *JSWorld -> *(!a, !ComponentDiff d a, !*JSWorld)

:: Conflict :== Bool

:: ComponentDiff diff state = NoDiff | Diff diff 
                   (Conflict state *JSWorld -> *(state, ComponentDiff diff state, *JSWorld))
                   
:: ComponentHTML = 
	{ width 	:: !UISize
	, height	:: !UISize
	, html		:: !HtmlTag
	} 
