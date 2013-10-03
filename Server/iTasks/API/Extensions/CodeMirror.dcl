definition module iTasks.API.Extensions.CodeMirror

import iTasks.API.Core.Client.Editlet

:: CodeMirrorState = { 
		codeMirror :: JSVal JSObject 
		}

:: CodeMirrorConfiguration 
		= CMMode !String
		| CMTheme !String
		| CMIdenUnit !Int
		| CMSmartIdent !Bool
		| CMTabSize !Int
		| CMIndentWithTabs !Bool
		| CMElectricChars !Bool
		| CMRtlMoveVisually !Bool
		| CMKeyMap !String
		| CMLineWrapping !Bool
		| CMLineNumbers !Bool
		| CMFirstLineNumber !Int
		| CMReadOnly !Bool
		| CMShowCursorWhenSelecting !Bool
		| CMUndoDepth !Int
		| CMHistoryEventDelay !Int
		| CMTabindex !Int
		| CMAutofocus !Bool
		| CMDragDrop !Bool
		| CMCursorBlinkRate	!Int
		| CMCursorScrollMargin !Int
		| CMCursorHeight !Int
		| CMWorkTime !Int
		| CMWorkDelay !Int
		| CMPollInterval !Int
		| CMFlattenSpans !Int
		| CMMaxHighlightLength !Int
		| CMCrudeMeasuringFrom !Int
		| CMViewportMargin !Int

codeMirrorEditlet :: !String [(String, ComponentEventHandlerFunc String CodeMirrorState)] -> Editlet String String

