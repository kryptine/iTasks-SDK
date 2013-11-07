definition module iTasks.API.Extensions.CodeMirror

import iTasks.API.Core.Client.Editlet

:: CodeMirrorClientSt = {
		  codeMirror 			:: !JSVal JSObject
		, systemEventHandlers	:: ![(!String, !JSVal (JSFunction JSObject))]
		}
 
:: CodeMirrorClient = {
		  val	:: CodeMirror
		, mbSt	:: Maybe CodeMirrorClientSt
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

// TODO: CodeMirror a
:: CodeMirror = {
		  configuration 	:: ![CodeMirrorConfiguration]
		, position			:: !Int // cursor position
		, selection 		:: !Maybe (Int,Int)
		, source			:: String // TODO: strictness kills graph_to_sapl_string here
		}

:: CodeMirrorDiff
		= SetOption !CodeMirrorConfiguration
		| SetPosition !Int
		| SetSelection !(Maybe (Int,Int))
		| SetValue !String // TODO

derive JSONEncode       CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive JSONDecode       CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gDefault         CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gEq              CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gVisualizeText   CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gEditor          CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gEditMeta        CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gUpdate          CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gVerify	        CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror

codeMirrorEditlet :: !CodeMirror
					 [(String, EditletEventHandlerFunc CodeMirrorClient)]
				  -> Editlet CodeMirror [CodeMirrorDiff]

