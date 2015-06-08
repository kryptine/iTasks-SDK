definition module iTasks.API.Extensions.CodeMirror

import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Tasklet

:: JSCM

:: CodeMirrorClientSt = {
		  codeMirror 			:: !JSObj JSCM
		, systemEventHandlers	:: ![(!String, !JSFun JSCM)]
		, marks					:: ![JSObj JSCM]
		}
 
:: CodeMirrorClient = {
		  val	    :: CodeMirror
		, initQueue :: [CodeMirrorDiff]
		, mbSt	    :: Maybe CodeMirrorClientSt
		}

:: CodeMirrorPosition :== (Int, Int) // line, cursor position

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
		, position			:: !CodeMirrorPosition
		, selection 		:: !Maybe (CodeMirrorPosition,CodeMirrorPosition)
		, highlighted		:: ![(CodeMirrorPosition,CodeMirrorPosition)]
		, source			:: ![String] // TODO: strictness kills graph_to_sapl_string here
		}

:: CodeMirrorDiff
		= SetOption !CodeMirrorConfiguration
		| SetPosition !CodeMirrorPosition
		| SetSelection !(Maybe (CodeMirrorPosition,CodeMirrorPosition))
		| ReplaceRange !(Int,Int) [String] // number of valid lines from the left/right
		| SetHighlights ![(CodeMirrorPosition,CodeMirrorPosition)]

derive JSONEncode       CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive JSONDecode       CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gDefault         CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gEq              CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gText            CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gEditor          CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gEditMeta        CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gUpdate          CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gVerify	        CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror

codeMirrorEditlet :: !CodeMirror
					 ![(String, EditletEventHandlerFunc [CodeMirrorDiff] CodeMirrorClient)]
				  -> Editlet CodeMirror [CodeMirrorDiff]

