implementation module iTasks.API.Extensions.CodeMirror

import StdMisc, StdString
import Data.Maybe
import iTasks.API.Core.Client.Editlet, iTasks.API.Core.Client.Interface

toAttrValue (CMMode a) 						= ("mode", toJSVal a)
toAttrValue (CMTheme a)						= ("theme", toJSVal a)
toAttrValue (CMIdenUnit a) 					= ("IdenUnit", toJSVal a)
toAttrValue (CMSmartIdent a) 				= ("SmartIdent", toJSVal a)
toAttrValue (CMTabSize a)					= ("TabSize", toJSVal a)
toAttrValue (CMIndentWithTabs a) 			= ("IndentWithTabs", toJSVal a)
toAttrValue (CMElectricChars a) 			= ("ElectricChars", toJSVal a)
toAttrValue (CMRtlMoveVisually a) 			= ("RtlMoveVisually", toJSVal a)
toAttrValue (CMKeyMap a) 					= ("KeyMap", toJSVal a)
toAttrValue (CMLineWrapping a) 				= ("LineWrapping", toJSVal a)
toAttrValue (CMLineNumbers a) 				= ("LineNumbers", toJSVal a)
toAttrValue (CMFirstLineNumber a) 			= ("FirstLineNumber", toJSVal a)
toAttrValue (CMReadOnly a) 					= ("ReadOnly", toJSVal a)
toAttrValue (CMShowCursorWhenSelecting a)	= ("ShowCursorWhenSelecting", toJSVal a)
toAttrValue (CMUndoDepth a) 				= ("UndoDepth", toJSVal a)
toAttrValue (CMHistoryEventDelay a) 		= ("HistoryEventDelay", toJSVal a)
toAttrValue (CMTabindex a) 					= ("Tabindex", toJSVal a)
toAttrValue (CMAutofocus a) 				= ("Autofocus", toJSVal a)
toAttrValue (CMDragDrop a) 					= ("DragDrop", toJSVal a)
toAttrValue (CMCursorBlinkRate a) 			= ("CursorBlinkRate", toJSVal a)
toAttrValue (CMCursorScrollMargin a) 		= ("CursorScrollMargin", toJSVal a)
toAttrValue (CMCursorHeight a) 				= ("CursorHeight", toJSVal a)
toAttrValue (CMWorkTime a) 					= ("WorkTime", toJSVal a)
toAttrValue (CMWorkDelay a) 				= ("WorkDelay", toJSVal a)
toAttrValue (CMPollInterval a) 				= ("PollInterval", toJSVal a)
toAttrValue (CMFlattenSpans a) 				= ("FlattenSpans", toJSVal a)
toAttrValue (CMMaxHighlightLength a) 		= ("MaxHighlightLength", toJSVal a)
toAttrValue (CMCrudeMeasuringFrom a)		= ("CrudeMeasuringFrom", toJSVal a)
toAttrValue (CMViewportMargin a) 			= ("ViewportMargin", toJSVal a)

createConfigurationObject :: [CodeMirrorConfiguration] !*JSWorld -> *(!JSVal CodeMirrorConfiguration, !*JSWorld)
createConfigurationObject cs world
	# (obj, world) = jsEmptyObject world
	= (obj, foldl (set obj) world (map toAttrValue cs))
where
	set obj world (attr,value) = jsSetObjectAttr attr value obj world

codeMirrorEditlet :: !String [(String, ComponentEventHandlerFunc String CodeMirrorState)] -> Editlet String String
codeMirrorEditlet g eventhandlers = {Editlet
				|value		= g
				,html		= \id -> TextareaTag [IdAttr (sourcearea id), ColsAttr "20", RowsAttr "20", StyleAttr "display:none;"] []
				,updateUI   = onUpdate
				,handlers	= \_ -> []
				,genDiff	= genDiff
				,appDiff	= appDiff
				}
where
	sourcearea id = "source_" +++ id
	
	// init
	onUpdate cid Nothing val Nothing world
		# (obj, world) = findObject "CodeMirror.defaults" world
		| not (jsIsUndefined obj)
		= onLoad cid undef val Nothing world
	
		# world = addJSFromUrl "codemirror.js" Nothing world
		# world = addJSFromUrl "mode/javascript/javascript.js" (Just handler) world
		# world = addCSSFromUrl "codemirror.css" world
		
		= (val, Nothing, world)
	where
		handler = createEditletEventHandler onLoad cid
	
	// update
	onUpdate cid (Just diff) val st world	
		= (val, st, world)		
		
	onLoad cid _ val Nothing world
		# (ta, world) = getDomElement (sourcearea cid) world
		# world = jsSetObjectAttr "value" (toJSVal val) ta world
		
		# (cm, world) = findObject "CodeMirror" world
		# (co, world) = createConfigurationObject [CMMode "javascript"] world 
		# (cm, world) = callObjectMethod "fromTextArea" [toJSArg ta, toJSArg co] cm world
		
		# world = foldl (putOnEventHandler cm) world eventhandlers
		
		= (val, Just {codeMirror = cm}, world)
	where
		putOnEventHandler cm world (event, handler)
			= snd (callObjectMethod "on" [toJSArg event, toJSArg (createEditletEventHandler handler cid)] cm world)
		
	genDiff val1 val2 = Just val2
	appDiff diff val = diff
	
	