implementation module iTasks.API.Extensions.CodeMirror

import StdMisc, StdString
import Data.Maybe, Data.List
import iTasks.API.Core.Client.Editlet, iTasks.API.Core.Client.Interface

toAttrValue (CMMode a) 						= ("mode", toJSVal a)
toAttrValue (CMTheme a)						= ("theme", toJSVal a)
toAttrValue (CMIdenUnit a) 					= ("idenUnit", toJSVal a)
toAttrValue (CMSmartIdent a) 				= ("smartIdent", toJSVal a)
toAttrValue (CMTabSize a)					= ("tabSize", toJSVal a)
toAttrValue (CMIndentWithTabs a) 			= ("indentWithTabs", toJSVal a)
toAttrValue (CMElectricChars a) 			= ("electricChars", toJSVal a)
toAttrValue (CMRtlMoveVisually a) 			= ("rtlMoveVisually", toJSVal a)
toAttrValue (CMKeyMap a) 					= ("keyMap", toJSVal a)
toAttrValue (CMLineWrapping a) 				= ("lineWrapping", toJSVal a)
toAttrValue (CMLineNumbers a) 				= ("lineNumbers", toJSVal a)
toAttrValue (CMFirstLineNumber a) 			= ("firstLineNumber", toJSVal a)
toAttrValue (CMReadOnly a) 					= ("readOnly", toJSVal a)
toAttrValue (CMShowCursorWhenSelecting a)	= ("showCursorWhenSelecting", toJSVal a)
toAttrValue (CMUndoDepth a) 				= ("undoDepth", toJSVal a)
toAttrValue (CMHistoryEventDelay a) 		= ("historyEventDelay", toJSVal a)
toAttrValue (CMTabindex a) 					= ("tabindex", toJSVal a)
toAttrValue (CMAutofocus a) 				= ("autofocus", toJSVal a)
toAttrValue (CMDragDrop a) 					= ("dragDrop", toJSVal a)
toAttrValue (CMCursorBlinkRate a) 			= ("cursorBlinkRate", toJSVal a)
toAttrValue (CMCursorScrollMargin a) 		= ("cursorScrollMargin", toJSVal a)
toAttrValue (CMCursorHeight a) 				= ("cursorHeight", toJSVal a)
toAttrValue (CMWorkTime a) 					= ("workTime", toJSVal a)
toAttrValue (CMWorkDelay a) 				= ("workDelay", toJSVal a)
toAttrValue (CMPollInterval a) 				= ("pollInterval", toJSVal a)
toAttrValue (CMFlattenSpans a) 				= ("flattenSpans", toJSVal a)
toAttrValue (CMMaxHighlightLength a) 		= ("maxHighlightLength", toJSVal a)
toAttrValue (CMCrudeMeasuringFrom a)		= ("crudeMeasuringFrom", toJSVal a)
toAttrValue (CMViewportMargin a) 			= ("viewportMargin", toJSVal a)

shallowEq a b = fst (toAttrValue a) == fst (toAttrValue b)

createConfigurationObject :: [CodeMirrorConfiguration] !*JSWorld -> *(!JSVal CodeMirrorConfiguration, !*JSWorld)
createConfigurationObject cs world
	# (obj, world) = jsEmptyObject world
	= (obj, foldl (set obj) world (map toAttrValue cs))
where
	set obj world (attr,value) = jsSetObjectAttr attr value obj world

setOptions :: [CodeMirrorConfiguration] (JSVal JSObject) !*JSWorld -> *JSWorld
setOptions cs cm world
	# world = foldl upd world (map toAttrValue cs)
	= loadModulesIfNeeded cs cm world
where
	upd world (attr, val) = snd (callObjectMethod "setOption" [toJSArg attr, toJSArg val] cm world)

loadModulesIfNeeded :: [CodeMirrorConfiguration] (JSVal JSObject) !*JSWorld -> *JSWorld
loadModulesIfNeeded cs cm world
	# (cmobj, world) = findObject "CodeMirror" world

	// Load mode
	# world = case find isSetMode cs of
			Nothing 				= world
			(Just (CMMode mode)) 	= snd (callObjectMethod "autoLoadMode" [toJSArg cm, toJSArg mode] cmobj world)

	// Load theme
	= case find isSetTheme cs of
			Nothing 				= world
			(Just (CMTheme theme)) 	= addCSSFromUrl ("theme/"+++theme+++".css") world

where
	isSetMode (CMMode _) = True
	isSetMode _ = False

	isSetTheme (CMTheme _) = True
	isSetTheme _ = False

codeMirrorEditlet :: !CodeMirror
					 [(String, ComponentEventHandlerFunc CodeMirror CodeMirrorState)] 
				  -> Editlet CodeMirror [CodeMirrorDiff]
			  
codeMirrorEditlet g eventhandlers = Editlet g
				{html		= \id -> TextareaTag [IdAttr (sourcearea id), ColsAttr "20", RowsAttr "20", StyleAttr "display:none;"] []
				,updateUI   = onUpdate
				,handlers	= \_ -> []
				,genDiff	= genDiff
				,appDiff	= appDiff
				}
where
	sourcearea id = "cm_source_" +++ id
	
	// init
	onUpdate cid Nothing val Nothing world
		# (obj, world) = findObject "CodeMirror.defaults" world
		| not (jsIsUndefined obj)
		= onLoad cid undef val Nothing world
	
		# world = addJSFromUrl "codemirror.js" Nothing world
		# world = addJSFromUrl "addon/mode/loadmode.js" (Just handler) world
		# world = addCSSFromUrl "codemirror.css" world
		
		= (val, Nothing, world)
	where
		handler = createEditletEventHandler onLoad cid
	
	// update
	onUpdate cid (Just diffs) val (Just st=:{codeMirror}) world	
		// disable system event handlers
		# world = manageSystemEvents "off" st world		
	
		# world = setOptions opts codeMirror world
		# world = loadModulesIfNeeded opts codeMirror world
		
		# (cmdoc, world) = callObjectMethod "getDoc" [] codeMirror world

		# world = case find isSetPos nopts of
			Nothing    	= world
			(Just (SetPosition idx)) 	
						# (pos, world) = posFromIndex idx cmdoc world 
						= snd (callObjectMethod "setCursor" [toJSArg pos] cmdoc world)

		# world = case find isSetSel nopts of
			Nothing    		= world
			(Just (SetSelection Nothing))
						// Clear the selection
						# (pos, world) = callObjectMethod "getCursor" [] cmdoc world
						= snd (callObjectMethod "setSelection" [toJSArg pos, toJSArg pos] cmdoc world)
			(Just (SetSelection (Just (idx1,idx2)))) 	
						# (pos1, world) = posFromIndex idx1 cmdoc world 
						# (pos2, world) = posFromIndex idx2 cmdoc world 							
						= snd (callObjectMethod "setSelection" [toJSArg pos1, toJSArg pos2] cmdoc world)

		# world = case find isSetVal nopts of
			Nothing    	= world
			(Just (SetValue str)) 	
						= snd (callObjectMethod "setValue" [toJSArg str] cmdoc world)

		// enable system event handlers
		# world = manageSystemEvents "on" st world
					
		= (val, Just st, world)
	where
		(opts`, nopts) = splitWith isSetOpt diffs
		opts = map (\(SetOption opt) -> opt) opts`
	
		isSetPos (SetPosition _) = True
		isSetPos _ = False

		isSetSel (SetSelection _) = True
		isSetSel _ = False

		isSetOpt (SetOption _) = True
		isSetOpt _ = False

		isSetVal (SetValue _) = True
		isSetVal _ = False

		posFromIndex idx cmdoc world = callObjectMethod "posFromIndex" [toJSArg idx] cmdoc world
	
	onLoad cid _ val=:{source,configuration} Nothing world
		# (ta, world) = getDomElement (sourcearea cid) world
		# world = jsSetObjectAttr "value" (toJSVal source) ta world
		
		# (cmobj, world) = findObject "CodeMirror" world
		# (co, world) = createConfigurationObject configuration world 
		# (cm, world) = callObjectMethod "fromTextArea" [toJSArg ta, toJSArg co] cmobj world
		
		# world = loadModulesIfNeeded configuration cm world
					
		# st = {codeMirror = cm, systemEventHandlers = systemEvents}
		
		# world = manageSystemEvents "on" st world	
		# world = foldl (putOnEventHandler cm) world eventhandlers
		
		= (val, Just st, world)
	where
		putOnEventHandler cm world (event, handler)
			= snd (callObjectMethod "on" [toJSArg event, toJSArg (createEditletEventHandler handler cid)] cm world)

		systemEvents = [("cursorActivity",	createEditletEventHandler onCursorActivity cid),
						("change",			createEditletEventHandler onChange cid)]

		isSetMode (CMMode _) = True
		isSetMode _ = False

	manageSystemEvents direction {codeMirror, systemEventHandlers} world
			= foldl sw world systemEventHandlers
	where
		sw world (event, handler) = snd (callObjectMethod direction [toJSArg event, toJSArg handler] codeMirror world)

	unPackPosition pos world
		# (line, world) = jsGetObjectAttr "line" pos world
		# (ch, world) = jsGetObjectAttr "ch" pos world		
		= ((line, ch), world)

	// TODO
	onChange cid event val st=:(Just {codeMirror}) world 
		# (cmdoc, world) = callObjectMethod "getDoc" [] codeMirror world
		# (newsource, world) = callObjectMethod "getValue" [] cmdoc world				
		= ({val & source = jsValToString newsource}, st, world)

	onCursorActivity cid event val st=:(Just {codeMirror}) world 
		# (cmdoc, world) = callObjectMethod "getDoc" [] codeMirror world

		# (pos, world) = callObjectMethod "getCursor" [toJSArg "start"] cmdoc world
		# (idx1, world) = indexFromPos pos cmdoc world
		# idx1 = jsValToInt idx1
		# val = {val & position = idx1}
		
		# (pos, world) = callObjectMethod "getCursor" [toJSArg "end"] cmdoc world
		# (idx2, world) = indexFromPos pos cmdoc world		
		# idx2 = jsValToInt idx2

		# val = if (idx1 == idx2)
				   {val & selection = Nothing}
				   {val & selection = Just (idx1,idx2)}
		
		= (val, st, world)
	where
		indexFromPos pos cmdoc world = callObjectMethod "indexFromPos" [toJSArg pos] cmdoc world
	
	genDiff val1 val2 = Just ( map SetOption (differenceBy (===) val2.configuration val1.configuration)
							   ++
							   if (val1.position == val2.position) [] [SetPosition val2.position] 
							   ++
							   if (val1.selection === val2.selection) [] [SetSelection val2.selection]
							   ++
							   if (val1.source == val2.source) [] [SetValue val2.source])

	appDiff diffs val = foldl upd val diffs
	where
		upd val=:{configuration} (SetOption opt) = {val & configuration = replaceInList shallowEq opt configuration}
		upd val=:{position} (SetPosition pos) = {val & position = pos}
		upd val=:{selection} (SetSelection sel) = {val & selection = sel}	
		upd val=:{source} (SetValue str) = {val & source = str}				

derive JSONEncode       CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive JSONDecode       CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gDefault         CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gEq              CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gVisualizeText   CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gEditor          CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gEditMeta        CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gUpdate          CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gVerify	        CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
