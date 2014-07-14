implementation module iTasks.API.Extensions.CodeMirror

import StdMisc, StdString, StdDebug
import Data.Maybe, Data.List

import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Tasklet
import iTasks.API.Core.Client.Interface

:: JSCM = JSCM

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

createConfigurationObject :: [CodeMirrorConfiguration] !*JSWorld -> *(!JSObj CodeMirrorConfiguration, !*JSWorld)
createConfigurationObject cs world
	# (obj, world) = jsEmptyObject world
	= (obj, foldl (set obj) world (map toAttrValue cs))
where
	set obj world (attr,value) = jsSetObjectAttr attr value obj world

setOptions :: [CodeMirrorConfiguration] (JSObj JSCM) !*JSWorld -> *JSWorld
setOptions cs cm world
	# world = foldl upd world (map toAttrValue cs)
	= loadModulesIfNeeded cs cm world
where
	upd world (attr, val) = snd (callObjectMethod "setOption" [toJSArg attr, toJSArg val] cm world)

loadModulesIfNeeded :: [CodeMirrorConfiguration] (JSObj JSCM) !*JSWorld -> *JSWorld
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

onInitClient hndCreator eventhandlers onLoadWrapper onLoadCont cid clval world
	# (obj, world) = findObject "CodeMirror.defaults" world
	| not (jsIsUndefined obj)
	    = onLoad hndCreator eventhandlers onLoadCont cid undef clval world
	# world = addCSSFromUrl "codemirror.css" world
	# world = addJSFromUrl "codemirror.js" Nothing world
	# world = addJSFromUrl "addon/mode/loadmode.js" (Just (hndCreator onLoadWrapper cid)) world
    = (clval,world)

onLoad hndCreator eventhandlers cont cid _ clval=:{val={source,configuration}} world
	# (ta, world)       = getDomElement (sourcearea cid) world
	# (co, world)       = createConfigurationObject configuration world
    # (cmobj, world)    = findObject "CodeMirror" world
    # (this, world)     = jsThis world
    # (cm, world)       = jsApply cmobj this [toJSArg ta, toJSArg co] world
	# world 			= loadModulesIfNeeded configuration cm world
	# st 				= {codeMirror = cm, systemEventHandlers = systemEvents, marks = []}
	# world 			= manageSystemEvents "on" st world	
	# world 			= foldl (putOnEventHandler cm) world eventhandlers

    # (editlets,world)  = findObject "itwc.controller.tasklets" world
    # (cmp,world)       = .? (editlets .# cid) world
    # (clval,world)		= onAfterShow cm cid undef clval world
    # world             = (cmp .# "afterResize" .= (toJSVal (hndCreator (onAfterShow cm) cid))) world
    # world             = (cmp .# "afterShow" .= (toJSVal (hndCreator (onAfterShow cm) cid))) world        
	
    //Call continuation to initialize the editor	
    = cont cid {clval & mbSt = Just st} world
where
	// Set the size directly because CodeMirror seems that cannot work with CSS3 FlexBox stuff
    onAfterShow cm cid _ st world
        # (editlets, world) = findObject "itwc.controller.editlets" world
        # (editlet,world)   = jsGetObjectAttr cid editlets world
        # (domEl,world)     = jsGetObjectAttr "domEl" editlet world
        # (style,world)     = callObjectMethod "getComputedStyle" [toJSArg domEl] jsWindow world
        # (width,world)     = jsGetObjectAttr "width" style world
        # (height,world)    = jsGetObjectAttr "height" style world

        # (_,world)       	= (cm .# "setSize" .$ (width,height)) world
	    = (st, world)

	putOnEventHandler cm world (event, handler)
		= snd (callObjectMethod "on" [toJSArg event, toJSArg (hndCreator handler cid)] cm world)

	systemEvents = [("cursorActivity",	hndCreator onCursorActivity cid),
					("change",			hndCreator onChange cid)]

	isSetMode (CMMode _) = True
	isSetMode _ = False

	onChange cid event clval=:{val={source}, mbSt=Just {codeMirror}} world
		# (cmdoc, world) = callObjectMethod "getDoc" [] codeMirror world
		# (newsource, world) = callObjectMethod "getValue" [] cmdoc world				
        = ({clval & val={clval.val & source = jsValToString newsource}}, world)

	onCursorActivity cid event clval=:{val, mbSt=Just {codeMirror}} world 
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
		
		= ({clval & val = val}, world)
	where
		indexFromPos pos cmdoc world = callObjectMethod "indexFromPos" [toJSArg pos] cmdoc world

manageSystemEvents direction {codeMirror, systemEventHandlers} world
	= foldl sw world systemEventHandlers
where
	sw world (event, handler) = snd (callObjectMethod direction [toJSArg event, toJSArg handler] codeMirror world)

posFromIndex idx cmdoc world = callObjectMethod "posFromIndex" [toJSArg idx] cmdoc world

addMark cmdoc (i1,i2) world
	# (p1, world) = posFromIndex i1 cmdoc world
	# (p2, world) = posFromIndex i2 cmdoc world
	# (conf, world) = jsEmptyObject world
	# world = (conf .# "className" .= "cm-highlight") world
	= (cmdoc .# "markText" .$ (p1,p2,conf)) world

sourcearea :: String -> String
sourcearea id = "cm_source_" +++ id

codeMirrorUIDef :: a [ComponentEvent b c] -> ComponentHTML b c | toString a
codeMirrorUIDef cid eventHandlers
	= { html 			= DivTag [] 
									[StyleTag [] [Text "span.cm-highlight { background: #F3FA25 } \n .CodeMirror-focused span.cm-highlight { background: #F3FA25; !important }"] //FAD328
									,DivTag [IdAttr (sourcearea (toString cid)), StyleAttr "display: block; position: absolute;"] []]
	  , eventHandlers 	= eventHandlers
	  , width 			= FlexSize //ExactSize 300
	  , height			= FlexSize //ExactSize 300
	  }

codeMirrorTasklet :: !CodeMirror
				  -> Tasklet CodeMirrorClient CodeMirror
codeMirrorTasklet g = 
				{ Tasklet 
				| genUI      = \tid _ world -> (TaskletHTML (codeMirrorUIDef tid eventHandlers), {val = g, mbSt = Nothing}, world)
				, resultFunc = \clval -> Value clval.val False
				, tweakUI    = id
				}
where
	eventHandlers = [ComponentEvent "tasklet" "init" onInit]

	onInit taskId _ clval world
		= onInitClient foo [] onLoadWrapper onLoadCont (toString taskId) clval world 
    where
		onLoadCont taskId clval world = onUpdate taskId clval world 
		onLoadWrapper taskId obj st world = onLoad foo [] onLoadCont (toString taskId) obj st world
		foo hnd taskId = createTaskletEventHandler (\taskId obj val st -> hnd (toString taskId) obj val st) (fromString taskId)

	onUpdate taskid clval=:{val={source,configuration,position,selection,highlighted}, mbSt=Just st=:{codeMirror,marks}} world 
		// disable system event handlers
		# world = manageSystemEvents "off" st world		
	
		# world = setOptions configuration codeMirror world
		# world = loadModulesIfNeeded configuration codeMirror world
		
		# (cmdoc, world) = callObjectMethod "getDoc" [] codeMirror world

		# world = snd (callObjectMethod "setValue" [toJSArg source] cmdoc world)
			
		# (pos, world) = posFromIndex position cmdoc world 
		# world = snd (callObjectMethod "setCursor" [toJSArg pos] cmdoc world)		
	
		# world = case selection of
			Nothing	
				= world
			(Just (idx1,idx2))
				# (pos1, world) = posFromIndex idx1 cmdoc world 
				# (pos2, world) = posFromIndex idx2 cmdoc world 							
				= snd (callObjectMethod "setSelection" [toJSArg pos1, toJSArg pos2] cmdoc world)	

		// Set marks	
		# (marks, world) =
			foldl (\(ms, world) pos -> let (m,w) = addMark cmdoc pos world in ([m:ms], w)) ([], world) highlighted	
	
		// enable system event handlers
		# world = manageSystemEvents "on" st world
					
		= ({clval & mbSt = Just {st & marks = marks}}, world)

codeMirrorEditlet :: !CodeMirror
					 [(String, EditletEventHandlerFunc CodeMirrorClient)]
				  -> Editlet CodeMirror [CodeMirrorDiff]
			  
codeMirrorEditlet g eventhandlers = Editlet g
				{ EditletServerDef
				| genUI		= \cid world -> (codeMirrorUIDef cid [], world)
				, defVal	= {source = "", configuration = [], position = 0, selection = Nothing, highlighted = []}
				, genDiff	= genDiffServer
				, appDiff	= appDiffServer
				}
				{ EditletClientDef
				| updateUI	= onUpdate
				, defVal 	= {val = {source = "", configuration = [], position = 0, selection = Nothing, highlighted = []}, mbSt = Nothing}
				, genDiff	= genDiffClient
				, appDiff	= appDiffClient
				}
				
where	
	// init
	onUpdate cid mbDiffs clval=:{mbSt=Nothing} world
		= onInitClient createEditletEventHandler eventhandlers onLoadWrapper onLoadCont cid clval world 
    where
		onLoadWrapper = onLoad createEditletEventHandler eventhandlers onLoadCont
		onLoadCont cid clval world = onUpdate cid mbDiffs clval world 

	// update
	onUpdate cid (Just diffs) clval=:{mbSt=Just st=:{codeMirror,marks}} world	
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

		# (marks, world) = case find isSetHighlights nopts of
			Nothing    	= (marks, world)
			(Just (SetHighlights newmarks)) 	

						// Clear all marks
						//# (marks, world) = (cmdoc .# "getAllMarks" .$ ()) world
						//# (marks, world) = fromJSArray marks id world
						# world = foldl (\world m -> snd ((m .# "clear" .$ ()) world)) world marks

						// Set marks
						= foldl (\(ms, world) pos -> let (m,w) = addMark cmdoc pos world in ([m:ms], w)) ([], world) newmarks

		// enable system event handlers
		# world = manageSystemEvents "on" st world
					
		= ({clval & mbSt = Just {st & marks = marks}}, world)
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

		isSetHighlights (SetHighlights _) = True
		isSetHighlights _ = False
	
	unPackPosition pos world
		# (line, world) = jsGetObjectAttr "line" pos world
		# (ch, world) = jsGetObjectAttr "ch" pos world		
		= ((line, ch), world)

	genDiffClient clval1 clval2 = genDiffServer clval1.val clval2.val
	
	genDiffServer val1 val2 = case ( map SetOption (differenceBy (===) val2.configuration val1.configuration)
							   ++
							   if (val1.position == val2.position) [] [SetPosition val2.position]
							   ++
							   if (val1.selection === val2.selection) [] [SetSelection val2.selection]
							   ++
							   if (val1.source == val2.source) [] [SetValue val2.source])
							   ++ 
							   [SetHighlights val2.highlighted] of
        []      = Nothing
        diffs   = Just diffs

	appDiffClient diffs clval = {clval & val = appDiffServer diffs clval.val}

	appDiffServer diffs val = foldl upd val diffs
	where
		upd val=:{configuration} (SetOption opt) = {val & configuration = replaceInList shallowEq opt configuration}
		upd val=:{position} (SetPosition pos) = {val & position = pos}
		upd val=:{selection} (SetSelection sel) = {val & selection = sel}	
		upd val=:{source} (SetValue str) = {val & source = str}
		upd val=:{highlighted} (SetHighlights hs) = {val & highlighted = hs}					

derive JSONEncode       CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive JSONDecode       CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gDefault         CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gEq              CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gText            CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gEditor          CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gEditMeta        CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gUpdate          CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gVerify	        CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
