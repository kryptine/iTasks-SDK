implementation module iTasks.API.Extensions.CodeMirror

import StdMisc, StdString, StdArray, StdDebug
import Data.Maybe, Data.List, Text

import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Tasklet
import iTasks.API.Core.Client.Interface

derive JSONEncode       CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive JSONDecode       CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gDefault         CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gEq              CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gText            CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gEditor          CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gEditMeta        CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gUpdate          CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror
derive gVerify	        CodeMirrorConfiguration, CodeMirrorDiff, CodeMirror

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
	# (ta, world)       = .? (getElementById (sourcearea cid)) world
	# (co, world)       = createConfigurationObject configuration world
    # (cmobj, world)    = findObject "CodeMirror" world
    # (this, world)     = jsThis world
    # (cm, world)       = jsApply cmobj this [toJSArg ta, toJSArg co] world
	# world 			= loadModulesIfNeeded configuration cm world
	# st 				= {codeMirror = cm, systemEventHandlers = systemEvents, marks = []}
	# world 			= manageSystemEvents "on" st world	
	# world 			= foldl (putOnEventHandler cm) world eventhandlers

    # (tasklets,world)  = findObject "itwc.controller.tasklets" world
    # (editlets,world)  = findObject "itwc.controller.editlets" world    
    # (cmp,world)       = .? (tasklets .# cid) world
    # (cmp,world)       = if (jsIsUndefined cmp) (.? (editlets .# cid) world) (cmp,world)
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

	onChange cid {[0] = event} clval=:{val={source}, mbSt=Just {codeMirror}} world
		# (cmdoc, world) = callObjectMethod "getDoc" [] codeMirror world
		# (newsource, world) = callObjectMethod "getValue" [] cmdoc world

		# (nrlines, world) = callObjectMethod "lineCount" [] cmdoc world
		# nrlines = jsValToInt nrlines		

		# (world, lines) = foldl (\(w,res) i -> let (line, w2) = readLine cmdoc i w in (w2, [line: res])) (world, []) (reverse [0..nrlines-1])

        = ({clval & val={clval.val & source = lines}}, world)
	where
		readLine cmdoc n world
			# (line, world) = callObjectMethod "getLine" [toJSArg n] cmdoc world
			= (jsValToString line, world)
	
	onCursorActivity cid event clval=:{val, mbSt=Just {codeMirror}} world 
		# (cmdoc, world) = callObjectMethod "getDoc" [] codeMirror world

		# (pos, world) = callObjectMethod "getCursor" [toJSArg "start"] cmdoc world
		# (idx1, world) = posToTuple pos world
		# val = {val & position = idx1}
		
		# (pos, world) = callObjectMethod "getCursor" [toJSArg "end"] cmdoc world
		# (idx2, world) = posToTuple pos world		
		# val = if (idx1 == idx2)
				   {val & selection = Nothing}
				   {val & selection = Just (idx1,idx2)}
		
		= ({clval & val = val}, world)

manageSystemEvents direction {codeMirror, systemEventHandlers} world
	= foldl sw world systemEventHandlers
where
	sw world (event, handler) = snd (callObjectMethod direction [toJSArg event, toJSArg handler] codeMirror world)

posToTuple cmpos world 
	# (line, world) = .? (cmpos .# "line") world
	# (ch, world) = .? (cmpos .# "ch") world
	= ((jsValToInt line, jsValToInt ch), world)

tupleToPos (line, ch) world 
	# (cmpos, world) = jsEmptyObject world
	# world = jsSetObjectAttr "line" (toJSVal line) cmpos world
	# world = jsSetObjectAttr "ch" (toJSVal ch) cmpos world
	= (cmpos, world)

addMark cmdoc (i1,i2) world
	# (p1, world) = tupleToPos i1 world
	# (p2, world) = tupleToPos i2 world
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
	  , width 			= ExactSize 300
	  , height			= ExactSize 300
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

		# world = applyDiffClient cmdoc 0 0 source world
			
		# (pos, world) = tupleToPos position world 
		# world = snd (callObjectMethod "setCursor" [toJSArg pos] cmdoc world)		
	
		# world = case selection of
			Nothing	
				= world
			(Just (idx1,idx2))
				# (pos1, world) = tupleToPos idx1 world 
				# (pos2, world) = tupleToPos idx2 world 							
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
			  
codeMirrorEditlet g eventhandlers
  = { Editlet
    | currVal   = g
    , genUI     = \cid world -> (codeMirrorUIDef cid [], world)
    , serverDef = { EditletDef
                  | performIO = \_ _ s w -> (s, w)
                  , defVal    = {source = [], configuration = [], position = (0,0), selection = Nothing, highlighted = []}
                  , genDiff   = genDiffServer
                  , appDiff   = appDiffServer
                  }
    , clientDef = { EditletDef
                  | performIO = onUpdate
                  , defVal    = {val = {source = [], configuration = [], position = (0,0), selection = Nothing, highlighted = []}, mbSt = Nothing}
                  , genDiff   = genDiffClient
                  , appDiff   = appDiffClient
                  }
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
						# (pos, world) = tupleToPos idx world 
						= snd (callObjectMethod "setCursor" [toJSArg pos] cmdoc world)

		# world = case find isSetSel nopts of
			Nothing    		= world
			(Just (SetSelection Nothing))
						// Clear the selection
						# (pos, world) = callObjectMethod "getCursor" [] cmdoc world
						= snd (callObjectMethod "setSelection" [toJSArg pos, toJSArg pos] cmdoc world)
			(Just (SetSelection (Just (idx1,idx2)))) 	
						# (pos1, world) = tupleToPos idx1 world 
						# (pos2, world) = tupleToPos idx2 world 							
						= snd (callObjectMethod "setSelection" [toJSArg pos1, toJSArg pos2] cmdoc world)

		# world = case find isReplaceRange nopts of
			Nothing    	= world
			(Just (ReplaceRange (flines,llines) diff)) = applyDiffClient cmdoc flines llines diff world

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

		isReplaceRange (ReplaceRange _ _) = True
		isReplaceRange _ = False

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
							   maybe [] (\(flines,llines,diff) -> [ReplaceRange (flines,llines) diff]) (genDiffRange val1.source val2.source)
							   ++ 
							   [SetHighlights val2.highlighted]) of
        []      = Nothing
        diffs   = Just diffs

	appDiffClient diffs clval = {clval & val = appDiffServer diffs clval.val}

	appDiffServer diffs val = foldl upd val diffs
	where
		upd val=:{configuration} (SetOption opt) = {val & configuration = replaceInList shallowEq opt configuration}
		upd val=:{position} (SetPosition pos) = {val & position = pos}
		upd val=:{selection} (SetSelection sel) = {val & selection = sel}	
		upd val=:{source} (ReplaceRange (flines,llines) diff) = {val & source = applyDiffServer source flines llines diff}
		upd val=:{highlighted} (SetHighlights hs) = {val & highlighted = hs}					

applyDiffClient cmdoc flines llines diff world
	# (nrlines, world) = callObjectMethod "lineCount" [] cmdoc world
	# nrlines = jsValToInt nrlines
		
	# (line1, world) = callObjectMethod "getLine" [toJSArg (flines - 1)] cmdoc world
	# line1 = if (jsIsUndefined line1) "" (jsValToString line1)
		
	| (size line1 == 0 && nrlines == 1) || (nrlines == flines + llines)
		= insert line1 world
		= replace nrlines world
where
	replace nrlines world 
		# world = jsTrace (flines) world	
		# world = jsTrace (nrlines - llines - 1) world
		# world = jsTrace ("\"" +++ join "\n" diff +++ "\n\"") world	
	
		# (cmposfrom, world) = tupleToPos (flines, 0) world
		# (cmposto, world) = tupleToPos (nrlines - llines - 1, 0) world
		= snd (callObjectMethod "replaceRange" [toJSArg (join "\n" diff +++ "\n"), toJSArg cmposfrom, toJSArg cmposto] cmdoc world)

	insert line1 world
		# (cmpos, world) = tupleToPos (flines - 1, size line1) world
		= foldl (\w l -> snd (callObjectMethod "replaceRange" [toJSArg (l +++ "\n"), toJSArg cmpos, toJSArg cmpos] cmdoc w)) world (reverse diff)

applyDiffServer :: [String] Int Int [String] -> [String]
applyDiffServer os flines llines diff
	= take flines os ++ diff ++ drop (length os - llines) os

genDiffRange :: [String] [String] -> Maybe (Int,Int,[String])
genDiffRange os ns
	= fmap addlines (calcDiffRange os ns)
where
	lns = length ns
	addlines (flines, llines) = (flines, llines, take (lns-llines-flines) (drop flines ns))

// (valid lines from the left, valid lines from the right)
calcDiffRange :: [String] [String] -> Maybe (Int,Int)
calcDiffRange [] []
	= Nothing
calcDiffRange [] _
	= Just (0,0)
calcDiffRange os ns
	| flines == los
		= Nothing
	= Just (flines, llines)
where
	flines = firstdiff os ns 0
	llines = firstdiff (reverse os) (reverse ns) 0
	los = length os
		
	firstdiff [] _ l = l
	firstdiff [o:os] [] l = l
	firstdiff [o:os] [n:ns] l 
		| o == n = firstdiff os ns (l+1)
				 = l
		 
firstDiff :: !String !String -> Int
firstDiff str1 str2 
	= if (size str1 == size str2 && diff == size str1) -1 diff	
where
	diff = d 0
    d p	| (p >= size str1) || (p >= size str2) = p
    d p | str1.[p] == str2.[p] = d (p+1)
    d p = p

lastDiff :: !String !String -> Int
lastDiff str1 str2 
	= if (size str1 == size str2 && diff == size str1) -1 diff
where
	s1 = size str1
	s2 = size str2
	diff = d 0
    d p	| (s1 - p - 1 < 0) || (s1 - p - 1 < 0) = p
    d p | str1.[p] == str2.[p] = d (p+1)
    d p = p
