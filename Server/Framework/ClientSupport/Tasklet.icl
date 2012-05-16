implementation module Tasklet

import iTasks, Task, TaskState
import LazyLinker, CodeGeneratorJS, SaplHtml, graph_to_sapl_string

mkTask :: (Tasklet st res) -> Task res | JSONDecode{|*|} st & JSONEncode{|*|} st & JSONDecode{|*|} res 
mkTask tasklet = Task taskFunc
where
	// Init
	taskFunc mbEdit mbCommit refreshFlag taskRepOpts (TCInit taskId ts) iworld
		= taskFunc mbEdit mbCommit refreshFlag taskRepOpts (TCBasic taskId ts (toJSON tasklet.defSt) False) iworld

	// Re-Init
	taskFunc Nothing Nothing refreshFlag taskRepOpts context=:(TCBasic taskId ts jsonSt _) iworld
		# st = fromJust (fromJSON (jsonSt))
		# (rep, iworld) = genRep taskId st taskRepOpts iworld 
		# result = ValueResult (tasklet.resultFunc st) ts rep context
		= (result, iworld)

	// Edit
	taskFunc (Just (TaskEvent targetTaskId ("state", jsonSt))) mbCommit refreshFlag taskRepOpts (TCBasic taskId ts _ _) iworld
		# st = fromJust (fromJSON (jsonSt))		
		# rep = TaskRep (appTweak (ViewPart, Just (defaultDef (TUITaskletPlaceholder)), [], [])) []
		# result = ValueResult (tasklet.resultFunc st) ts rep (TCBasic taskId ts jsonSt False)
		= (result, iworld)
 
	// Commit
	taskFunc mbEdit (Just (TaskEvent targetTaskId eventName)) refreshFlag taskRepOpts (TCBasic taskId ts jsonSt _) iworld
		# st = fromJust (fromJSON (jsonSt))		
		# rep = TaskRep (appTweak (ViewPart, Just (defaultDef (TUITaskletPlaceholder)), [], [])) []
		# result = ValueResult (tasklet.resultFunc st) ts rep (TCBasic taskId ts jsonSt False)
		= (result, iworld)

	// Destroy
	taskFunc mbEdit mbCommit refreshFlag taskRepOpts (TCDestroy _) iworld
		= (DestroyedResult, iworld)

	genRep taskId st taskRepOpts iworld 
		# (gui, state, iworld) = tasklet.generatorFunc taskId st iworld
		= case gui of
		
			TaskletHTML gui 

				# (state_js, script_js, events_js, iworld) 
					= linker state 
							 gui.eventHandlers
						     (\handler = handleWidgetEvent handler (toString taskId)) 
						     iworld
					
				# tui = toHTML gui taskId state_js script_js events_js
				# taskTuiRep = appTweak (ViewPart, Just tui, [], [])
				# layout = repLayout taskRepOpts
				# taskTuiRep = appLayout layout SingleTask [taskTuiRep] [] []
						
				# rep = TaskRep taskTuiRep []							
				= (rep, iworld)

	toHTML {TaskletHTML|width,height,html} taskId state_js script_js events_js
		= setSize width height 
			(defaultDef (TUITasklet { taskId   = toString taskId
								    , html     = Just html 
								    , defState = Just state_js
								    , script   = Just script_js
								    , events   = Just events_js}))

	appTweak taskTuiRep = tweakTUI tasklet.tweakUI taskTuiRep

//---------------------------------------------------------------------------------------

:: EventHandlerWrapperFunc st :== (HtmlEventHandlerFunc st) *HtmlObject -> Void

linker :: !st ![HtmlEvent st] (EventHandlerWrapperFunc st) !*IWorld -> *(!String, !String, [(!String,!String,!String)], !*IWorld)
linker state eventHandlers handlerWrapper iworld
	# (ls, iworld) = generateLoaderState iworld
	// load functions indicated by the state structure
	# saplst = graph_to_sapl_string state
	# (ls, a, iworld) = linkSaplforExprByLoaderState ls newAppender saplst iworld
				
	// load functions indicated by event handlers
	# (ls, a, iworld) = foldl (\(ls, a, iworld) (HtmlEvent _ _ f) = 
				linkSaplforExprByLoaderState ls a (graph_to_sapl_string f) iworld) (ls, a, iworld) eventHandlers
								
	// State object can be very large, so it is not a good idea to copy
	// it to the argument list of every event handler
	// It has its own place now in the TUIWidget structure
	# statejs = toString (fst (exprGenerateJS saplst))

	# sapl = toString a
	# script = toString (fst (generateJS sapl))
	# events = map (\(HtmlEvent id event handler) = (id,event,toString (fst 
				(exprGenerateJS (graph_to_sapl_string (handlerWrapper handler)))))) eventHandlers
	
	= (statejs, script, events, iworld)
 