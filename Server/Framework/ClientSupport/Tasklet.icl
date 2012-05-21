implementation module Tasklet

import iTasks, Task, TaskState, TUIEncode
import LazyLinker, CodeGeneratorJS, SaplHtml, graph_to_sapl_string
//import sapldebug, StdFile, StdMisc

//---------------------------------------------------------------------------------------

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

				# (state_js, script_js, events_js, _, iworld) 
					= linker state 
							 (map (eventHandlerWrapper taskId) gui.eventHandlers)
						     Nothing
						     iworld
					
				# tui = tHTMLToTasklet gui taskId state_js script_js events_js
				# taskTuiRep = appTweak (ViewPart, Just tui, [], [])
				# layout = repLayout taskRepOpts
				# taskTuiRep = appLayout layout SingleTask [taskTuiRep] [] []
						
				# rep = TaskRep taskTuiRep []						
				= (rep, iworld)

			TaskletTUI gui

				# (mb_ino, mb_cf) = case gui.eventHandler of
						Just (iNo, eh) = (Just (toString iNo), Just eh)
									   = (Nothing , Nothing)

				# (state_js, script_js, _, mb_cf_js, iworld) 
					= linker state 
							 []
						     (fmap controllerWrapper mb_cf)
						     iworld
					
				# tui = tTUIToTasklet gui taskId state_js script_js mb_ino mb_cf_js
				# taskTuiRep = appTweak (ViewPart, Just tui, [], [])
				# layout = repLayout taskRepOpts
				# taskTuiRep = appLayout layout SingleTask [taskTuiRep] [] []
						
				# rep = TaskRep taskTuiRep []							
				= (rep, iworld)

	tTUIToTasklet {TaskletTUI|tui} taskId state_js script_js mb_ino mb_cf_js
		 = (defaultDef (TUITasklet  { taskId   		 = toString taskId
									, html     		 = Nothing
								    , tui      		 = tui 
								    , defState 		 = Just state_js
								    , script   		 = Just script_js
								    , events   		 = Nothing
								    , instanceNo	 = mb_ino
								    , controllerFunc = mb_cf_js}))

	tHTMLToTasklet {TaskletHTML|width,height,html} taskId state_js script_js events_js
		= setSize width height 
			(defaultDef (TUITasklet { taskId   		 = toString taskId
								    , html     		 = Just html
								    , tui      		 = Nothing
								    , defState 		 = Just state_js
								    , script   		 = Just script_js
								    , events   		 = Just events_js
								    , instanceNo     = Nothing
								    , controllerFunc = Nothing}))

	appTweak taskTuiRep = tweakTUI tasklet.tweakUI taskTuiRep

	/* Controller wrapper to be easier to write controler function:
	 * 1. taskId is parsed
	 * 2. TUI result is stringified 
	 */
	controllerWrapper cf strTaskID st mbEventName mbEventHandler
		# (mbTUI, res, st) = cf (fromString strTaskID) st mbEventName mbEventHandler
		= (fmap (toString o encodeTUIDefinition) mbTUI, res, st)

	eventHandlerWrapper taskId (HtmlEvent id event f) 
		= (id, event, handleJSEvent f (toString taskId))

//---------------------------------------------------------------------------------------

linker state eventHandlers mbControllerFunc iworld

	/* 1. First, we collect all the necessary function definitions to generate ParserState */

	# (ls, iworld) = generateLoaderState iworld
	// link functions indicated by the state structure
	# saplst = graph_to_sapl_string state
	# (ls, a, iworld) = linkSaplforExprByLoaderState ls newAppender saplst iworld

	// link functions indicated by controller func
	# (ls, a, mbSaplCF, iworld) = case mbControllerFunc of
		Just cf # saplCF = graph_to_sapl_string cf
				# (ls, a, iworld) = linkSaplforExprByLoaderState ls a saplCF iworld
				= (ls, a, Just saplCF, iworld)
				= (ls, a, Nothing,  iworld)
				
	// link functions indicated by event handlers
	# (ls, a, iworld) = foldl (\(ls, a, iworld) (_,_,f) = 
				linkSaplforExprByLoaderState ls a (graph_to_sapl_string f) iworld) (ls, a, iworld) eventHandlers

	/* 2. Generate function definitions and ParserState */

	# sapl = toString a
	# (script, mbPst) = case sapl of
		"" = ("", Nothing)
		   = let (script, pst) = fromOk (generateJS sapl) in (toString script, Just pst)
	
	/* 3. Generate expressions by ParserState */
									
	# statejs = toString (fromOk (exprGenerateJS saplst mbPst))

	# events = map (\(id,event,handler) = (id,event,toString (fromOk 
				(exprGenerateJS (graph_to_sapl_string handler) mbPst)))) eventHandlers
	
	# cfjs = case mbSaplCF of
		Just saplCF = Just (toString (fromOk (exprGenerateJS saplCF mbPst)))
					= Nothing
					
/* For debugging:					

	# (_, iworld) = writeFile "debug_state.sapl" saplst iworld
	# (_, iworld) = writeFile "debug_state.js" statejs iworld	
	# (_, iworld) = writeFile "debug.sapl" sapl iworld
	# (_, iworld) = writeFile "debug.js" script iworld

*/
	= (statejs, script, events, cfjs, iworld)
 