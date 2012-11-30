implementation module Tasklet

import iTasks, Task, TaskState, UIDefinition
import LazyLinker, CodeGeneratorJS, SaplHtml, graph_to_sapl_string
import sapldebug, StdFile, StdMisc //, graph_to_string_with_descriptors
from Map import newMap 

//---------------------------------------------------------------------------------------

println :: !String !*IWorld -> *IWorld
println msg iw=:{world} 
	# (console,world)	= stdio world
	# console			= fwrites msg console
	# console			= fwrites "\n" console
	# (_,world)			= fclose console world
	= {iw & world = world} 

toDef c = UIControlSequence (newMap, [(c,newMap)], Vertical)

mkTask :: (Tasklet st res) -> Task res | JSONDecode{|*|} res & JSONEncode{|*|} res
mkTask tasklet = Task taskFunc
where
	// Init
	taskFunc event taskRepOpts (TCInit taskId ts) iworld
		# (rep, st, iworld) = genRep taskId taskRepOpts Nothing iworld
		# res = tasklet.Tasklet.resultFunc st
		# result = ValueResult res (taskInfo ts) rep (TCBasic taskId ts (toJSON res) False)
		= (result, println "init" iworld)

	// Refresh: server restart. anything else?
	taskFunc RefreshEvent taskRepOpts context=:(TCBasic taskId ts jsonRes _) iworld
		# (rep, st, iworld) = genRep taskId taskRepOpts Nothing iworld

		//No! because state and value will be out of sync!
		//# res = fromJust (fromJSON (jsonRes))
		# res = tasklet.Tasklet.resultFunc st
		
		# result = ValueResult res (taskInfo ts) rep context
		= (result, println "refresh" iworld)

	// Focus: tab switch. anything else?
	taskFunc (FocusEvent _) taskRepOpts context=:(TCBasic taskId ts jsonRes _) iworld
		# res = fromJust (fromJSON (jsonRes))
		# result = ValueResult res (taskInfo ts) (placeHolderRep taskId) context
		= (result, println "focus" iworld)
 
	// Edit: "result"
	taskFunc (EditEvent targetTaskId "result" jsonRes) taskRepOpts (TCBasic taskId ts _ _) iworld
		# res = fromJust (fromJSON (jsonRes))
		# result = ValueResult res (taskInfo ts) (placeHolderRep taskId) (TCBasic taskId ts jsonRes False)
		= (result, println "result" iworld) 
 
	// Edit: "finalize"
	taskFunc (EditEvent targetTaskId "finalize" jsonRes) taskRepOpts (TCBasic taskId ts _ _) iworld
		# res = fromJust (fromJSON (jsonRes))
		# rep = TaskRep (appTweak (ViewPart, Nothing, [], [])) []
		# result = DestroyedResult //ValueResult res (taskInfo ts) rep (TCDestroy (TCBasic taskId ts jsonRes False))
		= (result, println "finalize" iworld)  
 
	// Commit
	taskFunc event taskRepOpts (TCBasic taskId ts jsonRes _) iworld
		# res = fromJust (fromJSON (jsonRes))
		# rep = placeHolderRep taskId 
		# result = ValueResult res (taskInfo ts) rep (TCBasic taskId ts jsonRes False)
		= (result, println "commit" iworld)

	// Destroy
	taskFunc event taskRepOpts (TCDestroy _) iworld
		= (DestroyedResult, println "destroy" iworld)

	taskInfo ts = {TaskInfo | lastEvent = ts, expiresIn = Nothing}

	placeHolderRep taskId 
		= TaskRep (toDef (UITaskletPlaceholder defaultSizeOpts (toString taskId))) []

	genRep taskId taskRepOpts mbState iworld 
		# (gui, state, iworld) = tasklet.generatorFunc taskId mbState iworld
		= case gui of
		
			TaskletHTML gui 

				# (state_js, script_js, events_js, rf_js, _, iworld) 
					= linker state 
							 (map (eventHandlerWrapper taskId) gui.eventHandlers)
							 tasklet.Tasklet.resultFunc
						     Nothing
						     iworld
					
				# tui = tHTMLToTasklet gui taskId state_js script_js events_js rf_js
				# rep = TaskRep (appTweak tui) []						
				= (rep, state, iworld)

			TaskletTUI gui

				# (mb_ino, mb_cf) = case gui.eventHandler of
						Just (iNo, eh) = (Just (toString iNo), Just eh)
									   = (Nothing , Nothing)

				# (state_js, script_js, _, rf_js, mb_cf_js, iworld) 
					= linker state 
							 []
							 tasklet.Tasklet.resultFunc
						     (fmap controllerWrapper mb_cf)
						     iworld
					
				# tui = tTUIToTasklet gui taskId state_js script_js mb_ino rf_js mb_cf_js
				# rep = TaskRep (appTweak tui) []							
				= (rep, state, iworld)

	tTUIToTasklet {TaskletTUI|tui} taskId state_js script_js mb_ino rf_js mb_cf_js
		 = toDef (UITasklet defaultSizeOpts 
		 			{UITaskletOpts 
		 			| taskId   		 = toString taskId
					, html     		 = Nothing
					, tui      		 = tui 
					, st    		 = Just state_js
					, script   		 = Just script_js
					, events   		 = Nothing
					, resultFunc	 = Just rf_js
					, instanceNo	 = mb_ino
					, controllerFunc = mb_cf_js})

	tHTMLToTasklet {TaskletHTML|width,height,html} taskId state_js script_js events_js rf_js
		= toDef (setSize width height 
			(UITasklet defaultSizeOpts 
					 {UITaskletOpts 
					 | taskId   	  = toString taskId
					 , html     	  = Just (toString html)
					 , tui	      	  = Nothing
					 , st    		  = Just state_js
					 , script   	  = Just script_js
					 , events   	  = Just events_js
					 , resultFunc     = Just rf_js
					 , instanceNo     = Nothing
					 , controllerFunc = Nothing}))

	appTweak taskTuiRep = tweakUI tasklet.tweakUI taskTuiRep

	/* Controller wrapper to be easier to write controler function:
	 * 1. taskId is parsed
	 * 2. TUI result is stringified 
	 */
	controllerWrapper cf strTaskID st mbEventName mbEventHandler
		# (mbTUI, st) = cf (fromString strTaskID) st mbEventName mbEventHandler
		= (fmap (toString o encodeUIDefinition) mbTUI, st)

	// it uses the 2. layer (handleJSEvent), because it's created on the server
	eventHandlerWrapper taskId (HtmlEvent id event f) 
		= (id, event, handleJSEvent f (toString taskId))

//---------------------------------------------------------------------------------------

instance toString HtmlDef
where
	toString (HtmlDef a) = toString a

//---------------------------------------------------------------------------------------

linker state eventHandlers resultFunc mbControllerFunc iworld
	
	/* 1. First, we collect all the necessary function definitions to generate ParserState */

	# (ls, iworld) = generateLoaderState iworld
	// link functions indicated by the state structure
	# saplst = graph_to_sapl_string state
	# (ls, a, saplst, iworld) = linkSaplforExprByLoaderState ls newAppender saplst iworld

	// link functions indicated by result func
	# saplRF = graph_to_sapl_string resultFunc
	# (ls, a, saplRF, iworld) = linkSaplforExprByLoaderState ls a saplRF iworld

	// link functions indicated by controller func
	# (ls, a, mbSaplCF, iworld) = case mbControllerFunc of
		Just cf # saplCF = graph_to_sapl_string cf
				# (ls, a, saplCF, iworld) = linkSaplforExprByLoaderState ls a saplCF iworld
				= (ls, a, Just saplCF, iworld)
				= (ls, a, Nothing,  iworld)
				
	// link functions indicated by event handlers
	# (ls, a, eventHandlers, iworld) = foldl (\(ls, a, hs, iworld) (e1,e2,f) = 
				let (ls2, a2, f2, iworld2) = linkSaplforExprByLoaderState ls a (graph_to_sapl_string f) iworld
				 in (ls2, a2, [(e1,e2,f2):hs], iworld2)) 
			(ls, a, [], iworld) eventHandlers

	/* 2. Generate function definitions and ParserState */

	# sapl = toString a	
	# (script, mbPst) = case sapl of
		"" = ("", Nothing)
		   = let (script, pst) = fromOk (generateJS sapl) in (toString script, Just pst)
	
	/* 3. Generate expressions by ParserState */
									
	# statejs = toString (fromOk (exprGenerateJS saplst mbPst))

	# events = map (\(id,event,saplhandler) = (id,event,toString (fromOk 
				(exprGenerateJS saplhandler mbPst)))) eventHandlers
	
	# rfjs = toString (fromOk (exprGenerateJS saplRF mbPst))		
	
	# cfjs = case mbSaplCF of
		Just saplCF = Just (toString (fromOk (exprGenerateJS saplCF mbPst)))
					= Nothing		
					
/* For debugging:

	# (_, iworld) = writeFile "debug_state.sapl" saplst iworld
	# (_, iworld) = writeFile "debug_state.js" statejs iworld	
	# (_, iworld) = writeFile "debug.sapl" sapl iworld
	# (_, iworld) = writeFile "debug.js" script iworld
*/

	= (statejs, script, events, rfjs, cfjs, iworld)
 