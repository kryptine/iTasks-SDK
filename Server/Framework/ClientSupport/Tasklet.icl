implementation module Tasklet

import iTasks, Task, TaskState, UIDefinition
import LazyLinker, CodeGeneratorJS, SaplHtml, graph_to_sapl_string
import sapldebug, StdFile, StdMisc //, graph_to_string_with_descriptors
import Time
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

mkInstanceId :: Task String
mkInstanceId = mkInstantTask taskFunc
where
	// TODO: generate actually unique id
	taskFunc _ iw=:{world} 
		# (c, world) = clock world
		= (Ok ("i" +++ toString c), {iw & world = world})

/*
mkInstance :: (Tasklet st res) -> Task (TaskletInstance st res)
mkInstance tasklet = mkInstantTask taskFunc
where
	// TODO: generate actually unique id
	taskFunc _ iworld=:{taskTime} = (Ok ("i" +++ toString taskTime, tasklet), iworld)
*/

mkTask :: (TaskletInstance st res) -> Task res | JSONDecode{|*|} res & JSONEncode{|*|} res
mkTask ti = mkInterfaceTask ti []

mkInterfaceTask :: (TaskletInstance st res) [InterfaceFun st] -> Task res | JSONDecode{|*|} res & JSONEncode{|*|} res
mkInterfaceTask (iid, tasklet) fs = Task taskFunc
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
		= TaskRep (toDef (UITaskletPH defaultSizeOpts {UITaskletPHOpts|taskId = toString taskId, iid = iid})) []

	genRep taskId taskRepOpts mbState iworld
		# (gui, state, iworld) = tasklet.generatorFunc iid taskId mbState iworld
		= case gui of
		
			TaskletHTML gui 

				# (state_js, script_js, events_js, intfcs_js, rf_js, _, iworld) 
					= linker state
							 (map interfaceWrapper fs) 
							 (map eventHandlerWrapper gui.eventHandlers)
							 tasklet.Tasklet.resultFunc
						     Nothing
						     iworld
					
				# tui = tHTMLToTasklet gui taskId state_js script_js events_js intfcs_js rf_js
				# rep = TaskRep (appTweak tui) []						
				= (rep, state, iworld)

			TaskletTUI gui

				# (mb_ino, mb_cf) = case gui.eventHandler of
						Just (iNo, eh) = (Just (toString iNo), Just eh)
									   = (Nothing , Nothing)

				# (state_js, script_js, _, _, rf_js, mb_cf_js, iworld) 
					= linker state
							 []
							 []
							 tasklet.Tasklet.resultFunc
						     (fmap controllerWrapper mb_cf)
						     iworld
					
				# tui = tTUIToTasklet gui taskId state_js script_js mb_ino rf_js mb_cf_js
				# rep = TaskRep (appTweak tui) []							
				= (rep, state, iworld)

			NoGUI
			
				# (state_js, script_js, _, intfcs_js, rf_js, _, iworld) 
					= linker state
							 (map interfaceWrapper fs) 
							 []
							 tasklet.Tasklet.resultFunc
						     Nothing
						     iworld			
			
				# tui = toDef (UITasklet defaultSizeOpts 
				 			{UITaskletOpts 
				 			| taskId   		 = toString taskId
				 			, iid			 = iid
							, html     		 = Nothing
							, tui      		 = Nothing 
							, st    		 = Just state_js
							, script   		 = Just script_js
							, events   		 = Nothing
							, interfaceFuncs = Just intfcs_js
							, resultFunc	 = Just rf_js
							, instanceNo	 = Nothing
							, controllerFunc = Nothing})			
			
				# rep = TaskRep (appTweak tui) []				
				= (rep, state, iworld)

	tTUIToTasklet {TaskletTUI|tui} taskId state_js script_js mb_ino rf_js mb_cf_js
		 = toDef (UITasklet defaultSizeOpts 
		 			{UITaskletOpts 
		 			| taskId   		 = toString taskId
		 			, iid			 = iid
					, html     		 = Nothing
					, tui      		 = tui 
					, st    		 = Just state_js
					, script   		 = Just script_js
					, events   		 = Nothing
					, interfaceFuncs = Nothing
					, resultFunc	 = Just rf_js
					, instanceNo	 = mb_ino
					, controllerFunc = mb_cf_js})

	tHTMLToTasklet {TaskletHTML|width,height,html} taskId state_js script_js events_js intfcs_js rf_js
		= toDef (setSize width height 
			(UITasklet defaultSizeOpts 
					 {UITaskletOpts 
					 | taskId   	  = toString taskId
 		 			 , iid	  		  = iid
					 , html     	  = Just (toString html)
					 , tui	      	  = Nothing
					 , st    		  = Just state_js
					 , script   	  = Just script_js
					 , events   	  = Just events_js
					 , interfaceFuncs = Just intfcs_js					 
					 , resultFunc     = Just rf_js
					 , instanceNo     = Nothing
					 , controllerFunc = Nothing}))

	appTweak taskTuiRep = tweakUI tasklet.tweakUI taskTuiRep

	/* Controller wrapper to be easier to write controller function:
	 * 1. taskId is parsed
	 * 2. TUI result is stringified 
	 */
	controllerWrapper cf strTaskID st mbEventName mbEventHandler
		# (mbTUI, st) = cf (fromString strTaskID) st mbEventName mbEventHandler
		= (fmap (toString o encodeUIDefinition) mbTUI, st)

	// it uses the 2. layer (handleJSEvent), because it's created on the server
	eventHandlerWrapper (HtmlEvent id event f) 
		= (id, event, handleJSEvent f iid)

	interfaceWrapper (InterfaceFun fn f) = (fn, handleInterfaceCall f iid)

//---------------------------------------------------------------------------------------

instance toString HtmlDef
where
	toString (HtmlDef a) = toString a

//---------------------------------------------------------------------------------------

handlerr (Error str) = abort ("Tasklet.icl: " +++ str)
handlerr (Ok a) = a

linker state interfaceFuns eventHandlers resultFunc mbControllerFunc iworld=:{world}
	
	/* 1. First, we collect all the necessary function definitions to generate ParserState */

	# (ls, world) = generateLoaderState world
	// link functions indicated by the state structure
	# saplst = graph_to_sapl_string state
	# (ls, a, saplst, world) = linkSaplforExprByLoaderState ls newAppender saplst world

	// link functions indicated by result func
	# saplRF = graph_to_sapl_string resultFunc
	# (ls, a, saplRF, world) = linkSaplforExprByLoaderState ls a saplRF world

	// link functions indicated by controller func
	# (ls, a, mbSaplCF, world) = case mbControllerFunc of
		Just cf # saplCF = graph_to_sapl_string cf
				# (ls, a, saplCF, world) = linkSaplforExprByLoaderState ls a saplCF world
				= (ls, a, Just saplCF, world)
				= (ls, a, Nothing,  world)
				
	// link functions indicated by event handlers
	# (ls, a, eventHandlers, world) = foldl (\(ls, a, hs, world) (e1,e2,f) = 
				let (ls2, a2, f2, world2) = linkSaplforExprByLoaderState ls a (graph_to_sapl_string f) world
				 in (ls2, a2, [(e1,e2,f2):hs], world2)) 
			(ls, a, [], world) eventHandlers

	// link functions indicated by event handlers
	# (ls, a, interfaceFuns, world) = foldl (\(ls, a, hs, world) (fn, f) = 
				let (ls2, a2, f2, world2) = linkSaplforExprByLoaderState ls a (graph_to_sapl_string f) world
				 in (ls2, a2, [(fn,f2):hs], world2)) 
			(ls, a, [], world) interfaceFuns

	/* 2. Generate function definitions and ParserState */

	# sapl = toString a	
	# (script, mbPst) = case sapl of
		"" = ("", Nothing)
		   = let (script, pst) = handlerr (generateJS sapl) in (toString script, Just pst)
	
	/* 3. Generate expressions by ParserState */
									
	# statejs = toString (handlerr (exprGenerateJS saplst mbPst))

	# events = map (\(id,event,saplhandler) = (id,event,toString (handlerr 
				(exprGenerateJS saplhandler mbPst)))) eventHandlers
	
	# intfcs = map (\(fn,saplfun) = (fn, toString (handlerr 
				(exprGenerateJS saplfun mbPst)))) interfaceFuns	
	
	# rfjs = toString (handlerr (exprGenerateJS saplRF mbPst))		
	
	# cfjs = case mbSaplCF of
		Just saplCF = Just (toString (handlerr (exprGenerateJS saplCF mbPst)))
					= Nothing		

/* For debugging:*/

	# (_, world) = writeFile "debug_state.sapl" saplst world
	# (_, world) = writeFile "debug_state.js" statejs world	
	# (_, world) = writeFile "debug.sapl" sapl world
	# (_, world) = writeFile "debug.js" script world


	= (statejs, script, events, intfcs, rfjs, cfjs, {iworld & world=world})
 