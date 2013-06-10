implementation module Tasklet

import iTasks, iTasks.Framework.Task, iTasks.Framework.TaskState, iTasks.Framework.UIDefinition
import Sapl.Linker.LazyLinker, Sapl.Target.JS.CodeGeneratorJS, SaplHtml
import graph_to_sapl_string, sapldebug, StdFile, StdMisc //, graph_to_string_with_descriptors
import System.Time, System.File, System.FilePath

from Data.Map import newMap
from Data.SharedDataSource import qualified readRegister

//---------------------------------------------------------------------------------------

println :: !String !*IWorld -> *IWorld
println msg iw=:{world} 
	# (console,world)	= stdio world
	# console			= fwrites msg console
	# console			= fwrites "\n" console
	# (_,world)			= fclose console world
	= {iw & world = world} 

toDef c = UIControlSequence {UIControlSequence
							| attributes = newMap
							, controls	 = [(c,newMap)]
							, direction	 = Vertical
							}
	
mkInstanceId :: Task String
mkInstanceId = mkInstantTask taskFunc
where
	// TODO: generate actually unique id
	taskFunc _ iw=:{world} 
		# (c, world) = clock world
		= (Ok ("i" +++ toString c), {iw & world = world})

mkTask :: (TaskletInstance st res) -> Task res | JSONDecode{|*|} res & JSONEncode{|*|} res
mkTask ti = mkInterfaceTask ti []

mkTaskWithShared :: (TaskletInstance st res) !(Shared r) (r st -> st) -> Task res | JSONDecode{|*|} res & JSONEncode{|*|} res & iTask r
mkTaskWithShared (iid, tasklet) shared updateFunc = Task taskFunc
where
	// Init
	taskFunc event taskRepOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (rep, st, iworld) = genRep taskId taskRepOpts Nothing iworld
		# res = tasklet.Tasklet.resultFunc st

		# (val,iworld)	= 'Data.SharedDataSource'.readRegister instanceNo shared iworld
		# result = case val of
			Ok val		= ValueResult res (taskInfo ts) rep (TCInteract taskId ts (toJSON res) (toJSON val) (toJSON Void) Untouched)
			Error e		= exception (SharedException e)

		= (result, println "mkTaskWithShared: init" iworld)		

		// Refresh: server restart. anything else?
	taskFunc RefreshEvent taskRepOpts context=:(TCInteract taskId=:(TaskId instanceNo _) ts rJsonRes vJsonRes d1 d2) iworld
//		# (rep, st, iworld) = genRep taskId taskRepOpts Nothing iworld

		//No res from jsonRes! because state and value will be out of sync!
		# res = fromJust (fromJSON rJsonRes)
		# oldval = fromJust (fromJSON vJsonRes)
//		# res = tasklet.Tasklet.resultFunc st

		# (mbval,iworld)	= 'Data.SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbval		= (exception (fromError mbval),iworld)
		# val = fromOk mbval
		# valChanged = oldval =!= val

		# rep = placeHolderRep taskId (if valChanged (Just (graph_to_sapl_string val)) Nothing)

		# result = ValueResult res (taskInfo ts) rep (TCInteract taskId ts (toJSON res) (toJSON val) d1 d2)	
		= (result, println ("mkTaskWithShared: refresh") iworld)

	// Focus: tab switch. anything else?
	taskFunc (FocusEvent _) taskRepOpts context=:(TCInteract taskId=:(TaskId instanceNo _) ts rJsonRes vJsonRes _ _) iworld
		# res = fromJust (fromJSON (rJsonRes))
		# result = ValueResult res (taskInfo ts) (placeHolderRep taskId Nothing) context
		= (result, println "mkTaskWithShared: focus" iworld)
 
	// Edit: "result"
	taskFunc (EditEvent targetTaskId "result" jsonRes) taskRepOpts context=:(TCInteract taskId=:(TaskId instanceNo _) ts rJsonRes vJsonRes _ _) iworld
		# res = fromJust (fromJSON (rJsonRes))
		# result = ValueResult res (taskInfo ts) (placeHolderRep taskId Nothing) context
		= (result, println "mkTaskWithShared: result" iworld) 
 
	// Edit: "finalize"
	taskFunc (EditEvent targetTaskId "finalize" jsonRes) taskRepOpts (TCInteract taskId=:(TaskId instanceNo _) ts rJsonRes vJsonRes _ _) iworld
		# res = fromJust (fromJSON (rJsonRes))
		# rep = TaskRep (appTweak (ViewPart, Nothing, [], [])) []
		# result = DestroyedResult //ValueResult res (taskInfo ts) rep (TCDestroy (TCBasic taskId ts jsonRes False))
		= (result, println "mkTaskWithShared: finalize" iworld)  
 
	// Commit
	taskFunc event taskRepOpts context=:(TCInteract taskId=:(TaskId instanceNo _) ts rJsonRes vJsonRes _ _) iworld
		# res = fromJust (fromJSON (rJsonRes))
		# rep = placeHolderRep taskId Nothing
		# result = ValueResult res (taskInfo ts) rep context
		= (result, println "mkTaskWithShared: commit" iworld)

	// Destroy
	taskFunc event taskRepOpts (TCDestroy _) iworld
		= (DestroyedResult, println "mkTaskWithShared: destroy" iworld)

	taskInfo ts = {TaskInfo | lastEvent = ts, refreshSensitive = True} // expiresIn = Nothing}

	placeHolderRep taskId mbUpdateVal
		= TaskRep (toDef (UITaskletPH defaultSizeOpts {UITaskletPHOpts|taskId = toString taskId, iid = iid, updateVal = mbUpdateVal})) []

	genRep taskId taskRepOpts mbState iworld
		# (gui, state, iworld) = tasklet.generatorFunc iid taskId mbState iworld
		= case gui of
		
			TaskletHTML gui 

				# (state_js, script_js, events_js, intfcs_js, rf_js, _, mb_uf_js, iworld) 
						= linker state
							 (map interfaceWrapper []) 
							 (map eventHandlerWrapper gui.eventHandlers)
							 tasklet.Tasklet.resultFunc
						     Nothing
						     (Just updateFunc)
						     iworld
					
				# tui = tHTMLToTasklet gui taskId state_js script_js events_js intfcs_js rf_js mb_uf_js
				# rep = TaskRep (appTweak tui) []						
				= (rep, state, iworld)

			TaskletTUI gui

				# (mb_ino, mb_cf) = case gui.eventHandler of
						Just (iNo, eh) = (Just (toString iNo), Just eh)
									   = (Nothing , Nothing)

				# (state_js, script_js, _, _, rf_js, mb_cf_js, mb_uf_js, iworld) 
						= linker state
							 []
							 []
							 tasklet.Tasklet.resultFunc
						     (fmap controllerWrapper mb_cf)
						     (Just updateFunc)
						     iworld
					
				# tui = tTUIToTasklet gui taskId state_js script_js mb_ino rf_js mb_cf_js mb_uf_js
				# rep = TaskRep (appTweak tui) []							
				= (rep, state, iworld)

			NoGUI
			
				# (state_js, script_js, _, intfcs_js, rf_js,  _, mb_uf_js, iworld) 
					= linker state
							 (map interfaceWrapper []) 
							 []
							 tasklet.Tasklet.resultFunc
						     Nothing
						     (Just updateFunc)
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
							, updateFunc	 = mb_uf_js
							, instanceNo	 = Nothing
							, controllerFunc = Nothing})			
			
				# rep = TaskRep (appTweak tui) []				
				= (rep, state, iworld)

	tTUIToTasklet {TaskletTUI|tui} taskId state_js script_js mb_ino rf_js mb_cf_js mb_uf_js
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
					, updateFunc	 = mb_uf_js					
					, instanceNo	 = mb_ino
					, controllerFunc = mb_cf_js})

	tHTMLToTasklet {TaskletHTML|width,height,html} taskId state_js script_js events_js intfcs_js rf_js mb_uf_js
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
					 , updateFunc     = mb_uf_js
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

	taskInfo ts = {TaskInfo | lastEvent = ts, refreshSensitive = True} // expiresIn = Nothing}

	placeHolderRep taskId
		= TaskRep (toDef (UITaskletPH defaultSizeOpts {UITaskletPHOpts|taskId = toString taskId, iid = iid, updateVal = Nothing})) []

	genRep taskId taskRepOpts mbState iworld
		# (gui, state, iworld) = tasklet.generatorFunc iid taskId mbState iworld
		= case gui of
		
			TaskletHTML gui 

				# (state_js, script_js, events_js, intfcs_js, rf_js, _, _, iworld) 
					= linker state
							 (map interfaceWrapper fs) 
							 (map eventHandlerWrapper gui.eventHandlers)
							 tasklet.Tasklet.resultFunc
						     Nothing
						     Nothing
						     iworld
					
				# tui = tHTMLToTasklet gui taskId state_js script_js events_js intfcs_js rf_js
				# rep = TaskRep (appTweak tui) []						
				= (rep, state, iworld)

			TaskletTUI gui

				# (mb_ino, mb_cf) = case gui.eventHandler of
						Just (iNo, eh) = (Just (toString iNo), Just eh)
									   = (Nothing , Nothing)

				# (state_js, script_js, _, _, rf_js, mb_cf_js, _, iworld) 
					= linker state
							 []
							 []
							 tasklet.Tasklet.resultFunc
						     (fmap controllerWrapper mb_cf)
						     Nothing
						     iworld
					
				# tui = tTUIToTasklet gui taskId state_js script_js mb_ino rf_js mb_cf_js
				# rep = TaskRep (appTweak tui) []							
				= (rep, state, iworld)

			NoGUI
			
				# (state_js, script_js, _, intfcs_js, rf_js,  _, _, iworld) 
					= linker state
							 (map interfaceWrapper fs) 
							 []
							 tasklet.Tasklet.resultFunc
						     Nothing
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
							, updateFunc     = Nothing
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
					, updateFunc     = Nothing
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
					 , updateFunc     = Nothing
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

linker_update updateFunc iworld=:{world,sdkDirectory}

	/* 0. Load Clean flavour */
	
	# flavfile = sdkDirectory </> "Server" </> "lib" </> "SAPL" </>"clean.f"
	
	# (flavres, world) = readFile flavfile world
	| isError flavres
		= abort ("Flavour file cannot be found at " +++ flavfile)

	# mbFlav = toFlavour (fromOk flavres)
	| isNothing mbFlav
		= abort "Error in flavour file"

	# (ls, world) = generateLoaderState ["sapl"] [] world

	// link functions indicated by result updateFunc
	# saplUF = graph_to_sapl_string updateFunc
	# (ls, a, saplUF, world) = linkByExpr ls newAppender saplUF world

	# sapl = toString a	
	# (script, mbPst) = case sapl of
		"" = ("", Nothing)
		   = let (script, pst) = handlerr (generateJS (fromJust mbFlav) False sapl) in (toString script, Just pst)
	
	# ufjs = toString (handlerr (exprGenerateJS (fromJust mbFlav) False saplUF mbPst))			
	= (script, ufjs, {iworld & world=world})

linker state interfaceFuns eventHandlers resultFunc mbControllerFunc mbUpdateFunc iworld=:{world,sdkDirectory}
	
	/* 0. Load Clean flavour */
	
	# flavfile = sdkDirectory </> "Server" </> "lib" </> "SAPL" </>"clean.f"
	
	# (flavres, world) = readFile flavfile world
	| isError flavres
		= abort ("Flavour file cannot be found at " +++ flavfile)

	# mbFlav = toFlavour (fromOk flavres)
	| isNothing mbFlav
		= abort "Error in flavour file"
	
	/* 1. First, we collect all the necessary function definitions to generate ParserState */

	# (ls, world) = generateLoaderState ["sapl"] [] world
	// link functions indicated by the state structure
	# saplst = graph_to_sapl_string state
	# (ls, a, saplst, world) = linkByExpr ls newAppender saplst world

	// link functions indicated by result func
	# saplRF = graph_to_sapl_string resultFunc
	# (ls, a, saplRF, world) = linkByExpr ls a saplRF world

	// link functions indicated by update func (if given)
	# (ls, a, mbSaplUF, world) = case mbUpdateFunc of
		Just uf # saplUF = graph_to_sapl_string uf
				# (ls, a, _, world) = linkByExpr ls a saplUF world
				= (ls, a, Just saplUF, world)
				= (ls, a, Nothing, world)				
				
	// link functions indicated by controller func (if given)
	# (ls, a, mbSaplCF, world) = case mbControllerFunc of
		Just cf # saplCF = graph_to_sapl_string cf
				# (ls, a, saplCF, world) = linkByExpr ls a saplCF world
				= (ls, a, Just saplCF, world)
				= (ls, a, Nothing,  world)
				
	// link functions indicated by event handlers
	# (ls, a, eventHandlers, world) = foldl (\(ls, a, hs, world) (e1,e2,f) = 
				let (ls2, a2, f2, world2) = linkByExpr ls a (graph_to_sapl_string f) world
				 in (ls2, a2, [(e1,e2,f2):hs], world2)) 
			(ls, a, [], world) eventHandlers

	// link functions indicated by event handlers
	# (ls, a, interfaceFuns, world) = foldl (\(ls, a, hs, world) (fn, f) = 
				let (ls2, a2, f2, world2) = linkByExpr ls a (graph_to_sapl_string f) world
				 in (ls2, a2, [(fn,f2):hs], world2)) 
			(ls, a, [], world) interfaceFuns

	/* 2. Generate function definitions and ParserState */

	# sapl = toString a	
	# (script, mbPst) = case sapl of
		"" = ("", Nothing)
		   = let (script, pst) = handlerr (generateJS (fromJust mbFlav) False sapl) in (toString script, Just pst)
	
	/* 3. Generate expressions by ParserState */
									
	# statejs = toString (handlerr (exprGenerateJS (fromJust mbFlav) False saplst mbPst))

	# events = map (\(id,event,saplhandler) = (id,event,toString (handlerr 
				(exprGenerateJS (fromJust mbFlav) False saplhandler mbPst)))) eventHandlers
	
	# intfcs = map (\(fn,saplfun) = (fn, toString (handlerr 
				(exprGenerateJS (fromJust mbFlav) False saplfun mbPst)))) interfaceFuns	// No trampolining
	
	# rfjs = toString (handlerr (exprGenerateJS (fromJust mbFlav) False saplRF mbPst))			
	
	# cfjs = case mbSaplCF of
		Just saplCF = Just (toString (handlerr (exprGenerateJS (fromJust mbFlav) False saplCF mbPst)))
					= Nothing		

	# ufjs = case mbSaplUF of
		Just saplUF = Just (toString (handlerr (exprGenerateJS (fromJust mbFlav) False saplUF mbPst)))
					= Nothing		


/* For debugging:

	# (_, world) = writeFile "debug_state.sapl" saplst world
	# (_, world) = writeFile "debug_state.js" statejs world	
	# (_, world) = writeFile "debug.sapl" sapl world
	# (_, world) = writeFile "debug.js" script world
*/

	= (statejs, script, events, intfcs, rfjs, cfjs, ufjs, {iworld & world=world})
 