implementation module iTasks.API.Core.Client.Tasklet

import iTasks, iTasks._Framework.Task, iTasks._Framework.TaskState, iTasks.UI.Diff
import StdFile, StdMisc
import System.Time, System.File, System.FilePath
import iTasks._Framework.Client.LinkerSupport

import iTasks._Framework.Client.Tasklet
import iTasks.UI.JS.Interface

import Data.Functor, Data.Error

from Data.Map import newMap

:: EventQueue :== Void

//---------------------------------------------------------------------------------------

printlnI :: !String !*IWorld -> *IWorld
printlnI msg iw=:{world} 
	# (console,world)	= stdio world
	# console			= fwrites msg console
	# console			= fwrites "\n" console
	# (_,world)			= fclose console world
	= {iw & world = world} 

println :: !String !*World -> *World
println msg world 
	# (console,world)	= stdio world
	# console			= fwrites msg console
	# console			= fwrites "\n" console
	# (_,world)			= fclose console world
	= world

//---------------------------------------------------------------------------------------

createTaskletEventHandler :: (TaskletEventHandlerFunc a) !TaskId -> JSFun b
createTaskletEventHandler origHandler taskId = undef

fireEvent :: !*EventQueue !TaskId !String a -> *EventQueue
fireEvent eventqueue taskId eventName eventValue = undef

//---------------------------------------------------------------------------------------
	
mkTask :: (Tasklet st res) -> Task res | iTask res
mkTask ti = mkInterfaceTask ti []

mkInterfaceTask :: (Tasklet st res) [InterfaceFun st] -> Task res | JSONDecode{|*|} res & JSONEncode{|*|} res
mkInterfaceTask tasklet fs = Task taskFunc
where

	norep = TaskRep (toDef (stringDisplay "nothing"))

	// Init: no session id
//	taskFunc event taskRepOpts context=:(TCInit _ ts) iworld=:{currentSession=Nothing}
//		= (ValueResult NoValue (taskInfo ts) norep context, printlnI ("init, no session id") iworld)

	// Init
	taskFunc event evalOpts (TCInit taskId ts) iworld
		# (rep, st, iworld) = genRep tasklet taskId evalOpts Nothing iworld
		# res = tasklet.Tasklet.resultFunc st
		# result = ValueResult res (taskInfo ts) rep (TCBasic taskId ts (toJSON res) False)
		= (result, printlnI ("init") iworld) 

	// Refresh: no session id
//	taskFunc (RefreshEvent _) taskRepOpts context=:(TCBasic _ ts _ _) iworld=:{currentSession=Nothing}
//		= (ValueResult NoValue (taskInfo ts) norep context, printlnI ("refresh, no session id") iworld)

	// Refresh: server restart. anything else?
	taskFunc (RefreshEvent _) taskRepOpts context=:(TCBasic taskId ts jsonRes _) iworld
		# (rep, _, iworld) = genRep tasklet taskId taskRepOpts Nothing iworld

		//No! because state and value will be out of sync!
		# res = fromJust (fromJSON (jsonRes))
		//# res = tasklet.Tasklet.resultFunc st
		
		# result = ValueResult res (taskInfo ts) rep context
		= (result, printlnI "refresh" iworld)

	// Focus: tab switch. anything else?
	taskFunc (FocusEvent _) taskRepOpts context=:(TCBasic taskId ts jsonRes _) iworld
		# (rep, _, iworld) = genRep tasklet taskId taskRepOpts Nothing iworld	
		# res = fromJust (fromJSON (jsonRes))
		# result = ValueResult res (taskInfo ts) rep context
		= (result, printlnI "focus" iworld)
 
	// Edit: "result"
	taskFunc (EditEvent targetTaskId "result" jsonRes) taskRepOpts (TCBasic taskId ts _ _) iworld
		| targetTaskId == taskId
			# (rep, _, iworld) = genRep tasklet taskId taskRepOpts Nothing iworld			
			# res = fromJust (fromJSON (jsonRes))
			# result = ValueResult res (taskInfo ts) rep (TCBasic taskId ts jsonRes False)
			= (result, printlnI "result" iworld) 
 
	// Edit: "finalize"
	taskFunc (EditEvent targetTaskId "finalize" jsonRes) taskRepOpts (TCBasic taskId ts _ _) iworld
		| targetTaskId == taskId	
			# res = fromJust (fromJSON (jsonRes))
			# result = DestroyedResult
			= (result, printlnI "finalize" iworld)  
 
	// Commit
	taskFunc event taskRepOpts (TCBasic taskId ts jsonRes _) iworld
		# (rep, _, iworld) = genRep tasklet taskId taskRepOpts Nothing iworld
		# res = fromJust (fromJSON (jsonRes))
		# result = ValueResult res (taskInfo ts) rep (TCBasic taskId ts jsonRes False)
		= (result, printlnI "commit" iworld)

	// Destroy
	taskFunc event taskRepOpts (TCDestroy _) iworld
		= (DestroyedResult, printlnI "destroy" iworld)

//---------------------------------------------------------------------------------------

genRep tasklet taskId taskRepOpts mbState iworld
	# (gui, state, iworld) = tasklet.Tasklet.genUI taskId mbState iworld
	= case gui of
		
			TaskletHTML gui 

				# (state_js, script_js, events_js, intfcs_js, rf_js, _, iworld) 
						= taskletLinker state
							 (map (interfaceWrapper taskId) []) 
							 (map (eventHandlerWrapper taskId) gui.TaskletHTML.eventHandlers)
							 tasklet.Tasklet.resultFunc
						     Nothing
						     iworld
					
				# tui = tHTMLToTasklet gui taskId state_js script_js events_js intfcs_js rf_js
				# rep = TaskRep (appTweak tasklet tui) NoChange
				= (rep, state, iworld)

			TaskletTUI gui

				# mb_ino = Just (toString gui.TaskletTUI.instanceNo)
				# mb_cf  = Just gui.TaskletTUI.controllerFunc

				# (state_js, script_js, _, _, rf_js, mb_cf_js, iworld) 
						= taskletLinker state
							 []
							 []
							 tasklet.Tasklet.resultFunc
						     (fmap controllerWrapper mb_cf)
						     iworld
					
				# tui = tTUIToTasklet taskId state_js script_js mb_ino rf_js mb_cf_js
				# rep = TaskRep (appTweak tasklet tui) NoChange
				= (rep, state, iworld)

			NoGUI
			
				# (state_js, script_js, _, intfcs_js, rf_js,  _, iworld) 
					= taskletLinker state
							 (map (interfaceWrapper taskId) []) 
							 []
							 tasklet.Tasklet.resultFunc
						     Nothing
						     iworld			
			
				# tui = toDef (UITasklet defaultSizeOpts 
				 			{UITaskletOpts 
				 			| taskId   		 = toString taskId
							, html     		 = Nothing
							, st    		 = Just state_js
							, script   		 = Just script_js
							, events   		 = Nothing
							, interfaceFuncs = Just intfcs_js
							, resultFunc	 = Just rf_js
							, instanceNo	 = Nothing
							, controllerFunc = Nothing})			
			
				# rep = TaskRep (appTweak tasklet tui) NoChange
				= (rep, state, iworld)
where
	tTUIToTasklet taskId state_js script_js mb_ino rf_js mb_cf_js
		 = toDef (UITasklet defaultSizeOpts 
		 			{UITaskletOpts 
		 			| taskId   		 = toString taskId
					, html     		 = Nothing
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
					 , html     	  = Just (toString html)
					 , st    		  = Just state_js
					 , script   	  = Just script_js
					 , events   	  = Just events_js
					 , interfaceFuncs = Just intfcs_js
					 , resultFunc     = Just rf_js
					 , instanceNo     = Nothing
					 , controllerFunc = Nothing}))

taskInfo ts = {TaskEvalInfo|lastEvent = ts, removedTasks=[], refreshSensitive = True}

appTweak tasklet taskTuiRep = tweakUI tasklet.tweakUI taskTuiRep

toDef c = UIForm
            {UIForm
			| attributes = newMap
			, controls	 = [(c, newMap)]
            , size = defaultSizeOpts
			}

/* Controller wrapper to be easier to write controller function:
 * 1. taskId is parsed
 * 2. TUI result is stringified 
 */
controllerWrapper cf strTaskID st eventNo mbEventName mbEventHandler iworld
	# (mbTUI, st, iworld) = cf (fromString strTaskID) st eventNo mbEventName mbEventHandler iworld
	= (fmap (toString o encodeUIUpdates) mbTUI, st, iworld)

// it uses the 2. layer (handleJSEvent), because it's created on the server
eventHandlerWrapper taskId (TaskletEvent id event f) 
	= (id, event, handleJSEvent f taskId)

interfaceWrapper taskId (InterfaceFun fn f) = (fn, handleInterfaceCall f (toString taskId))


