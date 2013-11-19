implementation module iTasks.API.Core.Client.Tasklet

import iTasks, iTasks.Framework.Task, iTasks.Framework.TaskState, iTasks.Framework.UIDefinition
import StdFile, StdMisc
import System.Time, System.File, System.FilePath
import iTasks.Framework.Client.LinkerSupport

import iTasks.Framework.Client.Tasklet
import iTasks.API.Core.Client.Interface

import Data.Functor, Data.Error

from Data.SharedDataSource import qualified readRegister
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

createTaskletEventHandler :: (TaskletEventHandlerFunc a) !TaskId -> (JSVal (JSFunction b)) 
createTaskletEventHandler origHandler taskId = undef

fireEvent :: !*EventQueue !TaskId !String a -> *EventQueue
fireEvent eventqueue taskId eventName eventValue = undef

//---------------------------------------------------------------------------------------
	
mkTask :: (Tasklet st res) -> Task res | iTask res
mkTask ti = mkInterfaceTask ti []

/*
mkTaskWithShared :: (Tasklet st res) !(Shared r) (r st -> st) -> Task res | iTask res & iTask r
mkTaskWithShared tasklet shared updateFunc = Task taskFunc
where

	// Init: no session id
//	taskFunc event taskRepOpts context=:(TCInit _ ts) iworld=:{currentSession=Nothing}
//		= (ValueResult NoValue (taskInfo ts) NoRep context, iworld)

	// Init
	taskFunc event taskRepOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld

		# (mbval,iworld)	= 'Data.SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbval		= (exception (fromError mbval),iworld)
		# val = fromOk mbval

		# (rep, st, iworld) = genRep tasklet taskId taskRepOpts Nothing (Just updateFunc) (Just val) iworld
		# res = tasklet.Tasklet.resultFunc st

		# result = ValueResult res (taskInfo ts) rep (TCInteract taskId ts (toJSON res) (toJSON val) (toJSON Void) Untouched)
		= (result, printlnI ("mkTaskWithShared ("+++toString taskId+++"): init") iworld)		

	// Refresh: no session id
//	taskFunc (RefreshEvent _) taskRepOpts context=:(TCInteract _ ts _ _ _ _) iworld=:{currentSession=Nothing}
//		= (ValueResult NoValue (taskInfo ts) NoRep context, iworld)

	// Refresh: server restart, shared changes
	taskFunc (RefreshEvent _) taskRepOpts context=:(TCInteract taskId=:(TaskId instanceNo _) ts rJsonRes vJsonRes d1 d2) iworld
		# res = fromJust (fromJSON rJsonRes)
		# oldval = fromJust (fromJSON vJsonRes)

		# (mbval,iworld)	= 'Data.SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbval		= (exception (fromError mbval),iworld)
		# val = fromOk mbval

		# (rep, iworld) = case oldval =!= val of
				True # (js, iworld) = taskletUpdateLinker val iworld
					 = (placeHolderRep taskId (Just js), iworld)
					 = (placeHolderRep taskId Nothing, iworld)

		# result = ValueResult res (taskInfo ts) rep (TCInteract taskId ts (toJSON res) (toJSON val) d1 d2)	
		= (result, printlnI ("mkTaskWithShared ("+++toString taskId+++"): refresh") iworld)

	// Focus: tab switch. anything else?
	taskFunc (FocusEvent _ _) taskRepOpts context=:(TCInteract taskId=:(TaskId instanceNo _) ts rJsonRes vJsonRes _ _) iworld
		# res = fromJust (fromJSON (rJsonRes))
		# result = ValueResult res (taskInfo ts) (placeHolderRep taskId Nothing) context
		= (result, printlnI ("mkTaskWithShared ("+++toString taskId+++"): focus") iworld)
  
	// Edit: "result"
	taskFunc (EditEvent _ targetTaskId "result" jsonRes) taskRepOpts context=:(TCInteract taskId=:(TaskId instanceNo _) ts _ vJsonRes d1 d2) iworld
		| targetTaskId == taskId
			# res = fromJust (fromJSON (jsonRes))
			# result = ValueResult res (taskInfo ts) (placeHolderRep taskId Nothing) (TCInteract taskId ts jsonRes vJsonRes d1 d2)
			= (result, printlnI ("mkTaskWithShared ("+++toString taskId+++"): result") iworld) 
 
	// Edit: "finalize"
	taskFunc (EditEvent _ targetTaskId "finalize" jsonRes) taskRepOpts (TCInteract taskId=:(TaskId instanceNo _) ts rJsonRes vJsonRes _ _) iworld
		| targetTaskId == taskId	
			# res = fromJust (fromJSON (jsonRes))
			# rep = TaskRep (appTweak (ViewPart, Nothing, [], [])) []
			# result = DestroyedResult //ValueResult res (taskInfo ts) rep (TCDestroy (TCBasic taskId ts jsonRes False))
			= (result, printlnI ("mkTaskWithShared ("+++toString taskId+++"): finalize") iworld)  
  
	// Do nothing. commit or wrong traget
	taskFunc event taskRepOpts context=:(TCInteract taskId=:(TaskId instanceNo _) ts rJsonRes vJsonRes _ _) iworld
		# res = fromJust (fromJSON (rJsonRes))
		# rep = placeHolderRep taskId Nothing
		# result = ValueResult res (taskInfo ts) rep context
		= (result, printlnI ("mkTaskWithShared ("+++toString taskId+++"): skip") iworld)

	// Destroy
	taskFunc event taskRepOpts (TCDestroy _) iworld
		= (DestroyedResult, printlnI "mkTaskWithShared: destroy" iworld)
*/

mkInterfaceTask :: (Tasklet st res) [InterfaceFun st] -> Task res | JSONDecode{|*|} res & JSONEncode{|*|} res
mkInterfaceTask tasklet fs = Task taskFunc
where

	norep = TaskRep (toDef (stringDisplay "nothing")) []

	// Init: no session id
//	taskFunc event taskRepOpts context=:(TCInit _ ts) iworld=:{currentSession=Nothing}
//		= (ValueResult NoValue (taskInfo ts) norep context, printlnI ("init, no session id") iworld)

	// Init
	taskFunc event taskRepOpts (TCInit taskId ts) iworld
		# (rep, st, iworld) = genRep tasklet taskId taskRepOpts Nothing Nothing Nothing iworld
		# res = tasklet.Tasklet.resultFunc st
		# result = ValueResult res (taskInfo ts) rep (TCBasic taskId ts (toJSON res) False)
		= (result, printlnI ("init") iworld) 

	// Refresh: no session id
//	taskFunc (RefreshEvent _) taskRepOpts context=:(TCBasic _ ts _ _) iworld=:{currentSession=Nothing}
//		= (ValueResult NoValue (taskInfo ts) norep context, printlnI ("refresh, no session id") iworld)

	// Refresh: server restart. anything else?
	taskFunc (RefreshEvent _) taskRepOpts context=:(TCBasic taskId ts jsonRes _) iworld
		//# (rep, st, iworld) = genRep taskId taskRepOpts Nothing iworld

		//No! because state and value will be out of sync!
		# res = fromJust (fromJSON (jsonRes))
		//# res = tasklet.Tasklet.resultFunc st
		# rep = placeHolderRep taskId Nothing
		
		# result = ValueResult res (taskInfo ts) rep context
		= (result, printlnI "refresh" iworld)

	// Focus: tab switch. anything else?
	taskFunc (FocusEvent _ _) taskRepOpts context=:(TCBasic taskId ts jsonRes _) iworld
		# res = fromJust (fromJSON (jsonRes))
		# result = ValueResult res (taskInfo ts) (placeHolderRep taskId Nothing) context
		= (result, printlnI "focus" iworld)
 
	// Edit: "result"
	taskFunc (EditEvent _ targetTaskId "result" jsonRes) taskRepOpts (TCBasic taskId ts _ _) iworld
		| targetTaskId == taskId	
			# res = fromJust (fromJSON (jsonRes))
			# result = ValueResult res (taskInfo ts) (placeHolderRep taskId Nothing) (TCBasic taskId ts jsonRes False)
			= (result, printlnI "result" iworld) 
 
	// Edit: "finalize"
	taskFunc (EditEvent _ targetTaskId "finalize" jsonRes) taskRepOpts (TCBasic taskId ts _ _) iworld
		| targetTaskId == taskId	
			# res = fromJust (fromJSON (jsonRes))
			# rep = TaskRep (appTweak (ViewPart, Nothing, [], [])) []
			# result = DestroyedResult //ValueResult res (taskInfo ts) rep (TCDestroy (TCBasic taskId ts jsonRes False))
			= (result, printlnI "finalize" iworld)  
 
	// Commit
	taskFunc event taskRepOpts (TCBasic taskId ts jsonRes _) iworld
		# res = fromJust (fromJSON (jsonRes))
		# rep = placeHolderRep taskId Nothing
		# result = ValueResult res (taskInfo ts) rep (TCBasic taskId ts jsonRes False)
		= (result, printlnI "commit" iworld)

	// Destroy
	taskFunc event taskRepOpts (TCDestroy _) iworld
		= (DestroyedResult, printlnI "destroy" iworld)

//---------------------------------------------------------------------------------------

placeHolderRep taskId mbUpdateVal
	= TaskRep (toDef (UITaskletPH defaultSizeOpts {UITaskletPHOpts|taskId = toString taskId, updateVal = mbUpdateVal})) []

genRep tasklet taskId taskRepOpts mbState mbUpdateFunc mbUpdateVal iworld
	# (gui, state, iworld) = tasklet.Tasklet.genUI taskId mbState iworld
	= case gui of
		
			TaskletHTML gui 

				# (state_js, script_js, events_js, intfcs_js, rf_js, _, mb_uf_js, iworld) 
						= taskletLinker state
							 (map (interfaceWrapper taskId) []) 
							 (map (eventHandlerWrapper taskId) gui.eventHandlers)
							 tasklet.Tasklet.resultFunc
						     Nothing
						     mbUpdateFunc
						     mbUpdateVal
						     iworld
					
				# tui = tHTMLToTasklet gui taskId state_js script_js events_js intfcs_js rf_js mb_uf_js
				# rep = TaskRep (appTweak tasklet tui) []						
				= (rep, state, iworld)

			TaskletTUI gui

				# (mb_ino, mb_cf) = case gui.eventHandler of
						Just (iNo, eh) = (Just (toString iNo), Just eh)
									   = (Nothing , Nothing)

				# (state_js, script_js, _, _, rf_js, mb_cf_js, mb_uf_js, iworld) 
						= taskletLinker state
							 []
							 []
							 tasklet.Tasklet.resultFunc
						     (fmap controllerWrapper mb_cf)
						     mbUpdateFunc
						     mbUpdateVal
						     iworld
					
				# tui = tTUIToTasklet gui taskId state_js script_js mb_ino rf_js mb_cf_js mb_uf_js
				# rep = TaskRep (appTweak tasklet tui) []							
				= (rep, state, iworld)

			NoGUI
			
				# (state_js, script_js, _, intfcs_js, rf_js,  _, mb_uf_js, iworld) 
					= taskletLinker state
							 (map (interfaceWrapper taskId) []) 
							 []
							 tasklet.Tasklet.resultFunc
						     Nothing
						     mbUpdateFunc
						     mbUpdateVal
						     iworld			
			
				# tui = toDef (UITasklet defaultSizeOpts 
				 			{UITaskletOpts 
				 			| taskId   		 = toString taskId
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
			
				# rep = TaskRep (appTweak tasklet tui) []				
				= (rep, state, iworld)
where
	tTUIToTasklet {TaskletTUI|tui} taskId state_js script_js mb_ino rf_js mb_cf_js mb_uf_js
		 = toDef (UITasklet defaultSizeOpts 
		 			{UITaskletOpts 
		 			| taskId   		 = toString taskId
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

	tHTMLToTasklet {ComponentHTML|width,height,html} taskId state_js script_js events_js intfcs_js rf_js mb_uf_js
		= toDef (setSize width height 
				(UITasklet defaultSizeOpts 
					 {UITaskletOpts 
					 | taskId   	  = toString taskId
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

taskInfo ts = {TaskInfo | lastEvent = ts, refreshSensitive = True}

appTweak tasklet taskTuiRep = tweakUI tasklet.tweakUI taskTuiRep

toDef c = UIControlStack {UIControlStack
						 | attributes = newMap
						 , controls	 = [(c, newMap)]
						 }

/* Controller wrapper to be easier to write controller function:
 * 1. taskId is parsed
 * 2. TUI result is stringified 
 */
controllerWrapper cf strTaskID st eventNo mbEventName mbEventHandler iworld
	# (mbTUI, st, iworld) = cf (fromString strTaskID) st eventNo mbEventName mbEventHandler iworld
	= (fmap (toString o encodeUIDefinition) mbTUI, st, iworld)

// it uses the 2. layer (handleJSEvent), because it's created on the server
eventHandlerWrapper taskId (ComponentEvent id event f) 
	= (id, event, handleJSEvent f taskId)

interfaceWrapper taskId (InterfaceFun fn f) = (fn, handleInterfaceCall f (toString taskId))


