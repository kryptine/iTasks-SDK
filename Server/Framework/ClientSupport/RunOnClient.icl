implementation module RunOnClient

import StdMisc
import iTasks, Task, Tasklet, TaskState, TaskStore, TaskEval, UIDefinition

:: TaskState :== Maybe (TIMeta,TIReduct,TIResult)

runOnClient :: !(Task m) -> Task m | iTask m
runOnClient task

	# roc_tasklet =
		{ Tasklet 
		| generatorFunc		= roc_generator task
		, resultFunc		= toResult
		, tweakUI			= id
		}

	= mkTask roc_tasklet
				
toResult Nothing
	= NoValue	

toResult (Just (_,_,TIValue NoValue _))
	= NoValue
	
toResult (Just (_,_,TIValue (Value json stab) _))
	= Value (fromJust (fromJSON json)) stab

roc_generator :: !(Task m) !TaskId (Maybe TaskState) !*IWorld -> *(!TaskletGUI TaskState, !TaskState, !*IWorld) | iTask m
roc_generator task (TaskId instanceNo taskNo) _ iworld

	# (res, iworld) = loadTaskInstance instanceNo iworld
	# (ameta, _, _) = fromOk res
	
	# (newInstanceNo, iworld) = newInstanceId iworld
	# ((meta, reduct, taskres, rep), iworld) = 
		createTaskInstance newInstanceNo ameta.sessionId instanceNo ameta.TIMeta.worker task ameta.management ameta.TIMeta.progress iworld
	
	// Initialize embedded task
	# (meta, reduct, taskres, result, iworld) = evalInstance RefreshEvent (meta, reduct, taskres) iworld
	
	# mbUI = case result of
		(ValueResult _ _ (TaskRep uidef _) _) = Just uidef
											  = Nothing
	
	# gui = TaskletTUI { TaskletTUI
	  			 	   | tui  			= mbUI
	  			  	   , eventHandler	= Just (newInstanceNo, controllerFunc)
	  			       } 	
	
	= (gui, Just (meta, reduct, taskres), iworld)

// Init
controllerFunc taskId st Nothing Nothing
	= (Nothing, st)

// Commit
controllerFunc taskId st (Just eventName) Nothing
	= controllerFunc` taskId st (ActionEvent taskId eventName)

// Edit
controllerFunc taskId st (Just eventName) (Just eventValue)
	= controllerFunc` taskId st (EditEvent taskId eventName (fromString eventValue))

controllerFunc` taskId st event
	# iworld = createClientIWorld

	# (meta, reduct, taskres, result, iworld) = 
		evalInstance event (fromJust st) iworld
		
	# mbTUI = case result of
		(ValueResult _ _ (TaskRep uidef _) _) = Just uidef
											  = Nothing
											  
	= (mbTUI, Just (meta, reduct, taskres))

createClientIWorld :: *IWorld
createClientIWorld
		= {IWorld
		  |application			= locundef "application"
		  ,build				= locundef "build"
		  ,appDirectory			= locundef "appDirectory"
		  ,sdkDirectory			= locundef "sdkDirectory"
		  ,dataDirectory		= locundef "dataDirectory"
		  ,config				= locundef "config"
		  ,taskTime				= locundef "taskTime"
		  ,timestamp			= locundef "timestamp"
		  ,currentDateTime		= locundef "currentDateTime"
		  ,currentUser			= locundef "currentUser"
		  ,currentInstance		= locundef "currentInstance"
		  ,nextTaskNo			= locundef "nextTaskNo"
		  ,localShares			= locundef "localShares"
		  ,localLists			= locundef "localLists"
		  ,readShares			= locundef "readShares"
		  ,sessions				= locundef "sessions"
		  ,uis					= locundef "uis"
		  ,workQueue			= locundef "workQueue" 		  
		  ,world				= locundef "world"
		  }
where
	locundef var = abort ("IWorld structure is not avalaible at client side. Reference: "+++var)

/**
* Evaluate a single task instance by TaskEval.evalAndStoreInstance
*/
evalInstance :: !Event !(TIMeta,TIReduct,TIResult) !*IWorld -> (!TIMeta,!TIReduct,!TIResult,!TaskResult JSONNode,!*IWorld)
evalInstance event (meta=:{TIMeta|instanceNo,parent,worker=Just worker,progress},reduct=:{TIReduct|task=Task eval,nextTaskNo=curNextTaskNo,nextTaskTime,tree,shares,lists},result=:TIValue val _) iworld=:{currentUser,currentInstance,nextTaskNo,taskTime,localShares,localLists}
	//Eval instance
	# repAs						= {TaskRepOpts|useLayout=Nothing,afterLayout=Nothing,modLayout=Nothing,appFinalLayout=True}
	//Update current process id & eval stack in iworld
	# taskId					= TaskId instanceNo 0
	# iworld					= {iworld & currentInstance = instanceNo, currentUser = worker, nextTaskNo = curNextTaskNo, taskTime = nextTaskTime, localShares = shares, localLists = lists} 
	//Apply task's eval function and take updated nextTaskId from iworld
	# (result,iworld)			= eval event repAs tree iworld
	# (updNextTaskNo,iworld)	= getNextTaskNo iworld
	# (shares,iworld)			= getLocalShares iworld
	# (lists,iworld)			= getLocalLists iworld
	//Restore current process id, nextTask id and local shares in iworld
	# iworld					= {iworld & currentInstance = currentInstance, currentUser = currentUser, nextTaskNo = nextTaskNo, taskTime = taskTime, localShares = localShares, localLists = localLists}
	# reduct					= {TIReduct|reduct & nextTaskNo = updNextTaskNo, nextTaskTime = nextTaskTime + 1, tree = tasktree result, shares = shares, lists = lists}
	# meta						= {TIMeta|meta & progress = setStatus result progress}
	= (meta, reduct, taskres result, result, iworld)
where
	getNextTaskNo iworld=:{IWorld|nextTaskNo}	= (nextTaskNo,iworld)
	getLocalShares iworld=:{IWorld|localShares}	= (localShares,iworld)
	getLocalLists iworld=:{IWorld|localLists}	= (localLists,iworld)

	setStatus (ExceptionResult _ _) meta				= {meta & status = Stable}
	setStatus (ValueResult (Value _ Stable) _ _ _) meta	= {meta & status = Stable}
	setStatus _	meta									= {meta & status = Unstable}

	tasktree (ValueResult _ _ _ tree)	= tree
	tasktree (ExceptionResult _ _)		= TCNop
	
	taskres (ValueResult val {TaskInfo|lastEvent} _ _)	= TIValue val lastEvent
	taskres (ExceptionResult e str)						= TIException e str

evalInstance _ (meta,reduct,TIException e msg) iworld
	= (meta, reduct, TIException e msg, ExceptionResult e msg, iworld)

evalInstance _ _ iworld
	= abort "Could not unpack instance state"
	