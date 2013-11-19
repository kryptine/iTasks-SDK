implementation module iTasks.Framework.Client.RunOnClient

import StdMisc
import iTasks//, Task, Tasklet, TaskState, TaskStore, TaskEval, UIDefinition
import iTasks.Framework.TaskStore
import iTasks.Framework.TaskEval
import iTasks.API.Core.Client.Tasklet

from Data.Map import newMap, toList
from Data.List import find

import System.Time
import Text.JSON

:: TaskState a = 
			{ instanceNo :: !InstanceNo
			, sessionId  :: !SessionId
			, taskId     :: !Maybe TaskId
			, task		 :: !Task a			
			, value		 :: !Maybe (TaskValue JSONNode)
			}

runOnClient :: !(Task m) -> Task m | iTask m
runOnClient task

	# roc_tasklet =
		{ Tasklet 
		| genUI				= roc_generator task
		, resultFunc		= gen_res
		, tweakUI			= id
		}
 
	= mkTask roc_tasklet

gen_res {TaskState|value=Nothing} = NoValue
gen_res {TaskState|value=Just NoValue} = NoValue
gen_res {TaskState|value=Just (Value json stability)} = Value (fromJust (fromJSON json)) stability

roc_generator :: !(Task m) !TaskId (Maybe (TaskState m)) !*IWorld -> *(!TaskletGUI (TaskState m), !TaskState m, !*IWorld) | iTask m
roc_generator task (TaskId instanceNo _) _ iworld=:{currentSession,sessions}

	# currentInstance = fromJust currentSession
	# currentSession = fst (fromJust (find (\(sessionId,instanceId) -> instanceId == currentInstance) (toList sessions)))

	# gui = TaskletTUI {tui = Nothing, eventHandler = Just (instanceNo, controllerFunc)}
	
	# state = 	{ TaskState
				| instanceNo = instanceNo
				, sessionId  = currentSession
				, taskId 	 = Nothing
				, task		 = task
				, value 	 = Nothing}
	
	= (gui, state, iworld)

// Init
controllerFunc _ st=:{TaskState | sessionId, instanceNo, task, taskId = Nothing} eventNo Nothing Nothing iworld
	# (taskId, iworld)  = createClientTaskInstance task sessionId instanceNo iworld
	# (mbResult,iworld)	= evalSessionTaskInstance sessionId (RefreshEvent Nothing) iworld
	= case mbResult of
		Ok (ValueResult _ _ (TaskRep def _) _, _, _, _) 
					= (Just def, {TaskState | st & taskId = Just taskId}, iworld)
		_			= (Nothing, {TaskState | st & taskId = Just taskId}, iworld)

// Refresh
controllerFunc _ st=:{TaskState | sessionId, instanceNo, task} eventNo Nothing Nothing iworld
	# (mbResult,iworld)	= evalSessionTaskInstance sessionId (RefreshEvent Nothing) iworld
	= case mbResult of
		Ok (ValueResult val _ (TaskRep def _) _, _, _, _) 
					= (Just def, {TaskState | st & value = Just val}, iworld)
		Ok (ValueResult val _ NoRep _, _, _, _)
					= abort "NoRep"
		Ok (DestroyedResult, _, _, _)
					= abort "Destroy"
		Ok (ExceptionResult _ msg, _, _, _)
					= abort msg
		Error msg	= abort msg
		_			= (Nothing, {TaskState | st & value = Nothing}, iworld)	

// Edit
controllerFunc taskId st=:{TaskState | sessionId, instanceNo} eventNo (Just name) (Just jsonval) iworld
	# (mbResult,iworld)	= evalSessionTaskInstance sessionId (EditEvent eventNo taskId name (fromString jsonval)) iworld
	= case mbResult of
		Ok (ValueResult val _ (TaskRep def _) _, _, _, _) 
					= (Just def, {TaskState | st & value = Just val}, iworld)
		Ok (ValueResult val _ NoRep _, _, _, _)
					= abort "NoRep"
		Ok (DestroyedResult, _, _, _)
					= abort "Destroy"
		Ok (ExceptionResult _ msg, _, _, _) 
					= abort msg
		Error msg	= abort msg
		_			= (Nothing, {TaskState | st & value = Nothing}, iworld)	

// Action
controllerFunc taskId st=:{TaskState | sessionId, instanceNo} eventNo (Just name) Nothing iworld
	# (mbResult,iworld)	= evalSessionTaskInstance sessionId (ActionEvent eventNo taskId name) iworld
	= case mbResult of
		Ok (ValueResult val _ (TaskRep def _) _, _, _, _) 
					= (Just def, {TaskState | st & value = Just val}, iworld)
		Ok (ValueResult val _ NoRep _, _, _, _)
					= abort "NoRep"
		Ok (DestroyedResult, _, _, _)
					= abort "Destroy"
		Ok (ExceptionResult _ msg, _, _, _) 
					= abort msg
		Error msg	= abort msg
		_			= (Nothing, {TaskState | st & value = Nothing}, iworld)	

newWorld :: *World
newWorld = undef

createClientIWorld :: !InstanceNo -> *IWorld
createClientIWorld currentInstance
		= {IWorld
		  |application			= "application"
		  ,build				= "build"
		  ,config				= {sessionTime = 3600, smtpServer = locundef "smtpServer"}
		  ,systemDirectories	= {appDirectory  = locundef "appDirectory"
		  						  ,dataDirectory = locundef "dataDirectory"
		  						  ,sdkDirectory  = locundef "sdkDirectory"
    							  ,publicWebDirectories = locundef "publicWebDirectories"}
		  ,taskTime				= 0
		  ,timestamp			= Timestamp 1
		  ,currentDateTime		= DateTime {Date|day = 1, mon = 1, year = 1977} {Time|hour = 0, min = 0, sec = 0}
		  ,currentUser			= SystemUser
		  ,currentInstance		= currentInstance
		  ,currentSession		= Just currentInstance
		  ,currentAttachment	= []	  
		  ,nextTaskNo			= 6666
		  ,localShares			= newMap
		  ,localLists			= newMap
		  ,localTasks			= newMap
		  ,eventRoute			= newMap
		  ,readShares			= []
		  ,editletDiffs			= newMap
		  ,sessions				= newMap
		  ,jsCompilerState		= locundef "jsCompilerState"
		  ,workQueue			= []
		  ,uiMessages			= newMap	  
		  ,shutdown				= False
		  ,world				= newWorld
		  ,resources			= Nothing
		  ,onClient				= True
		  }
where
	locundef var = abort ("IWorld structure is not avalaible at client side. Reference: "+++var)
