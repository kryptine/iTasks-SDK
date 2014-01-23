implementation module iTasks.Framework.Client.RunOnClient

import StdMisc
import iTasks
import iTasks.Framework.TaskStore
import iTasks.Framework.TaskEval
import iTasks.API.Core.Client.Tasklet
import iTasks.Framework.UIDiff

from Data.Map import qualified newMap, toList, get
from Data.List import find

import System.Time, Math.Random
import Text.JSON
import StdDebug

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
roc_generator task (TaskId instanceNo _) _ iworld=:{current={sessionInstance=Just currentInstance}}
    # currentSession = "SESSIONID-" +++ toString currentInstance
	# gui = TaskletTUI {TaskletTUI|instanceNo = instanceNo, controllerFunc = controllerFunc}
	# state = 	{ TaskState
				| instanceNo = instanceNo
				, sessionId  = currentSession
				, taskId 	 = Nothing
				, task		 = task
				, value 	 = Nothing}
	= (gui, state, iworld)

// Init
controllerFunc _ st=:{TaskState | sessionId, instanceNo, task, taskId = Nothing} Nothing Nothing Nothing iworld
	# (taskId, iworld)  = createClientTaskInstance task sessionId instanceNo iworld
	# (mbResult,iworld)	= evalSessionTaskInstance instanceNo (RefreshEvent Nothing) iworld
	= case mbResult of
		Ok (ValueResult _ _ (TaskRep def _) _, _, _, update)
					= (Just update, {TaskState | st & taskId = Just taskId}, iworld)
		_			= (Nothing, {TaskState | st & taskId = Just taskId}, iworld)

// Refresh
controllerFunc _ st=:{TaskState | sessionId, instanceNo, task, taskId = Just t} Nothing Nothing Nothing iworld
	# (mbResult,iworld)	= evalSessionTaskInstance instanceNo (RefreshEvent Nothing) iworld
	= case mbResult of
		Ok (ValueResult val _ (TaskRep def _) _, _, _, update)
					= (Just update, {TaskState | st & value = Just val}, iworld)
		Ok (ValueResult val _ NoRep _, _, _, _)
					= abort "NoRep"
		Ok (DestroyedResult, _, _, _)
					= abort "Destroy"
		Ok (ExceptionResult _ msg, _, _, _)
					= abort msg
		Error msg	= abort msg
		_			= (Nothing, {TaskState | st & value = Nothing}, iworld)	

// Focus
controllerFunc _ st=:{TaskState | sessionId, instanceNo, task, taskId = Just t} (Just eventNo) Nothing Nothing iworld
	# iworld = trace_n "c_focus" iworld
	# (mbResult,iworld)	= evalSessionTaskInstance instanceNo (FocusEvent eventNo t) iworld
	= case mbResult of
		Ok (ValueResult val _ (TaskRep def _) _, _, _, update)
					= (Just update, {TaskState | st & value = Just val}, iworld)
		Ok (ValueResult val _ NoRep _, _, _, _)
					= abort "NoRep"
		Ok (DestroyedResult, _, _, _)
					= abort "Destroy"
		Ok (ExceptionResult _ msg, _, _, _)
					= abort msg
		Error msg	= abort msg
		_			= (Nothing, {TaskState | st & value = Nothing}, iworld)	

// Edit
controllerFunc taskId st=:{TaskState | sessionId, instanceNo} (Just eventNo) (Just name) (Just jsonval) iworld
	# (mbResult,iworld)	= evalSessionTaskInstance instanceNo (EditEvent eventNo taskId name (fromString jsonval)) iworld
	= case mbResult of
		Ok (ValueResult val _ (TaskRep def _) _, _, _, update)
					= (Just update, {TaskState | st & value = Just val}, iworld)
		Ok (ValueResult val _ NoRep _, _, _, _)
					= abort "NoRep"
		Ok (DestroyedResult, _, _, _)
					= abort "Destroy"
		Ok (ExceptionResult _ msg, _, _, _)
					= abort msg
		Error msg	= abort msg
		_			= (Nothing, {TaskState | st & value = Nothing}, iworld)	

// Action
controllerFunc taskId st=:{TaskState | sessionId, instanceNo} (Just eventNo) (Just name) Nothing iworld
	# (mbResult,iworld)	= evalSessionTaskInstance instanceNo (ActionEvent eventNo taskId name) iworld
	= case mbResult of
		Ok (ValueResult val _ (TaskRep def _) _, _, _, update)
					= (Just update, {TaskState | st & value = Just val}, iworld)
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

getUIUpdates :: !*IWorld -> (!Maybe [(InstanceNo, [String])], *IWorld)
getUIUpdates iworld=:{uiMessages}
		= case 'Data.Map'.toList uiMessages of
			[]   = (Nothing, iworld)		
			msgs = (Just (map getUpdates msgs), {iworld & uiMessages = 'Data.Map'.newMap}) 
where
	isUIUpdates (UIUpdates _) = True
	isUIUpdates _ = False

	getUpdates (instanceNo,msgs)
		= (instanceNo, map (\(UIUpdates upds) -> toString (encodeUIUpdates upds)) (filter isUIUpdates msgs))

createClientIWorld :: !String !InstanceNo -> *IWorld
createClientIWorld serverURL currentInstance
        # world = newWorld
        # (Timestamp seed,world) = time world
		= {IWorld
		  |server =
            {serverName = "application"
		    ,serverURL	= serverURL
		    ,buildID    = "build"
		    ,paths      = {appDirectory  = locundef "appDirectory"
                          ,dataDirectory = locundef "dataDirectory"
                          ,sdkDirectory  = locundef "sdkDirectory"
                          ,publicWebDirectories = locundef "publicWebDirectories" }
            ,customCSS  = False }
		  ,config				= {sessionTime = 3600, smtpServer = locundef "smtpServer"}
          ,current =
		    {timestamp			= Timestamp 1
		    ,utcDateTime	    = DateTime {Date|day = 1, mon = 1, year = 1977} {Time|hour = 0, min = 0, sec = 0}
		    ,localDateTime	    = DateTime {Date|day = 1, mon = 1, year = 1977} {Time|hour = 0, min = 0, sec = 0}
            ,taskTime			= 0
		    ,taskInstance	    = currentInstance
		    ,sessionInstance	= Just currentInstance
		    ,attachmentChain    = []
		    ,nextTaskNo			= 6666
		    ,user               = SystemUser
		    ,localShares		= 'Data.Map'.newMap
		    ,localLists			= 'Data.Map'.newMap
		    ,localTasks			= 'Data.Map'.newMap
		    ,eventRoute			= 'Data.Map'.newMap
		    ,readShares			= []
		    ,editletDiffs		= 'Data.Map'.newMap
          }
		  ,exposedShares		= 'Data.Map'.newMap
		  ,jsCompilerState		= locundef "jsCompilerState"
		  ,workQueue			= []
		  ,uiMessages			= 'Data.Map'.newMap
		  ,shutdown				= False
          ,random               = genRandInt seed
          ,io                   = {done=[],todo=[]}
		  ,ioValues             = 'Data.Map'.newMap
		  ,world				= world
		  ,resources			= Nothing
		  ,onClient				= True
		  }
where
	locundef var = abort ("IWorld structure is not avalaible at client side. Reference: "+++var)
