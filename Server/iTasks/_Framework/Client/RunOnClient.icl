implementation module iTasks._Framework.Client.RunOnClient

import StdMisc
import iTasks
import iTasks._Framework.TaskStore
import iTasks._Framework.TaskEval
import iTasks._Framework.IWorld
import iTasks.UI.Definition
import qualified iTasks._Framework.SDS as SDS

from Data.Map import qualified newMap, toList, fromList, get
from Data.List import find
from Data.Queue as DQ import qualified newQueue, dequeue

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
runOnClient task = task
/*
	# roc_tasklet =
		{ Tasklet 
		| genUI				= roc_generator task
		, resultFunc		= gen_res
		, tweakUI			= id
		}
 
	= mkTask roc_tasklet
*/
gen_res {TaskState|value=Nothing} = NoValue
gen_res {TaskState|value=Just NoValue} = NoValue
gen_res {TaskState|value=Just (Value json stability)} = Value (fromJust (fromJSON json)) stability

/*
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
*/
// Init
controllerFunc _ st=:{TaskState | sessionId, instanceNo, task, taskId = Nothing} Nothing Nothing Nothing iworld
	# (mbTaskId, iworld) = createClientTaskInstance task sessionId instanceNo iworld
    = case mbTaskId of
        Ok taskId
	      # (mbResult,iworld)  = evalTaskInstance instanceNo (RefreshEvent "Client init") iworld
	      = case mbResult of
	      	Ok _ 
	      				= (Nothing, {TaskState | st & taskId = Just taskId}, iworld)
	      	_			= (Nothing, {TaskState | st & taskId = Just taskId}, iworld)
        _ = (Nothing, st, iworld)
/* FIXME
// Refresh
controllerFunc _ st=:{TaskState | sessionId, instanceNo, task, taskId = Just t} Nothing Nothing Nothing iworld
	# (mbResult,iworld)	= evalTaskInstance instanceNo (RefreshEvent "Client refresh") iworld
	= case mbResult of
		Ok (_,value)
					= (Nothing, {TaskState | st & value = Just value}, iworld)
		Error msg	= abort msg
// Focus
controllerFunc _ st=:{TaskState | sessionId, instanceNo, task, taskId = Just t} Nothing Nothing Nothing iworld
	# iworld = trace_n "c_focus" iworld
	# (mbResult,iworld)	= evalTaskInstance instanceNo (FocusEvent t) iworld
	= case mbResult of
		Ok (_,value)
					= (Nothing, {TaskState | st & value = Just value}, iworld)
		Error msg	= abort msg
*/
// Edit
controllerFunc taskId st=:{TaskState | sessionId, instanceNo} Nothing (Just name) (Just jsonval) iworld
	# (mbResult,iworld)	= evalTaskInstance instanceNo (EditEvent taskId name (fromString jsonval)) iworld
	= case mbResult of
		Ok value
					= (Nothing, {TaskState | st & value = Just value}, iworld)
		Error msg	= abort msg
// Action
controllerFunc taskId st=:{TaskState | sessionId, instanceNo} Nothing (Just name) Nothing iworld
	# (mbResult,iworld)	= evalTaskInstance instanceNo (ActionEvent taskId name) iworld
	= case mbResult of
		Ok value
					= (Nothing, {TaskState | st & value = Just value}, iworld)
		Error msg	= abort msg

newWorld :: *World
newWorld = undef

getUIUpdates :: !*IWorld -> (!Maybe [(InstanceNo, [String])], *IWorld)
getUIUpdates iworld
	= case 'SDS'.read allUIChanges iworld of
		(Ok uiChanges,iworld)
			= case 'Data.Map'.toList uiChanges of
				[] = (Nothing,iworld)
				changes
					# (_,iworld) = 'SDS'.write 'Data.Map'.newMap allUIChanges iworld
					= (Just (map getUpdates changes), iworld)
		(_,iworld)
			= (Nothing, iworld)
where
	getUpdates (instanceNo,upds) = (instanceNo, [toString (encodeUIChanges (toList upds))])
	toList q = case 'DQ'.dequeue q of //TODO SHOULD BE IN Data.Queue
		(Nothing,q) 	= []
		(Just x,q) 		= [x:toList q]

createClientIWorld :: !String !InstanceNo -> *IWorld
createClientIWorld serverURL currentInstance
        # world = newWorld
        # (timestamp=:(Timestamp seed),world) = time world
		= {IWorld
		  |server =
            {serverName = "application"
		    ,serverURL	= serverURL
		    ,buildID    = "build"
		    ,paths      = {appDirectory  = locundef "appDirectory"
                          ,dataDirectory = locundef "dataDirectory"
                          ,publicWebDirectories = locundef "publicWebDirectories"
						  ,saplDirectory = locundef "saplDirectory"
						  ,saplFlavourFile = locundef "saplFlavourFile"}
            ,customCSS  = False }
		  ,config				= {sessionTime = 3600, smtpServer = locundef "smtpServer"}
          ,clocks =
            { timestamp =   timestamp
			, localDate =   {Date|day = 1, mon = 1, year = 1977}
            , localTime =   {Time|hour = 0, min = 0, sec = 0}
            , utcDate =     {Date|day = 1, mon = 1, year = 1977}
            , utcTime =     {Time|hour = 0, min = 0, sec = 0}
            }
          ,current =
            {taskTime			= 0
		    ,taskInstance	    = currentInstance
		    ,sessionInstance	= Just currentInstance
		    ,attachmentChain    = []
		    ,nextTaskNo			= 6666
          }
          ,sdsNotifyRequests    = []
          ,memoryShares         = 'Data.Map'.newMap
          ,cachedShares         = 'Data.Map'.newMap
		  ,exposedShares		= 'Data.Map'.newMap
		  ,jsCompilerState		= locundef "jsCompilerState"
		  ,shutdown				= False
          ,random               = genRandInt seed
          ,ioTasks              = {done=[],todo=[]}
		  ,ioStates             = 'Data.Map'.newMap
		  ,world				= world
		  ,resources			= Nothing
		  ,onClient				= True
		  }
where
	locundef var = abort ("IWorld structure is not avalaible at client side. Reference: "+++var)
