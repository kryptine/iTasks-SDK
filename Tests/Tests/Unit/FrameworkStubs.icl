implementation module Tests.Unit.FrameworkStubs
import iTasks._Framework.IWorld
import iTasks._Framework.Generic
import iTasks.API.Core.Types
import iTasks.UI.Editor
import System.Time
import Data.Maybe
import StdMisc
import qualified Data.Map as DM

//TEST STUBS
toStubIWorld :: *World -> *IWorld
toStubIWorld world
  = {IWorld
  |server = {serverName = "STUB",serverURL = "//127.0.0.1:80",buildID = "STUB"
        	,paths = {appDirectory = "./STUB/",dataDirectory = "./STUB/",publicWebDirectories = []
		     	,saplDirectory = "./STUB/", saplFlavourFile = "./STUB/flavour.f"},customCSS  = False}
  ,config = {sessionTime = 3600, smtpServer = "localhost"}
  ,clocks = {SystemClocks |timestamp = Timestamp 0,localDate=defaultValue,localTime=defaultValue,utcDate=defaultValue,utcTime=defaultValue}
  ,current ={TaskEvalState|taskTime= 0,taskInstance= 0,sessionInstance = Nothing,attachmentChain = [] ,nextTaskNo = 0}
  ,sdsNotifyRequests = [], memoryShares = 'DM'.newMap, cachedShares = 'DM'.newMap, exposedShares = 'DM'.newMap
  ,jsCompilerState = Nothing ,shutdown = False,ioTasks = {done = [], todo = []},ioStates = 'DM'.newMap
  ,world = world
  ,resources = Nothing,random = [],onClient = False }
	
fromStubIWorld :: *IWorld -> *World
fromStubIWorld iworld=:{IWorld|world} = world

toStubVSt :: *IWorld -> *VSt
toStubVSt iworld = {VSt| selectedConsIndex = -1, optional = False, disabled = False, taskId = "STUB", iworld = iworld}

fromStubVSt :: *VSt -> *IWorld
fromStubVSt vst=:{VSt|iworld} = iworld
