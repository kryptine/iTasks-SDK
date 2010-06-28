implementation module Engine

import StdMisc, StdArray, StdList, StdChar, StdFile, StdBool

from StdFunc import o
from StdLibMisc import ::Date{..}, ::Time{..}

import Store, UserDB, ProcessDB, SessionDB
import Text, Util
import CoreCombinators, TuningCombinators
import CommandLine
import Directory

import Http, HttpUtil
from HttpServer import :: HTTPServerControl(..), :: HTTPServerOption(..)

import Setup
import AuthenticationHandler, DeauthenticationHandler
import NewListHandler, NewStartHandler, WorkListHandler, WorkTabHandler, PropertyHandler, UserListHandler
import TaskTreeForestHandler, ProcessTableHandler
import RPCHandlers, DocumentHandler

import ApplicationService, SessionService, WorkflowService, TaskService, UserService
import HtmlUtil

import Config, TSt

from UserAdmin import userAdministration

PATH_SEP :== "\\"

// The iTasks engine consist of a set of HTTP request handlers
engine :: !(Maybe Config) [Workflow] -> [(!String -> Bool, HTTPRequest *World -> (!HTTPResponse, !HTTPServerControl, !*World))] 
engine mbConfig userflows	
	= case mbConfig of
		Just config
			= handlers config
		Nothing
			= [(\_ -> True, setupHandler handlers)]
where
	handlers config
		= [ // 'new' services
		   (startsWith "/services", serviceDispatch config flows)
			//'old' handlers
		  ,((==) (config.serverPath +++ "/authenticate"), handleAnonRequest config flows handleAuthenticationRequest)
		  ,((==) (config.serverPath +++ "/deauthenticate"), handleSessionRequest config flows handleDeauthenticationRequest)							
		  ,((==) (config.serverPath +++ "/new/list"), handleSessionRequest config flows handleNewListRequest)
		  ,((==) (config.serverPath +++ "/new/start"), handleSessionRequest config flows handleNewStartRequest)
		  ,((==) (config.serverPath +++ "/work/list"), handleSessionRequest config flows handleWorkListRequest)
		  ,((==) (config.serverPath +++ "/work/tab"), handleSessionRequest config flows handleWorkTabRequest)
		  ,((==) (config.serverPath +++ "/work/property"), handleSessionRequest config flows handlePropertyRequest)
		  ,((==) (config.serverPath +++ "/data/users"), handleSessionRequest config flows handleUserListRequest)
		  ,((==) (config.serverPath +++ "/rpc/request"), handleSessionRequest config flows handleRPCListRequest)
		  ,((==) (config.serverPath +++ "/rpc/response"), handleSessionRequest config flows handleRPCUpdates)
		  ,((==) (config.serverPath +++ "/debug/taskforest"), handleSessionRequest config flows handleTaskForestRequest)
		  ,((==) (config.serverPath +++ "/debug/processtable"), handleSessionRequest config flows handleProcessTableRequest)
		  ,((==) (config.serverPath +++ "/document/download"), handleSessionRequest config flows handleDocumentDownloadRequest)
		  ,((==) (config.serverPath +++ "/document/upload"), handleSessionRequest config flows handleDocumentUploadRequest)
		  ,((==) (config.serverPath +++ "/document/clear"), handleSessionRequest config flows handleDocumentClearRequest)
		  ,((startsWith) (config.serverPath +++ "/document/download/link"), handleSessionRequest config flows handleDocumentDownloadLinkRequest)
		  ,((startsWith) (config.serverPath +++ "/document/preview/link"), handleSessionRequest config flows handleDocumentPreviewLinkRequest)  
		  ,((==) "/stop", handleStopRequest)
		  ,(\_ -> True, handleStaticResourceRequest config)
		  ]	
	//Always add the workflows for administering the itask system
	flows = userflows ++ userAdministration

	serviceDispatch config flows req world
		# tst				= initTSt req config flows world
		# (response,tst) = case (split "/" req.req_path) of
			["","services",format:path]
				# html = format == "html"
				# json = format == "json"
				| html || json
					= case path of
						["application":path]	= applicationService req.req_path html path req tst
						["sessions":path]		= sessionService req.req_path html path req tst
						["workflows":path]		= workflowService req.req_path html path req tst
						["tasks":path]			= taskService req.req_path html path req tst
						["users":path]			= userService req.req_path html path req tst
						_						= (notFoundResponse req, tst)
				| otherwise
					= (notFoundResponse req, tst)
			_
				= (notFoundResponse req, tst)
		# tst		= flushStore tst
		= (response, HTTPServerContinue, finalizeTSt tst)
	
workflow :: !String !(Task a) -> Workflow | iTask a
workflow path task =
	{ Workflow
	| path	= path
	, roles	= []
	, thread = createThread task
	}

restrictedWorkflow :: !String ![Role] !(Task a) -> Workflow | iTask a
restrictedWorkflow path roles task =
	{ Workflow
	| path	= path
	, roles	= roles
	, thread = createThread task
	}

config :: !*World -> (!Maybe Config,!*World)
config world
	# (appName,world) = determineAppName world
	= loadConfig appName world

// Request handler which serves static resources from the application directory,
// or a system wide default directory if it is not found locally.
// This request handler is used for serving system wide javascript, css, images, etc...
handleStaticResourceRequest :: !Config !HTTPRequest *World -> (!HTTPResponse,!HTTPServerControl,!*World)
handleStaticResourceRequest config req world
	# path					= if (req.req_path == "/") "/index.html" req.req_path
	# filename				= config.clientPath +++ filePath path
	# (type, world)			= http_staticFileMimeType filename world
	# (ok, content, world)	= http_staticFileContent filename world
	|  ok 					= ({rsp_headers = [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size content))]
							   	,rsp_data = content}, HTTPServerContinue, world)
	# filename				= config.staticPath +++ filePath path
	# (type, world)			= http_staticFileMimeType filename world
	# (ok, content, world)	= http_staticFileContent filename world
	|  ok 					= ({rsp_headers = [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size content))											   
											   ]
							   	,rsp_data = content}, HTTPServerContinue, world)						   								 	 							   
	= http_notfoundResponse req world
where
	//Translate a URL path to a filesystem path
	filePath path = ((replaceSubString "/" PATH_SEP) o (replaceSubString ".." "")) path

handleStopRequest :: HTTPRequest *World -> (!HTTPResponse,!HTTPServerControl,!*World)
handleStopRequest req world = ({http_emptyResponse & rsp_data = "Server stopped..."},HTTPServerStop, world)
	
handleAnonRequest :: Config [Workflow] (HTTPRequest *TSt -> (!HTTPResponse, !*TSt)) !HTTPRequest *World -> (!HTTPResponse, !HTTPServerControl, !*World)
handleAnonRequest config flows handler request world
	# tst						= initTSt request config flows world
	# (response, tst)			= handler request tst
	# world						= finalizeTSt tst
	= (response, HTTPServerContinue, world)

handleSessionRequest :: Config [Workflow] (HTTPRequest *TSt -> (!HTTPResponse, !*TSt)) !HTTPRequest *World -> (!HTTPResponse, !HTTPServerControl, !*World)
handleSessionRequest config flows handler request world
	# tst						= initTSt request config flows world
	# sessionId					= http_getValue "_session" (request.arg_get ++ request.arg_post) ""
	# (mbSession,timeout,tst=:{staticInfo})	= restoreSession sessionId tst
	= case mbSession of
		Nothing
			# world				= finalizeTSt tst
			= ({http_emptyResponse & rsp_data = mkSessionFailureResponse timeout}, HTTPServerContinue, world)
		(Just session)
			# tst					= {tst & staticInfo = {staticInfo & currentSession = session}}
			# (response,tst)		= handler request tst
			# tst					= flushStore tst
			# world					= finalizeTSt tst
			= (response, HTTPServerContinue, world)		
where
	mkSessionFailureResponse to = "{\"success\" : false, \"session\": false, \"error\" : \"" +++ (if to "Your session timed out" "Failed to load session") +++ "\"}"
 
initTSt :: !HTTPRequest !Config ![Workflow] !*World -> *TSt
initTSt request config flows world
	# (appName,world) 			= determineAppName world
	# (pathstr,world)			= determineAppPath world
	# ((ok, path),world)		= pd_StringToPath (pathstr) world
	| not ok					= abort "Cannot find the executable."
	# ((err,info),world)		= getFileInfo path world
	| err <> NoDirError			= abort "Cannot get executable info."
	# (date,time)				= info.pi_fileInfo.lastModified
	# datestr					= (toString date.Date.year)+++"."+++(addPrefixZero date.Date.month)+++"."+++(addPrefixZero date.Date.day)+++"-"+++(addPrefixZero time.Time.hours)+++"."+++(addPrefixZero time.Time.minutes)+++"."+++(addPrefixZero time.Time.seconds)
	# ((ok,datapath),world)		= pd_StringToPath (appName +++ "-data") world
	# ((ok,docupath),world)		= pd_StringToPath (appName +++ "-document") world
	# (err,world)				= createDirectory datapath world
	| err <> NoDirError 
		&& err <> AlreadyExists	= abort "Cannot create data directory"
	# (err,world)				= createDirectory docupath world
	| err <> NoDirError
		&& err <> AlreadyExists	= abort "Cannot create document directory"
	= mkTSt appName config request flows (createStore (appName +++ "-data\\" +++ datestr)) (createStore (appName +++ "-document\\" +++ datestr)) world
where 
	addPrefixZero number
	| number < 10 = "0"+++toString number
	| otherwise = toString number

finalizeTSt :: !*TSt -> *World
finalizeTSt tst=:{TSt|world} = world

// Determines the server executables path
determineAppPath :: !*World -> (!String, !*World)
determineAppPath world
	# (args,world) = getCommandLine world
	= (hd args,world)

// Determines the server executables name
determineAppName :: !*World -> (!String,!*World)
determineAppName world 
	# (args,world)	= getCommandLine world
	= (strip (hd args),world)
where
	strip path = let executable = last (split PATH_SEP path) in executable % (0, size executable - 5)



