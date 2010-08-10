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
import RPCHandlers

import ApplicationService, SessionService, WorkflowService, TaskService, UserService, DocumentService
import HtmlUtil

import Config, TSt

from UserAdmin import userAdministration

PATH_SEP :== "\\"

// The iTasks engine consist of a set of HTTP request handlers
engine :: !(Maybe Config) [Workflow] -> [(!String -> Bool, HTTPRequest *World -> (!HTTPResponse, !HTTPServerControl, !*World))] 
engine mbConfig userFlows	
	= case mbConfig of
		Just config
			= handlers config
		Nothing
			= [(\_ -> True, setupHandler handlers)]
where
	handlers config
		= [
		  //'old' handlers
		   ((==) (config.serverPath +++ "/rpc/request"), handleSessionRequest config flows handleRPCListRequest)
		  ,((==) (config.serverPath +++ "/rpc/response"), handleSessionRequest config flows handleRPCUpdates)
		  // Webservices
		  ,(startsWith "/services", serviceDispatch config flows)
		  // Handler to stop the server nicely
		  ,((==) "/stop", handleStopRequest)
		  ,(\_ -> True, handleStaticResourceRequest config)
		  ]	
	//Always add the workflows for administering the itask system
	flows = userAdministration ++ userFlows

	serviceDispatch config flows req world
		# tst				= initTSt req config flows world
		# (response,tst) = case (split "/" (http_urldecode req.req_path)) of
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
						["documents":path]		= documentService req.req_path html path req tst
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
	# datestr					= (toString date.Date.year)+++"."+++(padZero date.Date.month)+++"."+++(padZero date.Date.day)+++"-"+++(padZero time.Time.hours)+++"."+++(padZero time.Time.minutes)+++"."+++(padZero time.Time.seconds)
	# ((ok,datapath),world)		= pd_StringToPath appName world
	# (err,world)				= createDirectory datapath world
	| err <> NoDirError 
		&& err <> AlreadyExists	= abort "Cannot create data directory"
	= mkTSt appName config request flows (createStore (appName +++ "\\" +++ datestr)) world
where 
	padZero number = (if (number < 10) "0" "") +++ toString number

finalizeTSt :: !*TSt -> *World
finalizeTSt tst=:{TSt|iworld={IWorld|world}} = world

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