implementation module Engine

import StdMisc, StdArray, StdList, StdChar, StdFile, StdBool

from StdFunc import o

from StdLibMisc import qualified ::Date{..}, ::Time{..}
from Directory import qualified pd_StringToPath, createDirectory, getFileInfo, ::DirError(..), ::FileInfo(..), ::PI_FileInfo(..), instance == DirError

import	Store, UserDB, ProcessDB, SessionDB
import	Text, Util, HtmlUtil

import	CommandLine
import	TuningCombinators

import	HTTP, HttpUtil
from	HttpServer import :: HTTPServerControl(..), :: HTTPServerOption(..)

import	Setup

import ApplicationService, SessionService, WorkflowService, TaskService, UserService, DocumentService

import Config, TSt

from UserAdmin	import manageUsers
from Messages	import manageMessages
from Lists		import manageLists
from Groups		import manageGroups

PATH_SEP :== "\\"

// The iTasks engine consist of a set of HTTP request handlers
engine :: !(Maybe Config) [Workflow] -> [(!String -> Bool, HTTPRequest *World -> (!HTTPResponse, !HTTPServerControl, !*World))] 
engine mbConfig userWorkflows	
	= case mbConfig of
		Just config
			= handlers config
		Nothing
			= [(\_ -> True, setupHandler handlers)]
where
	handlers config
		# flows = adminWorkflows ++ (if config.generalWorkflows generalWorkflows [] ) ++ userWorkflows
		= [
		  // Handler to stop the server nicely
		   ((==) "/stop", handleStopRequest)
		  // Webservices
		  ,(startsWith config.serverPath, serviceDispatch config flows)
		  ,(\_ -> True, handleStaticResourceRequest config)
		  ]

	adminWorkflows		= [restrictedWorkflow "Admin/Users" "Manage system users" ["admin"] manageUsers]
	generalWorkflows	= [workflow "Messages" "Send and receive messages" manageMessages
						  ,workflow "Lists" "Create and manage various lists" manageLists
						  ,workflow "Groups" "Manage user groups" manageGroups
						  ]
	
	serviceDispatch config flows req world
		# tst				= initTSt req config flows world
		# reqpath			= (http_urldecode req.req_path)
		# reqpath			= reqpath % (size config.serverPath, size reqpath)
		# (response,tst) = case (split "/" reqpath) of
			[""]								= (redirectResponse (req.req_path +++ "/html"), tst)
			["","html"]							= (overviewResponse, tst)
			["",format:path]
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

workflow :: !String !String !(Task a) -> Workflow | iTask a
workflow path description task =
	{ Workflow
	| path	= path
	, roles	= []
	, thread = createThread (task <<@ Title name)
	, description = description
	}
where
	name = last (split "/" path)
	
restrictedWorkflow :: !String !String ![Role] !(Task a) -> Workflow | iTask a
restrictedWorkflow path description roles task =
	{ Workflow
	| path	= path
	, roles	= roles
	, thread = createThread (task <<@ Title name)
	, description = description
	}
where
	name = last (split "/" path)

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
	= (notFoundResponse req,HTTPServerContinue,world)
where
	//Translate a URL path to a filesystem path
	filePath path = ((replaceSubString "/" PATH_SEP) o (replaceSubString ".." "")) path

handleStopRequest :: HTTPRequest *World -> (!HTTPResponse,!HTTPServerControl,!*World)
handleStopRequest req world = ({http_emptyResponse & rsp_data = "Server stopped..."},HTTPServerStop, world)

initTSt :: !HTTPRequest !Config ![Workflow] !*World -> *TSt
initTSt request config flows world
	# (appName,world) 			= determineAppName world
	# (pathstr,world)			= determineAppPath world
	# ((ok, path),world)		= 'Directory'.pd_StringToPath (pathstr) world
	| not ok					= abort "Cannot find the executable."
	# ((err,info),world)		= 'Directory'.getFileInfo path world
	| err <> 'Directory'.NoDirError			= abort "Cannot get executable info."
	# (date,time)				= info.'Directory'.pi_fileInfo.'Directory'.lastModified
	# datestr					= (toString date.'StdLibMisc'.Date.'StdLibMisc'.year)+++"."+++
								   (padZero date.'StdLibMisc'.Date.'StdLibMisc'.month)+++"."+++
								   (padZero date.'StdLibMisc'.Date.'StdLibMisc'.day)+++"-"+++
								   (padZero time.'StdLibMisc'.Time.'StdLibMisc'.hours)+++"."+++
								   (padZero time.'StdLibMisc'.Time.'StdLibMisc'.minutes)+++"."+++
								   (padZero time.'StdLibMisc'.Time.'StdLibMisc'.seconds
								  )
	# ((ok,datapath),world)		= 'Directory'.pd_StringToPath appName world
	# (err,world)				= 'Directory'.createDirectory datapath world
	| err <> 'Directory'.NoDirError && err <> 'Directory'.AlreadyExists
		= abort "Cannot create data directory"
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