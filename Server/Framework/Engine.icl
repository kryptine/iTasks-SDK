implementation module Engine

import StdMisc, StdArray, StdList, StdTuple, StdChar, StdFile, StdBool

from StdFunc import o

from StdLibMisc import qualified ::Date{..}, ::Time{..}

import	Store, UserDB, ProcessDB, SessionDB
import	Util, HtmlUtil
import	CommandLine, File, FilePath, Directory, HTTP, Text, MIME, UrlEncoding

import	TuningCombinators
import	Setup

import ApplicationService, SessionService, WorkflowService, TaskService, UserService, DocumentService, StencilService

import Config, TSt

from UserAdmin	import manageUsers
from Messages	import manageMessages
from Lists		import manageLists
from Groups		import manageGroups

// The iTasks engine consist of a set of HTTP request handlers
engine :: !(Maybe Config) [Workflow] -> [(!String -> Bool, HTTPRequest *World -> (!HTTPResponse, !*World))] 
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
		# reqpath			= (urlDecode req.req_path)
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
						["stencils":path]		= stencilService req.req_path html path req tst
						_						= (notFoundResponse req, tst)
				| otherwise
					= (notFoundResponse req, tst)
			_
				= (notFoundResponse req, tst)
		# tst		= flushStore tst
		= (response, finalizeTSt tst)

workflow :: !String !String !(Task a) -> Workflow | iTask a
workflow path description task =
	{ Workflow
	| path	= path
	, roles	= []
	, thread = createThread (task <<@ Title (path2name path))
	, description = description
	}
	
workflowParam :: !String !String !(a -> Task b)	-> Workflow | iTask a & iTask b
workflowParam path description task =
	{ Workflow
	| path	= path
	, roles	= []
	, thread = createThreadParam ((@>>) (Title (path2name path)) o task)
	, description = description
	}

restrictedWorkflow :: !String !String ![Role] !(Task a) -> Workflow | iTask a
restrictedWorkflow path description roles task =
	{ Workflow
	| path	= path
	, roles	= roles
	, thread = createThread (task <<@ Title (path2name path))
	, description = description
	}
	
restrictedWorkflowParam :: !String !String ![Role] !(a -> Task b) -> Workflow | iTask a & iTask b
restrictedWorkflowParam path description roles task =
	{ Workflow
	| path	= path
	, roles	= roles
	, thread = createThreadParam ((@>>) (Title (path2name path)) o task)
	, description = description
	}

path2name path = last (split "/" path)

config :: !*World -> (!Maybe Config,!*World)
config world
	# (appName,world) = determineAppName world
	= loadConfig appName world

// Request handler which serves static resources from the application directory,
// or a system wide default directory if it is not found locally.
// This request handler is used for serving system wide javascript, css, images, etc...
handleStaticResourceRequest :: !Config !HTTPRequest *World -> (!HTTPResponse,!*World)
handleStaticResourceRequest config req world
	# path					= if (req.req_path == "/") "/index.html" req.req_path
	# filename				= config.clientPath +++ filePath path
	# type					= mimeType filename
	# (mbContent, world)	= readFile filename world
	| isOk mbContent		= ({rsp_headers = fromList [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size (fromOk mbContent)))]
							   	,rsp_data = fromOk mbContent}, world)
	# filename				= config.staticPath +++ filePath path
	# type					= mimeType filename
	# (mbContent, world)	= readFile filename world
	| isOk mbContent 		= ({rsp_headers = fromList [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size (fromOk mbContent)))											   
											   ]
							   	,rsp_data = fromOk mbContent}, world)						   								 	 							   
	= (notFoundResponse req,world)
where
	//Translate a URL path to a filesystem path
	filePath path	= ((replaceSubString "/" {pathSeparator}) o (replaceSubString ".." "")) path
	mimeType path	= extensionToMimeType (takeExtension path)

handleStopRequest :: HTTPRequest *World -> (!HTTPResponse,!*World)
handleStopRequest req world = ({newHTTPResponse & rsp_headers = fromList [("X-Server-Control","stop")], rsp_data = "Server stopped..."}, world) //Stop

initTSt :: !HTTPRequest !Config ![Workflow] !*World -> *TSt
initTSt request config flows world
	# (appName,world) 			= determineAppName world
	# (appPath,world)			= determineAppPath world
	# appPath					= takeDirectory appPath
	# (res,world)				= getFileInfo appPath world
	| isError res				= abort "Cannot get executable info."
	# tm						= (fromOk res).lastModifiedTime
	# datestr					= (toString tm.Tm.year)+++"."+++
								   (padZero tm.Tm.mon)+++"."+++
								   (padZero tm.Tm.mday)+++"-"+++
								   (padZero tm.Tm.hour)+++"."+++
								   (padZero tm.Tm.min)+++"."+++
								   (padZero tm.Tm.sec
								  )
	# world						= ensureDir "data" (appPath </> appName) world
	# tmpPath					= appName </> "tmp-" +++ datestr
	# world						= ensureDir "tmp" tmpPath world
	# storePath					= appName </> datestr
	= mkTSt appName config request flows (createStore storePath) tmpPath world
where 
	padZero :: !Int -> String
	padZero number = (if (number < 10) "0" "") +++ toString number

	ensureDir :: !String !FilePath *World -> *World
	ensureDir name path world
	# (exists, world) = fileExists path world
	| exists = world
	# (res, world) = createDirectory path world
	| isError res = abort ("Cannot create " +++ name +++ " directory" +++ path +++ " : "  +++ snd (fromError res))
	= world

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
	= ((dropExtension o dropDirectory o hd) args,world)
