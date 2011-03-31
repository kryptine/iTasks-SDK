implementation module Engine

import StdMisc, StdArray, StdList, StdTuple, StdChar, StdFile, StdBool
from StdFunc import o
import	Store, UserDB, ProcessDB, SessionDB
import	Util, HtmlUtil
import	CommandLine, Error, File, FilePath, Directory, HTTP, OSError, Text, MIME, UrlEncoding
import	TuningCombinators
import	Setup
import Config, TSt

from UserAdmin	import manageUsers
from Messages	import manageMessages
from Lists		import manageLists
from Groups		import manageGroups

// The iTasks engine consist of a set of HTTP request handlers
engine :: !(Maybe Config) ![Workflow] ![Handler] -> [(!String -> Bool,!HTTPRequest *World -> (!HTTPResponse, !*World))] 
engine mbConfig userWorkflows handlers
	= case mbConfig of
		Just config
			= handlers` config
		Nothing
			= [(\_ -> True, setupHandler handlers`)]
where
	handlers` config
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
			["",format,name:path] = case filter (\(name`,formats,_) -> name` == name && isMember format formats) handlers of
				[(_,_,handler):_]	= handler req.req_path format path req tst
				[]					= (notFoundResponse req, tst)
			_
				= (notFoundResponse req, tst)
		# tst		= flushStore tst
		= (response, finalizeTSt tst)

workflow :: !String !String !w -> Workflow | workflowTask w
workflow path description task = workflowTask path description [] task

restrictedWorkflow :: !String !String ![Role] !w -> Workflow | workflowTask w
restrictedWorkflow path description roles task = workflowTask path description roles task
	
instance workflowTask (Task a) | iTask a
where
	workflowTask path description roles task = workflowTask path description roles (Workflow initManagerProperties noMenu task)
	
instance workflowTask (WorkflowContainer a) | iTask a
where
	workflowTask path description roles (Workflow managerP menu task) = mkWorkflow path description roles (createThread (task <<@ Title (path2name path))) managerP menu

instance workflowTask (a -> Task b) | iTask a & iTask b
where
	workflowTask path description roles paramTask = workflowTask path description roles (ParamWorkflow initManagerProperties noMenu paramTask)
	
instance workflowTask (ParamWorkflowContainer a b) | iTask a & iTask b
where
	workflowTask path description roles (ParamWorkflow managerP menu paramTask) = mkWorkflow path description roles (createThreadParam (path2name path) paramTask) managerP menu
	
mkWorkflow path description roles thread managerProps menu =
	{ Workflow
	| path	= path
	, roles	= roles
	, thread = thread
	, description = description
	, managerProperties = managerProps
	, menu = menu
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
	# appDir					= takeDirectory appPath
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
	# world						= ensureDir "data" (appDir </> appName) world
	# tmpPath					= appDir </> appName </> "tmp-" +++ datestr
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
