implementation module Engine

import StdMisc, StdArray, StdList, StdTuple, StdChar, StdFile, StdBool
from StdFunc import o
import	Map, Time, CommandLine, Error, File, FilePath, Directory, HTTP, OSError, Text, MIME, UrlEncoding
import	Util, HtmlUtil
import	TuningCombinators
import	Setup
import	Config

from WorkflowDB	import qualified class WorkflowDB(..), instance WorkflowDB IWorld
from UserAdmin	import manageUsers

from TaskInstance import createThread, createThreadParam

// The iTasks engine consist of a set of HTTP request handlers
engine :: !(Maybe Config) [Workflow] ![Handler] -> [(!String -> Bool,!HTTPRequest *World -> (!HTTPResponse, !*World))] 
engine mbConfig userWorkflows handlers
	= case mbConfig of
		Just config
			= handlers` config
		Nothing
			= [(\_ -> True, setupHandler handlers`)]
where
	handlers` config
		# flows = adminWorkflows  ++ userWorkflows
		= [
		  // Handler to stop the server nicely
		   ((==) "/stop", handleStopRequest)
		  // Webservices
		  ,(startsWith config.serverPath, serviceDispatch config flows)
		  ,(\_ -> True, handleStaticResourceRequest config)
		  ]

	adminWorkflows		= [restrictedWorkflow "Admin/Users" "Manage system users" ["admin"] manageUsers]
	
	serviceDispatch config flows req world
		# iworld			= initIWorld config flows world
		# reqpath			= (urlDecode req.req_path)
		# reqpath			= reqpath % (size config.serverPath, size reqpath)
		# (response,iworld)	= case (split "/" reqpath) of
			[""]								= (redirectResponse (req.req_path +++ "/html"), iworld)
			["","html"]							= (overviewResponse, iworld)
			["",format,name:path] = case filter (\(name`,formats,_) -> name` == name && isMember format formats) handlers of
				[(_,_,handler):_]	= handler req.req_path format path req iworld
				[]					= (notFoundResponse req, iworld)
			_
				= (notFoundResponse req, iworld)
		= (response, finalizeIWorld iworld)

initIWorld :: !Config ![Workflow] !*World -> *IWorld
initIWorld config flows world
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
	# (timestamp,world)			= time world
	# (localDateTime,world)		= currentDateTimeWorld world
	# (_,world)					= ensureDir "data" (appDir </> appName) world
	# tmpPath					= appDir </> appName </> "tmp-" +++ datestr
	# (_,world)					= ensureDir "tmp" tmpPath world
	# storePath					= appDir </> appName </> "store-"+++ datestr
	# (exists,world)			= ensureDir "store" storePath world
	# iworld					= {IWorld
								  |application		= appName
								  ,storeDirectory	= storePath
								  ,tmpDirectory		= tmpPath
								  ,config			= config
								  ,timestamp		= timestamp
								  ,localDateTime	= localDateTime
								  ,currentUser		= AnyUser
								  ,parallelVars		= newMap
								  ,world			= world
								  }
	= if exists iworld (snd (mapSt ('WorkflowDB'.addWorkflow) flows iworld)) 
where 
	padZero :: !Int -> String
	padZero number = (if (number < 10) "0" "") +++ toString number

	ensureDir :: !String !FilePath *World -> (!Bool,!*World)
	ensureDir name path world
		# (exists, world) = fileExists path world
		| exists = (True,world)
		# (res, world) = createDirectory path world
		| isError res = abort ("Cannot create " +++ name +++ " directory" +++ path +++ " : "  +++ snd (fromError res))
		= (False,world)

finalizeIWorld :: !*IWorld -> *World
finalizeIWorld iworld=:{IWorld|world} = world

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

workflow :: String String w -> Workflow | workflowTask w
workflow path description task = workflowTask path description [] task

restrictedWorkflow :: String String [Role] w -> Workflow | workflowTask w
restrictedWorkflow path description roles task = workflowTask path description roles task
	
instance workflowTask (Task a) | iTask a
where
	workflowTask path description roles task = workflowTask path description roles (Workflow initManagerProperties task)
	
instance workflowTask (WorkflowContainer a) | iTask a
where
	workflowTask path description roles (Workflow managerP task) = mkWorkflow path description roles (createThread (task <<@ Title (path2name path))) managerP

instance workflowTask (a -> Task b) | iTask a & iTask b
where
	workflowTask path description roles paramTask = workflowTask path description roles (ParamWorkflow initManagerProperties paramTask)
	
instance workflowTask (ParamWorkflowContainer a b) | iTask a & iTask b
where
	workflowTask path description roles (ParamWorkflow managerP paramTask) = mkWorkflow path description roles (createThreadParam (path2name path) paramTask) managerP
	
mkWorkflow path description roles thread managerProps =
	{ Workflow
	| path	= path
	, roles	= roles
	, thread = thread
	, description = description
	, managerProperties = managerProps
	}

path2name path = last (split "/" path)

config :: !*World -> (!Maybe Config,!*World)
config world
	# (appName,world) = determineAppName world
	= loadConfig appName world
									  
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
