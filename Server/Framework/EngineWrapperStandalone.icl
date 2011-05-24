implementation module EngineWrapperStandalone

import StdFile, StdInt
import Engine
import HTTP, HttpServer
import Config

import ApplicationService, SessionService, WorkflowService, TaskService, UserService, DocumentService, StencilService

startEngine :: [Workflow] !*World -> *World 
startEngine flows world
	# (mbConfig,world)	= config world
	# (app,world)		= determineAppName world
	# world				= instructions app mbConfig world
	# options			= case mbConfig of
							Just config = [HTTPServerOptPort config.serverPort, HTTPServerOptDebug config.debug]
							Nothing		= []
	= http_startServer options (engine mbConfig flows handlers) world
where
	instructions :: !String !(Maybe Config) *World -> *World
	//Normal operation
	instructions app (Just config=:{serverPort,serverPath,staticPath,clientPath}) world
		# (console,world)	= stdio world
		# console			= fwrites ("*** " +++ app +++ " HTTP server started ***\n\n") console
		# console			= fwrites ("Serving client from directory: " +++ clientPath +++ "\n") console
		# console			= fwrites ("Serving static content from directory: " +++ staticPath +++ "\n\n") console
		# console			= fwrites ("You can access the client at: " +++ host +++ "/\n") console
		# console			= fwrites ("You can access the services directly at: " +++ host +++ serverPath +++ "\n") console 
		# (_,world)			= fclose console world
		= world
		where
			host	= if (serverPort == 80) "http://localhost" ("http://localhost:" +++ toString serverPort)
	//Setup mode
	instructions app Nothing world
		# (console,world)	= stdio world
		# console			= fwrites ("*** " +++ app +++ " HTTP server started in setup mode***\n\n") console
		# console			= fwrites ("Please open http://localhost/ and follow instructions\n") console
		# (_,world)			= fclose console world
		= world
		
	handlers :: [Handler]
	handlers =	[ ("application",	["html","json"],	applicationService)
				, ("sessions",		["html","json"],	sessionService)
				, ("workflows",		["html","json"],	workflowService)
				, ("tasks",			["html","json"],	taskService)
				, ("users",			["html","json"],	userService)
				, ("documents",		["html","json"],	documentService)
				, ("stencils",		["html","json"],	stencilService)
				]
