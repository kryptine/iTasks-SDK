implementation module Engine

import StdMisc, StdArray, StdList, StdChar, GenBimap
import Store, UserDB, ProcessDB, SessionDB
import Text, Util
import CoreCombinators

import Http, HttpUtil

import AuthenticationHandler, DeauthenticationHandler
import NewListHandler, NewStartHandler, WorkListHandler, WorkTabHandler, PropertyHandler, UserListHandler
import TaskTreeForestHandler, ProcessTableHandler

import TSt

RESOURCE_DIR :== "Resources/" //TODO: Use config file

// The iTasks engine consist of a set of HTTP request handlers
engine :: [Workflow] -> [(!String -> Bool, HTTPRequest *World -> (!HTTPResponse, !*World))] 
engine flows = [((==) "/handlers/authenticate", handleAnonRequest flows handleAuthenticationRequest)
			   ,((==) "/handlers/deauthenticate", handleSessionRequest flows handleDeauthenticationRequest)							
			   ,((==) "/handlers/new/list", handleSessionRequest flows handleNewListRequest)
			   ,((==) "/handlers/new/start", handleSessionRequest flows handleNewStartRequest)
			   ,((==) "/handlers/work/list", handleSessionRequest flows handleWorkListRequest)
			   ,((==) "/handlers/work/tab", handleSessionRequest flows handleWorkTabRequest)
			   ,((==) "/handlers/work/property", handleSessionRequest flows handlePropertyRequest)
			   ,((==) "/handlers/data/users", handleSessionRequest flows handleUserListRequest)
			   ,((==) "/handlers/debug/tasktreeforest", handleSessionRequest flows handleTaskTreeForestRequest)
			   ,((==) "/handlers/debug/processtable", handleSessionRequest flows handleProcessTableRequest)
			   ,(\_ -> True, handleStaticResourceRequest)
				 ]
workflow :: String (Task a) -> Workflow | iTask a
workflow path task =
	{ Workflow
	| name	= path
	, label = last (split "/" path)
	, roles	= []
	, mainTask = task >>| return Void
	}


// Request handler which serves static resources from the application directory,
// or a system wide default directory if it is not found locally.
// This request handler is used for serving system wide javascript, css, images, etc...
handleStaticResourceRequest :: !HTTPRequest *World -> (!HTTPResponse, !*World)
handleStaticResourceRequest req world
	# path					= if (req.req_path == "/") "/index.html" req.req_path
	# (cwd,world)			= ("./",world) //TODO
	# filename				= cwd +++ path	
	# (type, world)			= http_staticFileMimeType filename world
	# (ok, content, world)	= http_staticFileContent filename world
	| ok					= ({rsp_headers = [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size content))]
							   ,rsp_data = content}, world)
	# filename				= RESOURCE_DIR +++ path
	# (type, world)			= http_staticFileMimeType filename world
	# (ok, content, world)	= http_staticFileContent filename world
	|  ok 					= ({rsp_headers = [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size content))]
							   	,rsp_data = content}, world)		 							   
	= http_notfoundResponse req world

handleAnonRequest :: [Workflow] (HTTPRequest *TSt -> (!HTTPResponse, !*TSt)) !HTTPRequest *World -> (!HTTPResponse, !*World)
handleAnonRequest flows handler request world
	# tst						= initTSt request flows world
	# (response, tst)			= handler request tst
	# world						= finalizeTSt tst
	= (response, world)

handleSessionRequest :: [Workflow] (HTTPRequest *TSt -> (!HTTPResponse, !*TSt)) !HTTPRequest *World -> (!HTTPResponse, !*World)
handleSessionRequest flows handler request world
	# tst						= mkTSt request (abort "session not active yet") flows (createStore "iTask-data") world
	# sessionId					= http_getValue "_session" (request.arg_get ++ request.arg_post) ""
	# (mbSession,timeout,tst=:{staticInfo})	= restoreSession sessionId tst
	= case mbSession of
		Nothing
			# world				= finalizeTSt tst
			= ({http_emptyResponse & rsp_data = mkSessionFailureResponse timeout}, world)
		(Just session)
			# tst					= {tst & staticInfo = {staticInfo & currentSession = session}}
			# (response,tst)		= handler request tst
			# tst					= flushStore tst
			# world					= finalizeTSt tst
			= (response, world)		
where
	mkSessionFailureResponse to = "{\"success\" : false, \"error\" : \"" +++ (if to "Your session timed out" "Failed to load session") +++ "\"}"
 
initTSt :: !HTTPRequest ![Workflow] !*World -> *TSt
initTSt request flows world = mkTSt request (abort "session not active yet") flows (createStore "iTask-data") world

finalizeTSt :: !*TSt -> *World
finalizeTSt tst=:{TSt|world} = world
