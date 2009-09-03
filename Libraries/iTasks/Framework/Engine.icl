implementation module Engine

import StdMisc, StdArray, StdList, StdChar, GenBimap
from StdFunc import o
import Store, UserDB, ProcessDB, SessionDB
import Text, Util
import CoreCombinators
import CommandLine

import Http, HttpUtil

import AuthenticationHandler, DeauthenticationHandler
import NewListHandler, NewStartHandler, WorkListHandler, WorkTabHandler, PropertyHandler, UserListHandler
import TaskTreeForestHandler, ProcessTableHandler

import Config, TSt

PATH_SEP :== "\\"

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
	# (appName,world)		= determineAppName world
	# (config,world)		= loadConfig appName world
	# path					= if (req.req_path == "/") "/index.html" req.req_path
	# filename				= config.clientPath +++ filePath path
	# (type, world)			= http_staticFileMimeType filename world
	# (ok, content, world)	= http_staticFileContent filename world
	|  ok 					= ({rsp_headers = [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size content))]
							   	,rsp_data = content}, world)		 							   
	= http_notfoundResponse req world
where
	//Translate a URL path to a filesystem path
	filePath path = ((replaceSubString "/" PATH_SEP) o (replaceSubString ".." "")) path
	
handleAnonRequest :: [Workflow] (HTTPRequest *TSt -> (!HTTPResponse, !*TSt)) !HTTPRequest *World -> (!HTTPResponse, !*World)
handleAnonRequest flows handler request world
	# tst						= initTSt request flows world
	# (response, tst)			= handler request tst
	# world						= finalizeTSt tst
	= (response, world)

handleSessionRequest :: [Workflow] (HTTPRequest *TSt -> (!HTTPResponse, !*TSt)) !HTTPRequest *World -> (!HTTPResponse, !*World)
handleSessionRequest flows handler request world
	# tst						= initTSt request flows world
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
initTSt request flows world
	# (appName,world)			= determineAppName world
	# (config,world)			= loadConfig appName world
	= mkTSt appName config request (abort "session not active yet") flows (createStore (appName +++ "-store")) world

finalizeTSt :: !*TSt -> *World
finalizeTSt tst=:{TSt|world} = world

// Determines the server executables name
determineAppName :: !*World -> (!String,!*World)
determineAppName world 
	# (args,world)	= getCommandLine world
	= (strip (hd args),world)
where
	strip path = let executable = last (split PATH_SEP path) in executable % (0, size executable - 5)



