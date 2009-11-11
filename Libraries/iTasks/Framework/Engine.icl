implementation module Engine

import StdMisc, StdArray, StdList, StdChar, StdFile, GenBimap

from StdFunc import o
from StdLibMisc import ::Date{..}, ::Time{..}

import Store, UserDB, ProcessDB, SessionDB
import Text, Util
import CoreCombinators
import CommandLine
import Directory

import Http, HttpUtil

import AuthenticationHandler, DeauthenticationHandler
import NewListHandler, NewStartHandler, WorkListHandler, WorkTabHandler, PropertyHandler, UserListHandler
import TaskTreeForestHandler, ProcessTableHandler
import RPCHandlers

import Config, TSt

PATH_SEP :== "\\"

// The iTasks engine consist of a set of HTTP request handlers
engine :: Config [Workflow] -> [(!String -> Bool, HTTPRequest *World -> (!HTTPResponse, !*World))] 
engine config flows 
	= [((==) "/handlers/authenticate", handleAnonRequest config flows handleAuthenticationRequest)
	  ,((==) "/handlers/deauthenticate", handleSessionRequest config flows handleDeauthenticationRequest)							
	  ,((==) "/handlers/new/list", handleSessionRequest config flows handleNewListRequest)
	  ,((==) "/handlers/new/start", handleSessionRequest config flows handleNewStartRequest)
	  ,((==) "/handlers/work/list", handleSessionRequest config flows handleWorkListRequest)
	  ,((==) "/handlers/work/tab", handleSessionRequest config flows handleWorkTabRequest)
	  ,((==) "/handlers/work/property", handleSessionRequest config flows handlePropertyRequest)
	  ,((==) "/handlers/data/users", handleSessionRequest config flows handleUserListRequest)
	  ,((==) "/handlers/rpc/request", handleSessionRequest config flows handleRPCListRequest)
	  ,((==) "/handlers/rpc/response", handleSessionRequest config flows handleRPCUpdates)
	  ,((==) "/handlers/debug/taskforest", handleSessionRequest config flows handleTaskForestRequest)
	  ,((==) "/handlers/debug/processtable", handleSessionRequest config flows handleProcessTableRequest)
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

config :: !*World -> (!Config,!*World)
config world
	# (appName,world) = determineAppName world
	= loadConfig appName world

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
	
handleAnonRequest :: Config [Workflow] (HTTPRequest *TSt -> (!HTTPResponse, !*TSt)) !HTTPRequest *World -> (!HTTPResponse, !*World)
handleAnonRequest config flows handler request world
	# tst						= initTSt request config flows world
	# (response, tst)			= handler request tst
	# world						= finalizeTSt tst
	= (response, world)

handleSessionRequest :: Config [Workflow] (HTTPRequest *TSt -> (!HTTPResponse, !*TSt)) !HTTPRequest *World -> (!HTTPResponse, !*World)
handleSessionRequest config flows handler request world
	# tst						= initTSt request config flows world
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
 
initTSt :: !HTTPRequest !Config ![Workflow] !*World -> *TSt
initTSt request config flows world
	# (appName,world) = determineAppName world
	# (pathstr,world)			= determineAppPath world
	# ((ok, path),world)		= pd_StringToPath (pathstr) world
	| not ok					= abort "Cannot find the executable."
	# ((err,info),world)		= getFileInfo path world
	| err <> NoDirError			= abort "Cannot get executable info."
	# (date,time)				= info.pi_fileInfo.lastModified
	# datestr					= (toString date.Date.year)+++(addPrefixZero date.Date.month)+++(addPrefixZero date.Date.day)+++"-"+++(addPrefixZero time.Time.hours)+++(addPrefixZero time.Time.minutes)+++(addPrefixZero time.Time.seconds)
	= mkTSt appName config request (abort "session not active yet") flows (createStore (appName +++ "-systemStore")) (createStore (appName +++ "-dataStore-" +++ datestr)) world
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



