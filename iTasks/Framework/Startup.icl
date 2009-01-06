implementation module Startup
// *********************************************************************************************************************************
// The iTasks library enables the specification of interactive multi-user workflow tasks (iTasks) for the web.
// This module contains iTask kernel.
// This library is still under construction - MJP
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdEnv
import iDataSettings, iDataForms, iDataWidgets, iDataFormlib, iDataTrivial
import iTasksSettings, InternaliTasksCommon, InternaliTasksThreadHandling
import BasicCombinators, iTasksProcessHandling

import Http, HttpUtil, HttpServer, HttpTextUtil, sapldebug
import AuthenticationHandler, DeauthenticationHandler, NewListHandler, NewStartHandler, WorkListHandler, WorkTabHandler //iTasks.Framework.Handlers.*
import TaskTreeForestHandler, ProcessTableHandler, ThreadTableHandler
import TaskTree, TaskTreeFilters

import Session //iTasks.Framework.Session

import JSON
derive JSONDecode HtmlState, StorageFormat, Lifespan

// ******************************************************************************************************
// *** Server / Client startup
// ******************************************************************************************************
startTaskEngine :: !(Task a) !*World -> *World  	| iData a
startTaskEngine maintask world = doHtmlServer maintask world		

doHtmlServer :: (Task a) !*World -> *World | iData a
doHtmlServer mainTask world
| ServerKind == Internal
	# world	= instructions world
	= startServer mainTask world		// link in the Clean http 1.0 server	
//| ServerKind == CGI					// build as CGI application
| otherwise
	= unimplemented world
where
	instructions :: *World -> *World
	instructions world
		# (console, world)	= stdio world
		# console			= fwrites "HTTP server started...\n" console
		# console			= fwrites ("Please point your browser to http://localhost/\n") console
		# (_,world)			= fclose console world
		= world
		
	unimplemented :: *World -> *World
	unimplemented world
		# (console, world)	= stdio world
		# console			= fwrites "The chosen server mode is not supported.\n" console
		# console			= fwrites "Please select ServerKind Internal in iDataSettings.dcl.\n" console
		# (_,world)			= fclose console world
		= world

startServer :: (Task a) !*World -> *World | iData a
startServer mainTask world
	# options = ServerOptions ++ (if TraceHTTP [HTTPServerOptDebug True] [])
	= http_startServer options   [((==) "/handlers/authenticate", handleAnonRequest handleAuthenticationRequest)
								 ,((==) "/handlers/deauthenticate", handleSessionRequest handleDeauthenticationRequest)							
								 ,((==) "/handlers/new/list", handleSessionRequest handleNewListRequest)
								 ,((==) "/handlers/new/start", handleSessionRequest handleNewStartRequest)
								 ,((==) "/handlers/work/list", handleSessionRequest (handleWorkListRequest mainTask))
								 ,((==) "/handlers/work/tab", handleSessionRequest (handleWorkTabRequest mainTask))
								 ,((==) "/handlers/debug/tasktreeforest", handleSessionRequest (handleTaskTreeForestRequest mainTask))
								 ,((==) "/handlers/debug/processtable", handleSessionRequest (handleProcessTableRequest mainTask))
								 ,((==) "/handlers/debug/threadtable", handleSessionRequest (handleThreadTableRequest mainTask))
								 ,(\_ -> True, handleStaticResourceRequest)
								 ] world

// Request handler which serves static resources from the application directory,
// or a system wide default directory if it is not found locally.
// This request handler is used for serving system wide javascript, css, images, etc...

handleStaticResourceRequest :: !HTTPRequest *World -> (!HTTPResponse, !*World)
handleStaticResourceRequest req world
	# path					= if (req.req_path == "/") "/index.html" req.req_path
	# filename				= MyAbsDir +++ path	
	# (type, world)			= http_staticFileMimeType filename world
	# (ok, content, world)	= http_staticFileContent filename world
	| ok					= ({rsp_headers = [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size content))]
							   ,rsp_data = content}, world)
	# filename				= ResourceDir +++ path
	# (type, world)			= http_staticFileMimeType filename world
	# (ok, content, world)	= http_staticFileContent filename world
	|  ok 					= ({rsp_headers = [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size content))]
							   	,rsp_data = content}, world)		 							   
	= http_notfoundResponse req world


handleAnonRequest :: (HTTPRequest *HSt -> (!HTTPResponse, !*HSt)) !HTTPRequest *World -> (!HTTPResponse, !*World)
handleAnonRequest handler request world
	# hst						= initHSt request world
	# (response, hst)			= handler request hst
	# world						= finalizeHSt hst
	= (response, world)

handleSessionRequest :: (HTTPRequest Session *HSt -> (!HTTPResponse, !*HSt)) !HTTPRequest *World -> (!HTTPResponse, !*World)
handleSessionRequest handler request world
	# hst						= initHSt request world
	# sessionId					= http_getValue "session" (request.arg_get ++ request.arg_post) ""
	# (mbSession,timeout,hst)	= restoreSession sessionId hst
	= case mbSession of
		Nothing
			# hst				= storeStates hst	
			# world				= finalizeHSt hst
			= ({http_emptyResponse & rsp_data = mkSessionFailureResponse timeout}, world)
		(Just session)
			# (response, hst)	= handler request session hst 
			# hst				= storeStates hst
			# world				= finalizeHSt hst
			= (response, world)		
where
	mkSessionFailureResponse to = "{\"success\" : false, \"error\" : \"" +++ (if to "Your session timed out" "Failed to load session") +++ "\"}"

initHSt :: !HTTPRequest !*World -> *HSt
initHSt request world
	# (gerda,world)				= openDatabase ODCBDataBaseName world						// open the relational database if option chosen
	# (datafile,world)			= openmDataFile DataFileName world							// open the datafile if option chosen
	# nworld 					= mkNWorld world datafile gerda								// Wrap all io states in an NWorld state
	# updates					= decodeFormUpdates request.arg_post						// Get the form updates from the post
	# states					= decodeHtmlStates request.arg_post							// Fetch stored states from the post
	# fstates	 				= mkFormStates states updates 								
	= mkHSt "" request fstates nworld
where
	decodeFormUpdates :: ![(!String, !String)] -> [FormUpdate]
	decodeFormUpdates args = [update \\ (Just update) <- map mbUpdate args]
	where
		mbUpdate (name, value)	= case mbInputId name ((size name) - 1) of
			Nothing			= Nothing
			Just inputid	= Just {FormUpdate | formid = name % (0, (size name) - (size inputid) - 2), inputid = toInt inputid, value = value}
	
		mbInputId "" _		= Nothing
		mbInputId name i
			| name.[i] == '-' && i < ((size name) - 1)	= Just (name % (i + 1, size name))	//Found the marker
			| isDigit name.[i]							= mbInputId name (i - 1)			//Move cursor one position to the left
														= Nothing							//We've hit an unexpected character
	
	decodeHtmlStates :: ![(!String, !String)] -> [HtmlState]
	decodeHtmlStates args = case fromJSON (http_getValue "state" args "") of
		Nothing	= []			//Parsing failed
		Just states = states 


finalizeHSt :: !*HSt -> *World
finalizeHSt hst =:{world = nworld =: {worldC = world, gerda, datafile}}
	# world						= closeDatabase gerda world									// close the relational database if option chosen
	# world						= closemDataFile datafile world								// close the datafile if option chosen
	= world
	
	
// Database OPTION
openDatabase database world
	:== IF_Database (openGerda database world) (abort "Trying to open a relational database while this option is switched off",world)
closeDatabase database world
	:== IF_Database (closeGerda database world) world

// DataFile OPTION
openmDataFile datafile world
	:== IF_DataFile (openDataFile  datafile world) (abort "Trying to open a dataFile while this option is switched off",world)
closemDataFile datafile world
	:== IF_DataFile (closeDataFile datafile world) world
