implementation module Engine

import StdMisc, StdArray, StdList, StdChar, GenBimap
import iDataSettings, iDataForms, iDataWidgets, iDataFormlib, iDataTrivial
import UserDB, ProcessDB, SessionDB
import Util
import BasicCombinators

import Http, HttpUtil

import AuthenticationHandler, DeauthenticationHandler
import NewListHandler, NewStartHandler, WorkListHandler, WorkTabHandler
import TaskTreeForestHandler, ProcessTableHandler

import TSt

import JSON
derive JSONDecode HtmlState, StorageFormat, Lifespan

// The iTasks engine consist of a set of HTTP request handlers
engine :: [Workflow] -> [(!String -> Bool, HTTPRequest *World -> (!HTTPResponse, !*World))] 
engine flows = [((==) "/handlers/authenticate", handleAnonRequest handleAuthenticationRequest)
			   ,((==) "/handlers/deauthenticate", handleSessionRequest flows handleDeauthenticationRequest)							
			   ,((==) "/handlers/new/list", handleSessionRequest flows handleNewListRequest)
			   ,((==) "/handlers/new/start", handleSessionRequest flows handleNewStartRequest)
			   ,((==) "/handlers/work/list", handleSessionRequest flows handleWorkListRequest)
			   ,((==) "/handlers/work/tab", handleSessionRequest flows handleWorkTabRequest)
			   ,((==) "/handlers/debug/tasktreeforest", handleSessionRequest flows handleTaskTreeForestRequest)
			   ,((==) "/handlers/debug/processtable", handleSessionRequest flows handleProcessTableRequest)
			   ,(\_ -> True, handleStaticResourceRequest)
				 ]

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

handleSessionRequest :: [Workflow] (HTTPRequest *TSt -> (!HTTPResponse, !*TSt)) !HTTPRequest *World -> (!HTTPResponse, !*World)
handleSessionRequest flows handler request world
	# hst						= initHSt request world
	# sessionId					= http_getValue "session" (request.arg_get ++ request.arg_post) ""
	# (mbSession,timeout,hst)	= restoreSession sessionId hst
	= case mbSession of
		Nothing
			# hst				= storeStates hst	
			# world				= finalizeHSt hst
			= ({http_emptyResponse & rsp_data = mkSessionFailureResponse timeout}, world)
		(Just session)
			# tst					= mkTSt LSTxtFile LSTxtFile session flows hst
			# (response,tst =:{hst})
									= handler request tst
			# hst					= storeStates hst
			# world					= finalizeHSt hst
			= (response, world)		
where
	mkSessionFailureResponse to = "{\"success\" : false, \"error\" : \"" +++ (if to "Your session timed out" "Failed to load session") +++ "\"}"

initHSt :: !HTTPRequest !*World -> *HSt
initHSt request world
	# (datafile,world)			= openmDataFile DataFileName world							// open the datafile if option chosen
	# nworld 					= mkNWorld world datafile									// Wrap all io states in an NWorld state
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
finalizeHSt hst =:{HSt | nworld = nworld =: {NWorld | world, datafile}}
	# world						= closemDataFile datafile world								// close the datafile if option chosen
	= world
	
// DataFile OPTION
openmDataFile datafile world
	:== IF_DataFile (openDataFile  datafile world) (abort "Trying to open a dataFile while this option is switched off",world)
closemDataFile datafile world
	:== IF_DataFile (closeDataFile datafile world) world
