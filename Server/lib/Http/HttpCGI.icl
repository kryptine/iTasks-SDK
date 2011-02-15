implementation module HttpCGI

import StdFile, StdInt, StdBool, StdArray
import HTTP, HttpUtil
import Text, Environment

//Http headers for which should be checked if they exist in the environment
HTTP_CGI_HEADERS :==[ ("Content-Type","CONTENT_TYPE")
					, ("Content-Length","CONTENT_LENGTH")
					, ("Content-Encoding","HTTP_CONTENT_ENCODING")
					, ("Accept","HTTP_ACCEPT")
					, ("User-Agent","HTTP_USER_AGENT")
					, ("Host", "HTTP_HOST")
					, ("Authorization","HTTP_AUTHORIZATION")
					, ("If-Modified-Since","HTTP_IF_MODIFIED_SINCE")
					, ("Referer","HTTP_REFERER")
					]

//Starts the CGI Wrapper
http_startCGI :: [HTTPCGIOption] [((String -> Bool),(HTTPRequest *World-> (HTTPResponse,*World)))] *World -> *World
http_startCGI options handlers world
	# (console, world)		= stdio world
	# (ok,console)			= freopen console FReadData
	//Read post data
	# (datalen,world)		= getDataLength world
	# (data, console)		= getData datalen console		
	//Create the request
	# (req_method,world)	= getFromEnv "REQUEST_METHOD" world
	# (req_path,world)		= getFromEnv "SCRIPT_NAME" world
	# (req_query,world)		= getFromEnv "QUERY_STRING" world
	# (req_version,world)	= getFromEnv "SERVER_PROTOCOL" world
	# (req_headers,world)	= makeHeaders HTTP_CGI_HEADERS world
	# (server_name,world)	= getFromEnv "SERVER_NAME" world
	# (server_port,world)	= getFromEnv "SERVER_PORT" world
	# (client_name,world)	= getClientName world
	# request				= {newHTTPRequest &	req_method = req_method,	
												req_path = req_path,
												req_query = req_query,
												req_version = req_version,
												req_headers = fromList req_headers,
												req_data = data,
												server_name = server_name,
												server_port = toInt server_port,
												client_name = client_name}
	# request				= if (getParseOption options) (http_parseArguments request) request
	# (response,world)		= http_makeResponse request handlers (getStaticOption options) world
	# (response,world)		= http_encodeResponse response False world
	# (ok,console)			= freopen console FWriteData
	# console				= fwrites response console
	# (ok,world)			= fclose console world
	= world

getDataLength :: !*World -> (!Int,!*World)
getDataLength world
	# (mbLen,world)		= getEnvironmentVariable "CONTENT_LENGTH" world
	= case mbLen of
		Nothing			= (0,world)
		Just len		= (toInt len, world)

getData :: !Int !*File -> (!String, !*File)
getData len file = freads file len

getFromEnv :: !String !*World -> (!String,!*World)
getFromEnv name world
	# (mbVal,world)	= getEnvironmentVariable name world
	= case mbVal of 
		Nothing		= ("",world)
		(Just v)	= (v,world)
						
getClientName :: !*World -> (!String,!*World)
getClientName world
	# (mbHost,world)	= getEnvironmentVariable "REMOTE_HOST" world
	= case mbHost of
		Just host	= (host,world)
		Nothing
			# (mbAddr,world)	= getEnvironmentVariable "REMOTE_ADDR" world
			= case mbAddr of
				Just addr		= (addr,world)
				Nothing			= ("",world)

							
makeHeaders :: [(String,String)] !*World -> ([(String,String)],!*World)
makeHeaders [] world = ([],world)
makeHeaders [(name,envname):xs] world	
	# (headersXs,world)	= makeHeaders xs world
	# (mbValue,world)	= getEnvironmentVariable envname world
	= case mbValue of
		Just value		= ([(name,value):headersXs],world)
		Nothing			= (headersXs,world)
	
getStaticOption :: [HTTPCGIOption] -> Bool
getStaticOption [] = False
getStaticOption [x:xs] = case x of	(HTTPCGIOptStaticFallback b) = b
									_							 = getStaticOption xs

getParseOption	:: [HTTPCGIOption] -> Bool
getParseOption [] = True
getParseOption [x:xs] = case x of (HTTPCGIOptParseArguments b)	= b
								  _								= getParseOption xs
