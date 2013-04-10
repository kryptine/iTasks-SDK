implementation module HttpServer

import StdList, StdTuple, StdArray, StdFile, StdBool, StdMisc
import StdMaybe

import 	Time, Map, Text
import	TCPChannelClass,
		TCPChannels,
		TCPEvent,
		TCPStringChannels,
		TCPDef
		
from HTTP import :: HTTPRequest(..), :: HTTPResponse(..), :: HTTPUpload, :: HTTPProtocol, :: Map
from HTTP import newHTTPRequest
from HTTP import instance toString HTTPRequest, instance toString HTTPResponse

from HttpUtil import http_addRequestData, http_parseArguments, http_makeResponse, http_encodeResponse, http_serverControl

:: ConnectionState
	= Idle String Timestamp
	| ReadingReq RequestState

:: RequestState =
	{ request		:: HTTPRequest
	, method_done	:: Bool
	, headers_done	:: Bool
	, data_done		:: Bool
	, error			:: Bool
	}

class HttpEnv st
where
	httpServerTimestamp :: *st -> (!Timestamp,!*st)

instance HttpEnv World
where
	httpServerTimestamp :: *World -> (!Timestamp,!*World)
	httpServerTimestamp world = time world

//Start the HTTP server
http_startServer :: ![HTTPServerOption *st] [(!(String -> Bool),!(HTTPRequest *st-> (!HTTPResponse,!*st)))] !*st -> *st | ChannelEnv st & FileSystem st & HttpEnv st
http_startServer options handlers world
	//Start the listener
	# (listener,world)	= startListener (getPortOption options) world
	//Enter the listen loop
	= loop options handlers listener [] [] [] world
	
// Try to open a listener on the given port
startListener :: Int !*st -> (TCP_Listener,!*st) | ChannelEnv st
startListener port world
	# (success, mbListener, world) = openTCP_Listener port world
	| success	= (fromJust mbListener,world)
	| otherwise = abort ("Error: The server port " +++ (toString port) +++ " is currently occupied!\n" +++
						 "Probably a previous application is still running and you have forgotten to close it.\n" +++
						 "It is also possible that another web server running on your machine is using this port.\n\n\n")

//Main event loop, it is called each time a client connects or data arrives
loop ::	[HTTPServerOption *st]
		[(!(String -> Bool),!(HTTPRequest *st -> (!HTTPResponse,!*st)))]
		TCP_Listener [TCP_RChannel] [TCP_SChannel]
		[ConnectionState]
		*st -> *st | ChannelEnv st & FileSystem st & HttpEnv st
loop options handlers listener rchannels schannels connStates world
	# (mbTimeout, world) = executeBackgroundProcess options world
	//Join the listener with the open channels
	# glue = TCP_Pair (TCP_Listeners [listener]) (TCP_RChannels rchannels)
	//Select the channel which has data available
	# (chlist,glue,_,world) = selectChannel_MT mbTimeout glue TCP_Void world
	//Split the listener from the open channels
	# (TCP_Pair (TCP_Listeners [listener:_]) (TCP_RChannels rchannels)) = glue
	= case chlist of
		[(who,what):_]
			//A new client attempts to connect
			| who == 0
				# world										= debug 1 ("New connection opened " +++ toString (length connStates)) options world
				# (tReport, mbNewMember, listener, world)	= receive_MT (Just 0) listener world
				| tReport <> TR_Success						= loop options handlers listener rchannels schannels connStates world //Just continue
				# (ip,{sChannel,rChannel})					= fromJust mbNewMember
				# (now,world)								= httpServerTimestamp world
				= loop options handlers listener [rChannel:rchannels] [sChannel:schannels] [Idle (toString ip) now:connStates] world		
			//A client has new data
			| otherwise
				// Select the offset without the listener
				# who = who - 1
				// Select the right read channel from the list
				# (currentrchannel, rchannels) = selectFromList who rchannels
				// Select the right write channel from the list
				# (currentschannel, schannels) = selectFromList who schannels
				// Select the right incomplete request from the list
				# (connState , connStates) = selectFromList who connStates
				// New data is available
				| what == SR_Available
					// Fetch the new data from the receive channel
					# (data,currentrchannel,world) = receive currentrchannel world
					# rstate = case connState of		
						(Idle client_name _)
							//Add new data to the request
							# request	= {newHTTPRequest & client_name = client_name, server_port = getPortOption options}
							# (request, method_done, headers_done, data_done, error) = http_addRequestData request False False False (toString data)
							= {request=request,method_done=method_done,headers_done=headers_done,data_done=data_done,error=error}
						(ReadingReq {request, method_done, headers_done, data_done})
							//Add new data to the request
							# (request, method_done, headers_done, data_done, error) = http_addRequestData request method_done headers_done data_done (toString data)
							= {request=request,method_done=method_done,headers_done=headers_done,data_done=data_done,error=error}
						_
							= {request=newHTTPRequest,method_done=False,headers_done=False,data_done=False,error=True}
					//Add new data to the request
					| rstate.RequestState.error
						//Sent bad request response and disconnect
						# (currentschannel,world) = send (toByteSeq "HTTP/1.0 400 Bad Request\r\n\r\n") currentschannel world
						# world = closeRChannel currentrchannel world
						# world = closeChannel currentschannel world
						= loop options handlers listener rchannels schannels connStates world
				
					//Process a completed request
					| rstate.method_done && rstate.headers_done && rstate.data_done
						# request			= if (getParseOption options) (http_parseArguments rstate.request) rstate.request
						# world				= debug 1 "Processing request:" options world
						# world				= debug 2 request options world
						//Determine if a  persistent connection was requested
						# keepalive			= maybe (request.req_version == "HTTP/1.1") (\h -> (toLowerCase h == "keep-alive")) (get "Connection" request.req_headers)
						// Create a response
						# (response,world)	= http_makeResponse request handlers (getStaticOption options) world
						# world				= debug 1 "Generated response:" options world
						# world				= debug 2 response options world
						//Add keep alive header if necessary
						# response			= if keepalive {response & rsp_headers = put "Connection" "Keep-Alive" response.rsp_headers} response
						// Encode the response to the HTTP protocol format
						# reply				= http_encodeResponse response True
						// Send the encoded response to the client
						# (currentschannel,world) = send (toByteSeq reply) currentschannel world
						# world				= debug 1 "Sent encoded reply:" options world
						# world				= debug 2 reply options world
						// Check for the server control header
						= case http_serverControl response of	
							"stop"
								= closeRChannel listener world
							"restart"
								# world				= closeRChannel listener world
								# (listener,world)	= startListener (getPortOption options) world
								= loop options handlers listener [] [] [] world
							_
								| keepalive
									//Switch connection to idle
									# world				= debug 1 "Setting connection to idle" options world	
									# (now,world)		= httpServerTimestamp world
									= loop options handlers listener [currentrchannel:rchannels] [currentschannel:schannels] [Idle rstate.request.client_name now:connStates] world
								| otherwise
									// Close the connection
									# world	= debug 1 "Closing connection" options world	
									# world = closeChannel currentschannel world
									# world = closeRChannel currentrchannel world
									= loop options handlers listener rchannels schannels connStates world	
								
					//We do not have everything we need yet, so continue
					| otherwise = loop options handlers listener [currentrchannel:rchannels] [currentschannel:schannels] [ReadingReq rstate:connStates] world
					
				//We lost the connection
				| otherwise
					# world	= debug 1 "Connection lost" options world	
					# world = closeRChannel currentrchannel world
					# world = closeChannel currentschannel world
					= loop options handlers listener rchannels schannels connStates world
		_ // timeout
			//TODO close idle connections with a timestamp that is to old
			= loop options handlers listener rchannels schannels connStates world
where
	selectFromList nr list
		# (left,[element:right]) = splitAt nr list
		= (element,left++right)

getPortOption :: [HTTPServerOption *st] -> Int
getPortOption [] = 80
getPortOption [x:xs] = case x of (HTTPServerOptPort port)	= port
								 _							= getPortOption xs

getStaticOption :: [HTTPServerOption *st] -> Bool
getStaticOption [] = False
getStaticOption [x:xs] = case x of (HTTPServerOptStaticFallback b) = b
								   _							   = getStaticOption xs

getParseOption :: [HTTPServerOption *st] -> Bool
getParseOption [] = True
getParseOption [x:xs] = case x of (HTTPServerOptParseArguments b)	= b						
								  _									= getParseOption xs

getDebugOption :: [HTTPServerOption *st] -> Int
getDebugOption [] = 0
getDebugOption [x:xs] = case x of (HTTPServerOptDebug l)	= l
								  _							= getDebugOption xs
								  
executeBackgroundProcess :: ![HTTPServerOption *st] !*st -> (!Maybe Timeout, !*st)
executeBackgroundProcess [] env = (Nothing, env)
executeBackgroundProcess [x:xs] env = case x of
	(HTTPServerOptBackgroundProcess bproc)	= bproc env
	_										= executeBackgroundProcess xs env
								  
debug:: Int a [HTTPServerOption *st] *st -> *st | toString a & FileSystem st
debug level msg options world
	| level > (getDebugOption options)	= world
	# (sio, world)						= stdio world
	# sio								= fwrites ((toString msg) +++ "\n") sio
	= snd (fclose sio world)
