implementation module iTasks.Framework.TaskServer

import StdFile, StdBool, StdInt, StdClass, StdList, StdMisc, StdArray
import Data.Maybe, System.Time, Data.List, Data.Map, Text
import TCPChannelClass, TCPChannels, TCPEvent, TCPStringChannels, TCPDef, tcp


from Internet.HTTP import :: HTTPRequest(..), :: HTTPResponse(..), :: HTTPUpload, :: HTTPProtocol
from Internet.HTTP import newHTTPRequest, newHTTPResponse
from Internet.HTTP import instance toString HTTPRequest, instance toString HTTPResponse

from HttpUtil import http_addRequestData, http_parseArguments
import iTasks.Framework.IWorld

// TCP level server
startServer :: !Int 
				(IPAddress *env -> (loc,*env)) ((Maybe {#Char}) loc *env -> *(Maybe {#Char},!Bool, !loc, !*env)) (loc *env -> *env)
				(*env -> (!Maybe Timeout,!*env)) (*env -> (!Bool,!*env)) *env -> *env | ChannelEnv env
startServer port handleNewConnection handleActiveConnection handleConnectionLost determineTimeout handleBackground env
	//Start the listener
    # (listener,env)  = startListener port env
	= serve listener [] [] [] env
where
	startListener port env
    	# (success, mbListener, env) = openTCP_Listener port env
    	| success   = (fromJust mbListener,env)
    	| otherwise = abort ("Error: The server port " +++ (toString port) +++ " is currently occupied!\n" +++
							 "Probably a previous application is still running and you have forgotten to close it.\n" +++
							 "It is also possible that another server running on your machine is using this port.\n\n\n")

	serve listener rChannels sChannels connStates env
		//Join the listener with the open channels
    	#! selectSet				= TCP_Pair (TCP_Listeners [listener]) (TCP_RChannels rChannels)
    	//Select the channel which has data available
		#! (mbTimeout,env)			= determineTimeout env
    	#! (chList,selectSet,_,env)	= selectChannel_MT mbTimeout selectSet TCP_Void env 
		// (this needs to be strict, else we start executing the background handler too early)
    	//Split the listener from the open channels
    	# (TCP_Pair (TCP_Listeners [listener:_]) (TCP_RChannels rChannels)) = selectSet
		//Check if the connection listener is in the change list
		//if so, accept the connection and append it to the lists
		# (listener,rChannels,sChannels,connStates,env) = case [who \\ (who,what) <- chList] of
			[0]	
 				# (tReport, mbNewConn, listener, env)   = receive_MT (Just 0) listener env
               	| tReport <> TR_Success	= (listener, rChannels,sChannels,connStates,env)
				# (ip,{sChannel,rChannel})				= fromJust mbNewConn
				# (connState,env)						= handleNewConnection ip env
				= (listener, rChannels ++ [rChannel], sChannels ++ [sChannel], connStates ++ [connState], env)
		
			_	= (listener,rChannels,sChannels,connStates,env)
		//Call the handler for all active connections
		//(we start handleActiveConnections with 1, because when the chList was created the listener is index 0 during the select)
		# (rChannels,sChannels,connStates,env)	= handleActiveConnections 1 rChannels sChannels connStates chList env
		//Call the background handler last
		# (done,env) = handleBackground env
		| done
			= closeAllConnections listener rChannels sChannels env
		| otherwise
			= serve listener rChannels sChannels connStates env

	handleActiveConnections i [] [] [] chList env
		= ([],[],[],env)
	handleActiveConnections i [rChannel:rChannels] [sChannel:sChannels] [connState:connStates] chList env
		//Check if the connection is in the select list. Read data if available and check if the connection was lost
		# (mbData,lost,rChannel,env) = case [what \\ (who,what) <- chList | who == i] of
			[SR_Available:_]	//Data available
				# (data,rChannel,env)	= receive rChannel env
				= (Just (toString data),False,rChannel,env)
			[_:_]				//Connection lost
				= (Nothing,True,rChannel,env)
			_					//No data
				= (Nothing,False,rChannel,env)
		| lost
			# env = handleConnectionLost connState env
 			# env = closeRChannel rChannel env
            # env = closeChannel sChannel env
			= handleActiveConnections (i + 1) rChannels sChannels connStates chList env
		//Call handler callback function
		# (mbData,closeConn,connState,env)	= handleActiveConnection mbData connState env
		//Send data if produced
		# (sChannel,env) = case mbData of
			(Just data)	= send (toByteSeq data) sChannel env
			_			= (sChannel,env)
		//Close the connection
		| closeConn
 			# env = closeRChannel rChannel env
            # env = closeChannel sChannel env
			= handleActiveConnections (i + 1) rChannels sChannels connStates chList env
		//Process the remaining connections
		# (rChannels,sChannels,connStates,env)	= handleActiveConnections (i + 1) rChannels sChannels connStates chList env
		= ([rChannel:rChannels],[sChannel:sChannels],[connState:connStates],env)
	
	closeAllConnections listener [] [] env
		= closeRChannel listener env //Close the listener last
	closeAllConnections listener [rChannel:rChannels] [sChannel:sChannels] env
		# env = closeRChannel rChannel env
		# env = closeChannel sChannel env
		= closeAllConnections listener rChannels sChannels env

// (Streaming) HTTP level server
// The server at this level expects that connections use the HTTP protocol and thus can
// aggregate HTTP request headers or full http requests and use the requested path to select
// an appropriate handler function

:: HttpConnState loc
    = Idle String Timestamp
    | ReadingRequest HttpReqState
	| ProcessingRequest HTTPRequest loc

:: HttpReqState =
    { request       :: HTTPRequest
    , method_done   :: Bool
    , headers_done  :: Bool
    , data_done     :: Bool
    , error         :: Bool
    }

//For keep-alive connections we need to keep track of when a connection
class HttpServerEnv env
where
    serverTime :: *env -> (!Timestamp,!*env)

instance HttpServerEnv World
where
    serverTime :: *World -> (!Timestamp,!*World)
    serverTime world = time world

startHTTPServer :: !Int !Int
						[(!(String -> Bool)
						 ,!Bool
						 ,!(HTTPRequest *env -> (!HTTPResponse,!Maybe loc,!*env))
						 ,!(HTTPRequest (Maybe {#Char}) loc *env -> (!Maybe {#Char}, !Bool, loc, !*env))
						 ,!(HTTPRequest loc *env -> *env)
						 )] (*env -> (!Maybe Timeout,!*env)) (*env -> (!Bool,!*env)) *env -> *env | ChannelEnv env & HttpServerEnv env
startHTTPServer port keepAliveTime requestProcessHandlers determineTimeout handleBackground env
	= startServer port handleNewConnection handleActiveConnection handleConnectionLost determineTimeoutHTTP handleBackground env
where
	determineTimeoutHTTP env
		# (mbTimeout,env)	= determineTimeout env
		# keepAliveTimeout	= keepAliveTime * 1000
		= case mbTimeout of
			Nothing			= (Just keepAliveTimeout, env)
			Just t			= (Just (if (t < keepAliveTimeout) t keepAliveTimeout), env)

	handleNewConnection ip env
		# (now,env) = serverTime env
		= (Idle (toString ip) now, env)

	handleActiveConnection mbData connState=:(ProcessingRequest request localState) env
		//Select handler based on request path
		= case selectHandler request requestProcessHandlers of
			Just (_,_,_,handler,_) 
				# (mbData,done,localState,env) = handler request mbData localState env
				| done && isKeepAlive request	//Don't close the connection if we are done, but keepalive is enabled
					# (now,env)       = serverTime env
					= (mbData, False, Idle request.client_name now,env)
				| otherwise
					= (mbData,done,ProcessingRequest request localState,env)
			Nothing
				= (Just "HTTP/1.0 400 Bad Request\r\n\r\n", True, connState, env)
	handleActiveConnection (Just data) connState env  //(connState is either Idle or ReadingRequest)
		# rstate = case connState of
			(Idle client_name _)
				//Add new data to the request
				# request   = {newHTTPRequest & client_name = client_name, server_port = port}
				# (request, method_done, headers_done, data_done, error) = http_addRequestData request False False False data
				= {request=request,method_done=method_done,headers_done=headers_done,data_done=data_done,error=error}
			(ReadingRequest {request, method_done, headers_done, data_done})
				//Add new data to the request
				# (request, method_done, headers_done, data_done, error) = http_addRequestData request method_done headers_done data_done (toString data)
				= {request=request,method_done=method_done,headers_done=headers_done,data_done=data_done,error=error}
			_
				= {request=newHTTPRequest,method_done=False,headers_done=False,data_done=False,error=True}
		| rstate.HttpReqState.error
			//Sent bad request response and disconnect
			= (Just "HTTP/1.0 400 Bad Request\r\n\r\n", True, connState, env)
		| not rstate.headers_done
			//Without headers we can't select our handler functions yet
			= (Nothing, False, ReadingRequest rstate, env)
		//Determine the handler
		= case selectHandler rstate.HttpReqState.request requestProcessHandlers of
			Nothing
				= (Just "HTTP/1.0 404 Not Found\r\n\r\n", True, connState, env)
			Just (_,completeRequest,newReqHandler,procReqHandler,_)
				//Process a completed request, or as soon as the headers are done if the handler indicates so
				| rstate.data_done || (not completeRequest)
					# request	= if completeRequest (http_parseArguments rstate.request) rstate.request
					//Determine if a  persistent connection was requested
					# keepalive	= isKeepAlive request 
					// Create a response
					# (response,mbLocalState,env)	= newReqHandler request env
					//Add keep alive header if necessary
					# response	= if keepalive {response & rsp_headers = put "Connection" "Keep-Alive" response.rsp_headers} response
					// Encode the response to the HTTP protocol format
					= case mbLocalState of
						Nothing	
							# reply		= encodeResponse True response
							| keepalive
								# (now,env)       = serverTime env
								= (Just reply, False, Idle rstate.request.client_name now,env)
							| otherwise
								= (Just reply, True, connState, env)
						Just localState	
							= (Just (encodeResponse False response), False, ProcessingRequest request localState, env)
				| otherwise
					= (Nothing, False, ReadingRequest rstate, env)		

	//Close idle connections if the keepalive time has passed
	handleActiveConnection Nothing connState=:(Idle ip (Timestamp t)) env
		# (Timestamp now,env)	= serverTime env	//TODO: Do we really need to do this for every connection all the time?
		= (Nothing, now >= t + keepAliveTime, connState, env)

	//Do nothing if no data arrives for now
	handleActiveConnection Nothing connState env = (Nothing,False,connState,env)

	//If we were processing a request and were interupted we need to
	//select the appropriate handler to wrap up
	handleConnectionLost (ProcessingRequest request loc) env
		= case selectHandler request requestProcessHandlers of
			Nothing	= env
			Just (_,_,_,_,connLostHandler) = connLostHandler request loc env

	handleConnectionLost _ env = env

	selectHandler req [] = Nothing
	selectHandler req [h=:(pred,_,_,_,_):hs]
		| pred req.req_path	= Just h
							= selectHandler req hs

	isKeepAlive request = maybe (request.req_version == "HTTP/1.1") (\h -> (toLowerCase h == "keep-alive")) (get "Connection" request.req_headers)

encodeResponse :: !Bool !HTTPResponse -> String
encodeResponse autoContentLength {rsp_headers, rsp_data}
	= join "\r\n" (
		["HTTP/1.0 " +++ fromMaybe "200 OK" (get "Status" rsp_headers)
		,"Server: " +++ fromMaybe "iTasks HTTP Server" (get "Server" rsp_headers)					//Server identifier
    	,"Content-Type: " +++ fromMaybe "text/html" (get "Content-Type" rsp_headers)				//Content type header
		] ++ (if autoContentLength																	//Content length header
				["Content-Length: " +++ fromMaybe (toString (size rsp_data)) (get "Content-Length" rsp_headers)]
				[] ) ++
			[(n +++ ": " +++ v) \\ (n,v) <- toList rsp_headers | not (skipHeader n)] ++				//Additional headers
			["",rsp_data])
where
    skipHeader s = isMember s ["Status","Server","Content-Type","Content-Length"]					//Do not add these headers two times

simpleHTTPResponse ::
	(!(String -> Bool),HTTPRequest *env -> (!HTTPResponse,*env))
	->
	(!(String -> Bool),!Bool,!(HTTPRequest *env -> (HTTPResponse, Maybe loc,*env))
							,!(HTTPRequest (Maybe {#Char}) loc *env -> (!Maybe {#Char}, !Bool, loc, !*env))
							,!(HTTPRequest loc *env -> *env))
simpleHTTPResponse (pred,responseFun) = (pred,True,initFun,dataFun,lostFun)
where
	initFun req env
		# (rsp,env) = responseFun req env
		= (rsp,Nothing,env)
		
	dataFun _ _ s env = (Nothing,True,s,env)
	lostFun _ s env = env

// Task level server

//Wrapper instance for TCP channels with IWorld
instance ChannelEnv IWorld
where
	channelEnvKind iworld=:{IWorld|world}
		# (kind,world) = channelEnvKind world
		= (kind,{IWorld|iworld & world = world})
	
	mb_close_inet_receiver_without_id b (endpoint,cat) iworld=:{IWorld|world}
		= {IWorld|iworld & world = mb_close_inet_receiver_without_id b (endpoint,cat) world}
	
	channel_env_get_current_tick iworld=:{IWorld|world}
		# (tick,world) = channel_env_get_current_tick world
		= (tick,{IWorld|iworld & world = world})
	
