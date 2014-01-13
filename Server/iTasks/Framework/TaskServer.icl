implementation module iTasks.Framework.TaskServer

import StdFile, StdBool, StdInt, StdClass, StdList, StdMisc, StdArray, StdTuple, StdOrdList
import Data.Maybe, Data.Functor, System.Time, Data.List, Data.Map, Text
import TCPChannelClass, TCPChannels, TCPEvent, TCPStringChannels, TCPDef, tcp

from Internet.HTTP import :: HTTPRequest(..), :: HTTPResponse(..), :: HTTPUpload, :: HTTPProtocol, :: HTTPMethod
from Internet.HTTP import newHTTPRequest, newHTTPResponse
from Internet.HTTP import instance toString HTTPRequest, instance toString HTTPResponse

from HttpUtil import http_addRequestData, http_parseArguments
import iTasks.Framework.IWorld
import iTasks.Framework.Task

//Helper type that holds the mainloop instances during a select call
//in these mainloop instances the unique listeners and read channels
//have been temporarily removed.
:: *MainLoopInstanceDuringSelect
    = ListenerInstanceDS !Int !NetTask
    | ConnectionInstanceDS !IPAddress !*TCP_SChannel !NetTask !NetTaskState
    | BackgroundInstanceDS !BackgroundTask

serve :: !Int !NetTask !BackgroundTask (*IWorld -> (!Maybe Timeout,!*IWorld)) *IWorld -> *IWorld
serve port nt bt determineTimeout iworld
    = loop determineTimeout (init port nt bt iworld)

init :: !Int !NetTask !BackgroundTask !*IWorld -> *IWorld
init port nt bt iworld=:{IWorld|loop,world}
    # (success, mbListener, world) = openTCP_Listener port world
    | not success = abort ("Error: port "+++ toString port +++ " already in use.\n")
    = {iworld & loop = {done=[],todo=[ListenerInstance port (fromJust mbListener) nt,BackgroundInstance bt]}, world = world}

loop :: !(*IWorld -> (!Maybe Timeout,!*IWorld)) !*IWorld -> *IWorld
loop determineTimeout iworld
    # (mbTimeout,iworld=:{IWorld|loop={todo},world}) = determineTimeout iworld
    //Check which mainloop tasks have data available
    # (todo,chList,world) = select mbTimeout todo world
    //Process the select result
    # iworld =:{shutdown,loop={done}} = process 0 chList {iworld & loop = {done=[],todo=todo}, world = world}
    //Move everything from the done list  back to the todo list
    # iworld = {iworld & loop={todo = reverse done,done=[]}}
    //Everything needs to be re-evaluated
    | shutdown  = halt iworld
    | otherwise = loop determineTimeout iworld

select :: (Maybe Timeout) *[MainLoopInstance] *World -> (!*[MainLoopInstance],![(Int,SelectResult)],!*World)
select mbTimeout mlInstances world
    # (listeners,rChannels,mlInstances)
        = toSelectSet mlInstances
    # (chList,(TCP_Pair (TCP_Listeners listeners) (TCP_RChannels rChannels)),_,world)	
        = selectChannel_MT mbTimeout (TCP_Pair (TCP_Listeners listeners) (TCP_RChannels rChannels)) TCP_Void world
    # (mlInstances, chList)
        = fromSelectSet listeners rChannels mlInstances chList
    = (mlInstances, chList, world)

toSelectSet :: !*[MainLoopInstance] -> *(!*[*TCP_Listener],!*[*TCP_RChannel],!*[*MainLoopInstanceDuringSelect])
toSelectSet [] = ([],[],[])
toSelectSet [i:is]
    # (ls,rs,is) = toSelectSet is
    = case i of
        ListenerInstance port l nt = ([l:ls],rs,[ListenerInstanceDS port nt:is])
        ConnectionInstance ip {rChannel,sChannel} nt state = (ls,[rChannel:rs],[ConnectionInstanceDS ip sChannel nt state:is])
        BackgroundInstance bt = (ls,rs,[BackgroundInstanceDS bt:is])

/* Restore the list of main loop instances.
    In the same pass also update the indices in the select result to match the
    correct indices of the main loop instance list.
*/
fromSelectSet :: !*[*TCP_Listener] !*[*TCP_RChannel] !*[*MainLoopInstanceDuringSelect] ![(!Int,!SelectResult)] -> *(![*MainLoopInstance],![(!Int,!SelectResult)])
fromSelectSet ls rs is chList = fromSS 0 0 ls rs is (sortBy (\(x,_) (y,_) -> (x < y)) chList)
where
    fromSS offset i ls rs [] [] = ([],[])
    fromSS offset i [l:ls] rs [ListenerInstanceDS port nt:is] []
        # (is,_) = fromSS offset (i+1) ls rs is []
        = ([ListenerInstance port l nt:is],[])
    fromSS offset i [l:ls] rs [ListenerInstanceDS port nt:is] chList=:[(who,what):ws]
        | who + offset == i
            # (is,ws) = fromSS offset (i+1) ls rs is ws
            = ([ListenerInstance port l nt:is],[(who + offset,what):ws])
        | otherwise
            # (is,chList) = fromSS offset (i+1) ls rs is chList
            = ([ListenerInstance port l nt:is],chList)
    fromSS offset i ls [rChannel:rs] [ConnectionInstanceDS ip sChannel nt state:is] []
        # (is,_) = fromSS offset (i+1) ls rs is []
        = ([ConnectionInstance ip {rChannel=rChannel,sChannel=sChannel} nt state:is],[])
    fromSS offset i ls [rChannel:rs] [ConnectionInstanceDS ip sChannel nt state:is] chList=:[(who,what):ws]
        | who + offset == i
            # (is,ws) = fromSS offset (i+1) ls rs is ws
            = ([ConnectionInstance ip {rChannel=rChannel,sChannel=sChannel} nt state:is],[(who + offset,what):ws])
        | otherwise
            # (is,chList) = fromSS offset (i+1) ls rs is chList
            = ([ConnectionInstance ip {rChannel=rChannel,sChannel=sChannel} nt state:is],chList)
    fromSS offset i ls rs [BackgroundInstanceDS bt:is] chList
        # (is,chList) = fromSS (offset+1) (i+1) ls rs is chList
        = ([BackgroundInstance bt:is],chList)

process :: !Int [(!Int,!SelectResult)] !*IWorld -> *IWorld
process i chList iworld=:{loop={done,todo=[]}} = iworld
process i chList iworld=:{loop={done,todo=[ListenerInstance port listener nt:todo]},world}
    # (mbSelect,chList) = checkSelect i chList
    | mbSelect =:(Just _)
 	    # (tReport, mbNewConn, listener, world)   = receive_MT (Just 0) listener world
        | tReport == TR_Success
            # (ts,world)    = time world
            # (ip,conn)     = fromJust mbNewConn
            # todo          = todo ++ [ConnectionInstance ip conn nt (NTIdle (toString ip) ts)]
            = process (i+1) chList {iworld & loop={done=[ListenerInstance port listener nt:done],todo=todo}, world=world}
        = process (i+1) chList {iworld & loop={done=[ListenerInstance port listener nt:done],todo=todo}, world=world}
    = process (i+1) chList {iworld & loop={done=[ListenerInstance port listener nt:done],todo=todo}, world=world}
process i chList iworld=:{loop={done,todo=[ConnectionInstance ip {rChannel,sChannel} nt=:(NetTask eval) state:todo]},world}
    # (mbSelect,chList) = checkSelect i chList
    //Check if disconnected
    | mbSelect =:(Just SR_Disconnected) || mbSelect=:(Just SR_EOM)
 	    # world = closeRChannel rChannel world
        # world = closeChannel sChannel world
        = process (i+1) chList {iworld & loop={done=done,todo=todo},world=world}
    //Read data
    # (data,rChannel,world) = case mbSelect of
        Just SR_Available
		    # (data,rChannel,world) = receive rChannel world
            = (Just (toString data),rChannel,world)
        _
            = (Nothing,rChannel,world)
    //Eval main loop task
    # (out,close,state,iworld=:{loop={todo,done},world})
        = eval data state {iworld & loop={done=done,todo=todo},world=world}
    //Send data if produced
    # (sChannel,world) = case out of
        Just data   = send (toByteSeq data) sChannel world
        _           = (sChannel,world)
    | close
        //Close connection
 		# world = closeRChannel rChannel world
        # world = closeChannel sChannel world
        = process (i+1) chList {iworld & loop={done=done,todo=todo},world=world}
    = process (i+1) chList {iworld & loop={done=[ConnectionInstance ip {rChannel=rChannel,sChannel=sChannel} nt state:done],todo=todo},world=world}
process i chList iworld=:{loop={done,todo=[BackgroundInstance bt=:(BackgroundTask eval):todo]}}
    # iworld=:{loop={done,todo}} = eval {iworld & loop = {done=done,todo=todo}}
    = process (i+1) chList {iworld & loop={done=[BackgroundInstance bt:done],todo=todo}}

checkSelect :: !Int ![(!Int,!SelectResult)] -> (!Maybe SelectResult,![(!Int,!SelectResult)])
checkSelect i chList =:[(who,what):ws] | (i == who) = (Just what,ws)
checkSelect i chList = (Nothing,chList)

halt :: !*IWorld -> !*IWorld
halt iworld=:{loop={todo=[],done}} = iworld
halt iworld=:{loop={todo=[ListenerInstance _ listener _:todo],done},world}
 	# world = closeRChannel listener world
    = halt {iworld & loop = {todo=todo,done=done}}
halt iworld=:{loop={todo=[ConnectionInstance _ {rChannel,sChannel} _ _:todo],done},world}
 	# world = closeRChannel rChannel world
    # world = closeChannel sChannel world
    = halt {iworld & loop = {todo=todo,done=done}}
halt iworld=:{loop={todo=[BackgroundInstance _ :todo],done},world}
    = halt {iworld & loop = {todo=todo,done=done}}

httpService :: !Int !Int ![(!String -> Bool
				,!Bool
				,!(HTTPRequest *IWorld -> (!HTTPResponse,!Maybe ConnectionType, !*IWorld))
				,!(HTTPRequest (Maybe {#Char}) ConnectionType *IWorld -> (!Maybe {#Char}, !Bool, !ConnectionType, !*IWorld))
				,!(HTTPRequest ConnectionType *IWorld -> *IWorld)
				)] -> NetTask
httpService port keepAliveTime requestProcessHandlers = NetTask eval
where
	eval mbData connState=:(NTProcessingRequest request localState) env
		//Select handler based on request path
		= case selectHandler request requestProcessHandlers of
			Just (_,_,_,handler,_)
				# (mbData,done,localState,env=:{IWorld|world}) = handler request mbData localState env
				| done && isKeepAlive request	//Don't close the connection if we are done, but keepalive is enabled
					# (now,world)       = time world
					= (mbData, False, NTIdle request.client_name now,{IWorld|env & world = world})
				| otherwise
					= (mbData,done,NTProcessingRequest request localState,{IWorld|env & world = world})
			Nothing
				= (Just "HTTP/1.1 400 Bad Request\r\n\r\n", True, connState, env)

	eval (Just data) connState env  //(connState is either Idle or ReadingRequest)
		# rstate = case connState of
			(NTIdle client_name _)
				//Add new data to the request
				# request   = {newHTTPRequest & client_name = client_name, server_port = port}
				# (request, method_done, headers_done, data_done, error) = http_addRequestData request False False False data
				= {NTHttpReqState|request=request,method_done=method_done,headers_done=headers_done,data_done=data_done,error=error}
			(NTReadingRequest {NTHttpReqState|request, method_done, headers_done, data_done})
				//Add new data to the request
				# (request, method_done, headers_done, data_done, error) = http_addRequestData request method_done headers_done data_done (toString data)
				= {NTHttpReqState|request=request,method_done=method_done,headers_done=headers_done,data_done=data_done,error=error}
			_
				= {NTHttpReqState|request=newHTTPRequest,method_done=False,headers_done=False,data_done=False,error=True}
		| rstate.NTHttpReqState.error
			//Sent bad request response and disconnect
			= (Just "HTTP/1.1 400 Bad Request\r\n\r\n", True, connState, env)
		| not rstate.NTHttpReqState.headers_done
			//Without headers we can't select our handler functions yet
			= (Nothing, False, NTReadingRequest rstate, env)
		//Determine the handler
		= case selectHandler rstate.NTHttpReqState.request requestProcessHandlers of
			Nothing
				= (Just "HTTP/1.1 404 Not Found\r\n\r\n", True, connState, env)
			Just (_,completeRequest,newReqHandler,procReqHandler,_)
				//Process a completed request, or as soon as the headers are done if the handler indicates so
				| rstate.NTHttpReqState.data_done || (not completeRequest)
					# request	= if completeRequest (http_parseArguments rstate.NTHttpReqState.request) rstate.NTHttpReqState.request
					//Determine if a  persistent connection was requested
					# keepalive	= isKeepAlive request
					// Create a response
					# (response,mbLocalState,env)	= newReqHandler request env
					//Add keep alive header if necessary
					# response	= if keepalive {response & rsp_headers = [("Connection","Keep-Alive"):response.rsp_headers]} response
					// Encode the response to the HTTP protocol format
					= case mbLocalState of
						Nothing	
							# reply		= encodeResponse True response
							| keepalive
                                # env=:{IWorld|world} = env
								# (now,world)       = time world
								= (Just reply, False, NTIdle rstate.NTHttpReqState.request.client_name now, {IWorld|env & world=world})
							| otherwise
								= (Just reply, True, connState, env)
						Just localState	
							= (Just (encodeResponse False response), False, NTProcessingRequest request localState, env)
				| otherwise
					= (Nothing, False, NTReadingRequest rstate, env)		

	//Close idle connections if the keepalive time has passed
	eval Nothing connState=:(NTIdle ip (Timestamp t)) iworld=:{IWorld|world}
		# (Timestamp now,world)	= time world//TODO: Do we really need to do this for every connection all the time?
		= (Nothing, now >= t + keepAliveTime, connState, {IWorld|iworld & world = world})

	//Do nothing if no data arrives for now
	eval Nothing connState env = (Nothing,False,connState,env)

	//If we were processing a request and were interupted we need to
	//select the appropriate handler to wrap up
	handleConnectionLost (NTProcessingRequest request loc) env
		= case selectHandler request requestProcessHandlers of
			Nothing	= env
			Just (_,_,_,_,connLostHandler) = connLostHandler request loc env

	handleConnectionLost _ env = env

	selectHandler req [] = Nothing
	selectHandler req [h=:(pred,_,_,_,_):hs]
		| pred req.req_path	= Just h
							= selectHandler req hs

	isKeepAlive request = maybe (request.req_version == "HTTP/1.1") (\h -> (toLowerCase h == "keep-alive")) (get "Connection" request.req_headers)

    encodeResponse autoContentLength response=:{rsp_headers, rsp_data}
	    # rsp_headers = addDefault rsp_headers "Server" "iTasks HTTP Server"
	    # rsp_headers = addDefault rsp_headers "Content-Type" "text/html"
	    # rsp_headers = if autoContentLength
	    					(addDefault rsp_headers "Content-Length" (toString (size rsp_data)))
	    					rsp_headers
	    = toString {response & rsp_headers = rsp_headers}
    where		
    	addDefault headers hdr val = if ((lookup hdr headers) =: Nothing) [(hdr,val):headers] headers

