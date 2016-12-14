implementation module iTasks._Framework.WebService

import StdList, StdBool, StdTuple, StdArray, StdFile
from StdFunc import o, const
import Data.Maybe, Data.Functor
from Data.Map import :: Map (..)
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Queue as DQ
import qualified iTasks._Framework.SDS as SDS

import System.Time, Text, Text.JSON, Internet.HTTP, Data.Error
import System.File, System.FilePath, System.Directory
import iTasks._Framework.Task, iTasks._Framework.TaskState, iTasks._Framework.TaskEval, iTasks._Framework.TaskStore
import iTasks.UI.Definition, iTasks._Framework.Util, iTasks._Framework.HtmlUtil, iTasks._Framework.Engine, iTasks._Framework.IWorld
import iTasks.API.Core.SDSs, iTasks.API.Common.SDSCombinators
import iTasks.API.Core.Types
import Crypto.Hash.SHA1, Text.Encodings.Base64, Text.Encodings.MIME
import Text.HTML

from iTasks._Framework.HttpUtil import http_addRequestData, http_parseArguments

:: NetTaskState
    = NTIdle String Timestamp
    | NTReadingRequest HttpReqState
	| NTProcessingRequest HTTPRequest ConnectionState

:: HttpReqState =
    { request       :: HTTPRequest
    , method_done   :: Bool
    , headers_done  :: Bool
    , data_done     :: Bool
    , error         :: Bool
    }

//Opcodes used in websocket frames:
WS_OP_CONTINUE :== 0x00
WS_OP_TEXT     :== 0x01 
WS_OP_BINARY   :== 0x02
WS_OP_CLOSE    :== 0x08
WS_OP_PING     :== 0x09
WS_OP_PONG     :== 0x0A

wsockHandShake :: String -> String
wsockHandShake key = base64Encode digest
where
	(SHA1Digest digest) = sha1StringDigest (key+++"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

wsockAddData :: WebSockState {#Char} -> (!WebSockState,![WebSockEvent])
wsockAddData state data //Read as many frames as possible
	# (state,evCur) = wsockReadFrame state data
	| evCur =:[] = (state,[])
	# (state,evNext) = wsockAddData state ""
	= (state,evCur++evNext)
wsockReadFrame state=:{WebSockState|cur_frame,message_data} data
	# cur_frame = cur_frame +++ data
	| size cur_frame < 6 = ({state & cur_frame = cur_frame},[]) //No frame header yet
	//Determine values of flags in the first two header bytes
	# final = (fromChar cur_frame.[0] bitand 0x80) > 0
	# opcode = (fromChar cur_frame.[0] bitand 0x0F)
	# masked = (fromChar cur_frame.[1] bitand 0x80) > 0
	//Determine payload length (and how many bytes were used to encode it)
	# (payload_length,ext_payload_length_size) = payloadLen cur_frame
	//Determine the total expected frame length
	# frame_length = 2 + ext_payload_length_size + if masked 4 0 + payload_length 
	//If no full frame is read yet we can stop for now 
	| size cur_frame < frame_length 
		= ({state & cur_frame = cur_frame},[])
	//Extract the payload
	# payload = if masked
		(let mask = cur_frame % (2 + ext_payload_length_size, 5 + ext_payload_length_size) in
			decodePayload mask (cur_frame % (6 + ext_payload_length_size, 6 + ext_payload_length_size + payload_length - 1))
		)
		(cur_frame % (2 + ext_payload_length_size, payload_length))
	//Remove the frame data
	# state = if (size cur_frame == frame_length)
					 {state & cur_frame = ""}
					 {state & cur_frame = cur_frame % (frame_length,size cur_frame)}
	//Process frame	
	| opcode == WS_OP_CLOSE
		= (state,[WSClose payload])
	| opcode == WS_OP_PING
		= (state,[WSPing payload])
	| opcode == WS_OP_TEXT 
		| final = ({state & message_data = []},[WSTextMessage payload])
		| otherwise = ({state & message_text = True, message_data = [payload]},[])
	| opcode == WS_OP_BINARY
		| final = ({state & message_data = []},[WSBinMessage payload])
		| otherwise = ({state & message_text = False, message_data = [payload]},[])
	| opcode == WS_OP_CONTINUE
		| final = ({state & message_data = []},[(if state.message_text WSTextMessage WSBinMessage) (concat (reverse [payload:state.message_data]))])
		| otherwise = ({state & message_data = [payload:state.message_data]},[])
	| otherwise	
		= (state,[])
where
	payloadLen data
		# len = fromChar data.[1] bitand 0x7F //First determine if length fits in first 7 available bits
		| len == 126 //Use byte 2 & 3 as length (16-bit)
			= ((fromChar data.[2] << 8) + (fromChar data.[3]), 2)
		| len == 127 //Use byte 2,3,4,5,6,7,8,9 as length (64-bit)
			= (foldr (+) 0 [fromChar data.[b] << (i * 8) \\ b <- [2..9] & i <- [0..7]], 8)
		| otherwise = (len,0)

	decodePayload mask encoded =  {decode i c \\ c <-: encoded & i <- [0..]}
	where
		decode i c = toChar ((fromChar mask.[i rem 4]) bitxor (fromChar c))

wsockControlFrame :: !Int !String -> String
wsockControlFrame opcode payload = wsockMsgFrame opcode True payload

wsockCloseMsg :: String -> String
wsockCloseMsg payload = wsockControlFrame WS_OP_CLOSE payload 

wsockPongMsg :: String -> String
wsockPongMsg payload = wsockControlFrame WS_OP_PONG payload

wsockMsgFrame :: !Int !Bool !String -> String
wsockMsgFrame opcode final payload 
	| num_bytes < 125   = frame num_bytes "" payload
	| num_bytes < 65536 = frame 126 {toChar (num_bytes >> (8*i)) \\ i <- [1,0]} payload
	| otherwise         = IF_INT_64_OR_32
							(frame 127 {toChar (num_bytes >> (8*i)) \\ i <- [7,6,5,4,3,2,1,0]} payload)
							(frame 127 {toChar (if (i < 4) (num_bytes >> (8*i)) 0) \\ i <- [7,6,5,4,3,2,1,0]} payload)
where
	num_bytes = size payload
	frame payload_length ext_payload_length payload
		= {toChar (opcode bitor (if final 0x80 0x00)),toChar payload_length} +++ ext_payload_length +++ payload
	
wsockTextMsg :: String -> [String]
wsockTextMsg payload = [wsockMsgFrame WS_OP_TEXT True payload]

httpServer :: !Int !Int ![(!String -> Bool
				,!Bool
				,!(HTTPRequest r *IWorld -> (!HTTPResponse,!Maybe ConnectionState, !Maybe w, !*IWorld))
				,!(HTTPRequest r (Maybe {#Char}) ConnectionState *IWorld -> (![{#Char}], !Bool, !ConnectionState, !Maybe w, !*IWorld))
				,!(HTTPRequest r ConnectionState *IWorld -> (!Maybe w, !*IWorld))
				)] (RWShared () r w) -> ConnectionTask | TC r & TC w
httpServer port keepAliveTime requestProcessHandlers sds
 = wrapIWorldConnectionTask {ConnectionHandlersIWorld|onConnect=onConnect, whileConnected=whileConnected, onDisconnect=onDisconnect} sds
where
    onConnect host r iworld=:{IWorld|world,clocks}
        = (Ok (NTIdle host clocks.timestamp),Nothing,[],False,{IWorld|iworld & world = world})

	whileConnected mbData connState=:(NTProcessingRequest request localState) r env
		//Select handler based on request path
		= case selectHandler request requestProcessHandlers of
			Just (_,_,_,handler,_)
				# (mbData,done,localState,mbW,env=:{IWorld|world,clocks}) = handler request r mbData localState env
				| done && isKeepAlive request	//Don't close the connection if we are done, but keepalive is enabled
					= (Ok (NTIdle request.client_name clocks.timestamp), mbW, mbData, False,{IWorld|env & world = world})
				| otherwise
					= (Ok (NTProcessingRequest request localState), mbW, mbData,done,{IWorld|env & world = world})
			Nothing
				= (Ok connState, Nothing, ["HTTP/1.1 400 Bad Request\r\n\r\n"], True, env)

	whileConnected (Just data) connState r iworld=:{IWorld|clocks}//(connState is either Idle or ReadingRequest)
		# rstate = case connState of
			(NTIdle client_name _)
				//Add new data to the request
				# request   = {newHTTPRequest & client_name = client_name, server_port = port}
				# (request, method_done, headers_done, data_done, error) = http_addRequestData request False False False data
				= {HttpReqState|request=request,method_done=method_done,headers_done=headers_done,data_done=data_done,error=error}
			(NTReadingRequest {HttpReqState|request, method_done, headers_done, data_done})
				//Add new data to the request
				# (request, method_done, headers_done, data_done, error) = http_addRequestData request method_done headers_done data_done (toString data)
				= {HttpReqState|request=request,method_done=method_done,headers_done=headers_done,data_done=data_done,error=error}
			_
				= {HttpReqState|request=newHTTPRequest,method_done=False,headers_done=False,data_done=False,error=True}
		| rstate.HttpReqState.error
			//Sent bad request response and disconnect
			= (Ok connState, Nothing, ["HTTP/1.1 400 Bad Request\r\n\r\n"], True, iworld)
		| not rstate.HttpReqState.headers_done
			//Without headers we can't select our handler functions yet
			= (Ok (NTReadingRequest rstate), Nothing, [], False, iworld)
		//Determine the handler
		= case selectHandler rstate.HttpReqState.request requestProcessHandlers of
			Nothing
				= (Ok connState, Nothing, ["HTTP/1.1 404 Not Found\r\n\r\n"], True, iworld)
			Just (_,completeRequest,newReqHandler,procReqHandler,_)
				//Process a completed request, or as soon as the headers are done if the handler indicates so
				| rstate.HttpReqState.data_done || (not completeRequest)
					# request	= if completeRequest (http_parseArguments rstate.HttpReqState.request) rstate.HttpReqState.request
					//Determine if a  persistent connection was requested
					# keepalive	= isKeepAlive request
					// Create a response
					# (response,mbLocalState,mbW,iworld)	= newReqHandler request r iworld 
					//Add keep alive header if necessary
					# response	= if keepalive {HTTPResponse|response & rsp_headers = [("Connection","Keep-Alive"):response.HTTPResponse.rsp_headers]} response
					// Encode the response to the HTTP protocol format
					= case mbLocalState of
						Nothing	
							# reply		= encodeResponse True response
							| keepalive
								= (Ok (NTIdle rstate.HttpReqState.request.client_name clocks.timestamp), mbW, [reply], False, iworld)
							| otherwise
								= (Ok connState, mbW, [reply], True, iworld)
						Just localState	
							= (Ok (NTProcessingRequest request localState), mbW, [(encodeResponse False response)], False, iworld)
				| otherwise
					= (Ok (NTReadingRequest rstate), Nothing, [], False, iworld)		

	//Close idle connections if the keepalive time has passed
	whileConnected Nothing connState=:(NTIdle ip (Timestamp t)) r iworld=:{IWorld|clocks={timestamp=Timestamp now}}
		= (Ok connState, Nothing, [], now >= t + keepAliveTime, iworld)

	//Do nothing if no data arrives for now
	whileConnected Nothing connState r iworld = (Ok connState,Nothing,[],False,iworld)

	//If we were processing a request and were interupted we need to
	//select the appropriate handler to wrap up

    onDisconnect connState=:(NTProcessingRequest request localState) r env
		= case selectHandler request requestProcessHandlers of
			Nothing	                        = (Ok connState, Nothing, env)
			Just (_,_,_,_,connLostHandler)  
				# (mbW, env) = connLostHandler request r localState env
				= (Ok connState, mbW, env)
    onDisconnect connState r env = (Ok connState, Nothing, env)

	selectHandler req [] = Nothing
	selectHandler req [h=:(pred,_,_,_,_):hs]
		| pred req.HTTPRequest.req_path	= Just h
										= selectHandler req hs

	isKeepAlive request = maybe (request.HTTPRequest.req_version == "HTTP/1.1") (\h -> (toLowerCase h == "keep-alive")) ('DM'.get "Connection" request.HTTPRequest.req_headers)

    encodeResponse autoContentLength response=:{HTTPResponse|rsp_headers, rsp_data}
	    # rsp_headers = addDefault rsp_headers "Server" "iTasks HTTP Server"
	    # rsp_headers = addDefault rsp_headers "Content-Type" "text/html"
	    # rsp_headers = if autoContentLength
	    					(addDefault rsp_headers "Content-Length" (toString (size rsp_data)))
	    					rsp_headers
	    = toString {HTTPResponse|response & rsp_headers = rsp_headers}
    where		
    	addDefault headers hdr val = if (('DL'.lookup hdr headers) =: Nothing) [(hdr,val):headers] headers

:: ChangeQueues :== Map InstanceNo (Queue UIChange)


taskUIService :: ![PublishedTask] ->
                 (!(String -> Bool)
                 ,!Bool
                 ,!(HTTPRequest ChangeQueues *IWorld -> (!HTTPResponse,!Maybe ConnectionState, !Maybe ChangeQueues, !*IWorld))
                 ,!(HTTPRequest ChangeQueues (Maybe {#Char}) ConnectionState *IWorld -> (![{#Char}], !Bool, !ConnectionState, !Maybe ChangeQueues, !*IWorld))
                 ,!(HTTPRequest ChangeQueues ConnectionState *IWorld -> (!Maybe ChangeQueues, !*IWorld))
                 ) 
taskUIService taskUrls = (matchFun [url \\ {PublishedTask|url} <-taskUrls],True,reqFun` taskUrls,dataFun,disconnectFun)
where
    matchFun :: [String] String -> Bool
    matchFun matchUrls reqUrl = or [reqUrl == uiUrl matchUrl \\ matchUrl <- matchUrls]

	reqFun` taskUrls req output iworld=:{server}
		# server_url = "//" +++ req.server_name +++ ":" +++ toString req.server_port
		= reqFun taskUrls req output {IWorld|iworld & server = {server & serverURL = server_url}}

	reqFun taskUrls req output iworld=:{IWorld|server={serverName},config}
		//Check for WebSocket upgrade headers
        | ('DM'.get "Upgrade" req.HTTPRequest.req_headers) =:(Just "websocket") && isJust ('DM'.get "Sec-WebSocket-Key" req.HTTPRequest.req_headers)
            # secWebSocketKey       = trim (fromJust ('DM'.get "Sec-WebSocket-Key" req.HTTPRequest.req_headers))
            # secWebSocketAccept    = wsockHandShake secWebSocketKey
            //Create handshake response
            # headers = [("Upgrade","websocket"), ("Connection","Upgrade")
                        ,("Sec-WebSocket-Accept",secWebSocketAccept)]
			# state = {WebSockState|cur_frame = "",message_text = True, message_data = []}
            = ({newHTTPResponse 101 "Switching Protocols" & rsp_headers = headers, rsp_data = ""}
			  , Just (state,[]),Nothing,iworld)
        | otherwise
			= (errorResponse "Requested service format not available for this task", Nothing, Nothing, iworld)

	dataFun req output (Just data) (state,instances) iworld
		# (state,result) = wsockAddData state data 
		= case result of //TODO: Process multiple events
			[WSClose msg:_]
				//Respond with a close frame and close the connection
				= ([wsockCloseMsg msg],True, (state,instances),Nothing,iworld)
			[WSTextMessage msg:_] 
				//Process events:
				= case fromString msg of
					//- new session
					(JSONArray [JSONInt reqId,JSONString "new"])
                    	= case createTaskInstance` req taskUrls iworld of
							(Error (_,err), iworld)
								# json = JSONArray [JSONInt reqId,JSONString "ERROR",JSONString err]
								= (wsockTextMsg (toString json),False, (state,instances),Nothing,iworld)
                        	(Ok (instanceNo,instanceKey),iworld)
								# iworld = queueEvent instanceNo ResetEvent iworld //Queue a Reset event to make sure we start with a fresh GUI
								# json = JSONArray [JSONInt reqId, JSONObject [("instanceNo",JSONInt instanceNo),("instanceKey",JSONString instanceKey)]]
								= (wsockTextMsg (toString json),False, (state,instances),Nothing,iworld)
					//- attach existing instance
					(JSONArray [JSONString "attach",JSONInt instanceNo,JSONString instanceKey])
						//TODO: Clear output
						# iworld = queueEvent instanceNo ResetEvent iworld //Queue a Reset event to make sure we start with a fresh GUI
						= ([],False, (state,[instanceNo:instances]),Nothing,iworld) //TODO: Maybe send confirmation message?
					//- detach instance
					(JSONArray [JSONString "detach",JSONInt instanceNo,JSONString instanceKey])
						= ([],False, (state,removeMember instanceNo instances),Nothing,iworld) //TODO: Maybe send confirmation message?
					(JSONArray [JSONString "event",JSONInt instanceNo,JSONArray [JSONString taskId,JSONNull,JSONString actionId]]) //Action event
						# iworld = queueEvent instanceNo (ActionEvent (fromString taskId) actionId) iworld //Queue event
						= ([],False, (state,instances),Nothing,iworld) //TODO: Maybe send confirmation message?
					(JSONArray [JSONString "event",JSONInt instanceNo,JSONArray [JSONString taskId,JSONString name,value]]) //Edit event
						# iworld = queueEvent instanceNo (EditEvent (fromString taskId) name value) iworld //Queue event
						= ([],False, (state,instances),Nothing,iworld) //TODO: Maybe send confirmation message?
					//Unknown message 
					e
						# json = JSONArray [JSONString "ERROR",JSONString "Unknown event"]
						= (wsockTextMsg (toString json),False, (state,instances),Nothing,iworld)
			[WSPing msg:_]
				= ([wsockPongMsg msg],False,(state,instances),Nothing,iworld)
			_ = ([],False,(state,instances),Nothing,iworld)
	
    dataFun req output Nothing (state,instances) iworld
		//Check for UI updates for all attached instances
		# (changes, output) = dequeueOutput instances output
		= case changes of //Ignore empty updates
			[] = ([],False,(state,instances),Nothing,iworld)
			changes	
                # (_,iworld) = updateInstanceLastIO instances iworld
				# msgs = [wsockTextMsg (toString (JSONObject [("instance",JSONInt instanceNo)
															 ,("change",encodeUIChange change)])) \\ (instanceNo,change) <- changes]
				= (flatten msgs,False, (state,instances),Just output,iworld)

	disconnectFun _ _ (state,instances) iworld = (Nothing, snd (updateInstanceDisconnect instances iworld))
	disconnectFun _ _ _ iworld                 = (Nothing, iworld)

	createTaskInstance` req [{PublishedTask|url,task=TaskWrapper task}:taskUrls] iworld
		| req.HTTPRequest.req_path == uiUrl url = createTaskInstance (task req) iworld
		| otherwise = createTaskInstance` req taskUrls iworld

	uiUrl matchUrl = (if (endsWith "/" matchUrl) matchUrl (matchUrl +++ "/")) +++ "gui-wsock"

	dequeueOutput :: ![InstanceNo] !(Map InstanceNo (Queue UIChange)) -> (![(!InstanceNo,!UIChange)],!Map InstanceNo (Queue UIChange))
	dequeueOutput [] states = ([],states)
	dequeueOutput [i:is] states
		# (output,states) = dequeueOutput is states
		= case 'DM'.get i states of
			Just out 	= ([(i,c) \\ c <- toList out] ++ output,'DM'.put i 'DQ'.newQueue states)
			Nothing  	= (output,states)
	where
		toList q = case 'DQ'.dequeue q of
			(Nothing,q) 	= []
			(Just x,q) 		= [x:toList q]

		
	eventsResponse messages
		= {okResponse &   rsp_headers = [("Content-Type","text/event-stream"),("Cache-Control","no-cache")]
                        , rsp_data = formatMessageEvents messages}
	
	formatMessageEvents messages = concat (map format messages)
    where
        format (instanceNo,change) = "data: {\"instance\":" +++toString instanceNo+++",\"change\":" +++ toString (encodeUIChange change) +++ "}\n\n"

//TODO: The upload and download mechanism used here is inherently insecure!!!
// A smarter scheme that checks up and downloads, based on the current session/task is needed to prevent
// unauthorized downloading of documents and DDOS uploading.
	
documentService :: (!(String -> Bool),!Bool,!(HTTPRequest r *IWorld -> (HTTPResponse, Maybe loc, Maybe w ,*IWorld))
                        ,!(HTTPRequest r (Maybe {#Char}) loc *IWorld -> (![{#Char}], !Bool, loc, Maybe w ,!*IWorld))
                        ,!(HTTPRequest r loc *IWorld -> (!Maybe w,!*IWorld)))
documentService = (matchFun,True,reqFun,dataFun,lostFun)
where
	matchFun path = case dropWhile ((==)"") (split "/" path) of
		["upload"]          = True  // Upload of documents
		["download",_]      = True  // Download of documents
		_ 					= False

	reqFun req output iworld=:{IWorld|server={serverName},config}
		= case dropWhile ((==)"") (split "/" req.HTTPRequest.req_path) of
			["upload"]
				# uploads = 'DM'.toList req.arg_uploads
				| length uploads == 0
					= (jsonResponse (JSONArray []),Nothing,Nothing,iworld)
				# (documents, iworld) = createDocumentsFromUploads uploads iworld
				= (jsonResponse (toJSON documents),Nothing,Nothing,iworld)
			["download",downloadParam]
				# (mbContent, iworld)	= loadDocumentContent downloadParam iworld
				# (mbMeta, iworld)		= loadDocumentMeta downloadParam iworld
				= case (mbContent,mbMeta) of
					(Just content,Just {Document|name,mime,size})
						# headers	= [("Content-Type", mime),("Content-Length", toString size)
									  ,("Content-Disposition","attachment;filename=\"" +++ name +++ "\"")]
						= ({okResponse & rsp_headers = headers, rsp_data = content},Nothing,Nothing,iworld)
					_
						= (notFoundResponse req,Nothing,Nothing,iworld)
			_
				= (notFoundResponse req,Nothing,Nothing,iworld)

	dataFun _ _ _ s env = ([],True,s,Nothing,env)
	lostFun _ _ s env = (Nothing,env)

createDocumentsFromUploads [] iworld = ([],iworld)
createDocumentsFromUploads [(n,u):us] iworld
	# (mbD,iworld)	= createDocument u.upl_filename u.upl_mimetype u.upl_content iworld
	| isError mbD	= createDocumentsFromUploads us iworld
	# (ds,iworld)	= createDocumentsFromUploads us iworld
	= ([fromOk mbD:ds],iworld)

jsonResponse json
		= {okResponse & rsp_headers = [("Content-Type","text/json"),("Access-Control-Allow-Origin","*")], rsp_data = toString json}
	
// Request handler which serves static resources from the application directory,
// or a system wide default directory if it is not found locally.
// This request handler is used for serving system wide javascript, css, images, etc...
staticResourceService :: [String] -> (!(String -> Bool),!Bool,!(HTTPRequest r *IWorld -> (HTTPResponse, Maybe loc, Maybe w ,*IWorld))
                        ,!(HTTPRequest r (Maybe {#Char}) loc *IWorld -> (![{#Char}], !Bool, loc, Maybe w ,!*IWorld))
                        ,!(HTTPRequest r loc *IWorld -> (!Maybe w,!*IWorld)))
staticResourceService taskPaths = (const True,True,initFun,dataFun,lostFun)
where
	initFun req _ env
		# (rsp,env) = handleStaticResourceRequest req env
		= (rsp,Nothing,Nothing,env)
		
	dataFun _ _ _ s env = ([],True,s,Nothing,env)
	lostFun _ _ s env = (Nothing,env)

	handleStaticResourceRequest :: !HTTPRequest *IWorld -> (!HTTPResponse,!*IWorld)
	handleStaticResourceRequest req iworld=:{IWorld|server={paths={publicWebDirectories}}}
    	= serveStaticResource req publicWebDirectories iworld
	where
    	serveStaticResource req [] iworld
	    	= (notFoundResponse req,iworld)
    	serveStaticResource req [d:ds] iworld=:{IWorld|world}
			# filename		   = if (isMember req.HTTPRequest.req_path taskPaths) //Check if one of the published tasks is requested, then serve bootstrap page
									(d +++ filePath "/index.html")
									(d +++ filePath req.HTTPRequest.req_path)
			# type			   = mimeType filename
            # (mbInfo,world) = getFileInfo filename world
			| case mbInfo of (Ok info) = info.directory ; _ = True
               = serveStaticResource req ds {IWorld|iworld & world = world}
			# (mbContent, world)	= readFile filename world
			= case mbContent of
				(Ok content) = ({ okResponse
	    						& rsp_headers = [("Content-Type", type),("Content-Length", toString (size content))]
                                , rsp_data = content}, {IWorld|iworld & world = world})
                (Error e)    = (errorResponse (toString e +++ " ("+++ filename +++")"), {IWorld|iworld & world = world})

		//Translate a URL path to a filesystem path
		filePath path	= ((replaceSubString "/" {pathSeparator}) o (replaceSubString ".." "")) path
		mimeType path	= extensionToMimeType (takeExtension path)
