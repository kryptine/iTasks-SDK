implementation module HttpServer

import Http, HttpUtil, HttpTextUtil
import StdList, StdTuple, StdArray, StdFile, StdBool, StdMisc
import StdTCP

//Start the HTTP server
http_startServer :: [HTTPServerOption] [((String -> Bool),(HTTPRequest *World-> (HTTPResponse,*World)))] *World -> *World
http_startServer options handlers world
	//Start the listener
	# (listener,world) = startListener (getPortOption options) world
	//Enter the endless loop
	= loop options handlers listener [] [] [] world

// Try to open a listener on the given port
startListener :: Int !*World -> (TCP_Listener,!*World)
startListener port world
	# (success, mbListener, world) = openTCP_Listener port world
	| success	= (fromJust mbListener,world)
	| otherwise = abort ("Error: The server port " +++ (toString port) +++ " is currently occupied!\n" +++
						 "Probably a previous application is still running and you have forgotten to close it.\n" +++
						 "It is also possible that another web server running on your machine is using this port.\n\n\n")

//Main event loop, it is called each time a client connects or data arrives
loop ::	[HTTPServerOption]
		[((String -> Bool),(HTTPRequest *World-> (HTTPResponse,*World)))]
		TCP_Listener [TCP_RChannel] [TCP_SChannel]
		[(HTTPRequest,Bool,Bool,Bool)]
		*World -> *World
loop options handlers listener rchannels schannels requests world
	//Join the listener with the open channels
	# glue = (TCP_Listeners [listener]) :^: (TCP_RChannels rchannels)
	//Select the channel which has data available
	# ([(who,what):_],glue,_,world) = selectChannel_MT Nothing glue Void world
	//Split the listener from the open channels
	# ((TCP_Listeners [listener:_]) :^: (TCP_RChannels rchannels)) = glue
	//A new client attempts to connect
	| who == 0
		# (tReport, mbNewMember, listener, world)	= receive_MT (Just 0) listener world
		| tReport <> TR_Success						= loop options handlers listener rchannels schannels requests world //Just continue
		# (ip,{sChannel,rChannel})					= fromJust mbNewMember
		# request									= {http_emptyRequest & client_name = toString ip, server_port = getPortOption options}
		= loop options handlers listener [rChannel:rchannels] [sChannel:schannels] [(request,False,False,False):requests] world		
	//A client has new data
	| otherwise
		// Select the offset without the listener
		# who = who - 1
		// Select the right read channel from the list
		# (currentrchannel, rchannels) = selectFromList who rchannels
		// Select the right write channel from the list
		# (currentschannel, schannels) = selectFromList who schannels
		// Select the right incomplete request from the list
		# ((request, method_done, headers_done, data_done), requests) = selectFromList who requests
		
		// New data is available
		| what == SR_Available
			// Fetch the new data from the receive channel
			# (data,currentrchannel,world) = receive currentrchannel world
		
			//Add new data to the request
			# (request, method_done, headers_done, data_done, error) = addRequestData request method_done headers_done data_done (toString data)
			| error
				//Sent bad request response and disconnect
				# (currentschannel,world) = send (toByteSeq "HTTP/1.0 400 Bad Request\r\n\r\n") currentschannel world
				# world = closeRChannel currentrchannel world
				# world = closeChannel currentschannel world
				= loop options handlers listener rchannels schannels requests world
		
			//Process a completed request
			| method_done && headers_done && data_done
				#  request			= if (getParseOption options) (http_parseArguments request) request
				// Create a response
				# (response,world)	= makeResponse options request handlers world
				// Encode the response to the HTTP protocol format
				# (reply, world) = http_encodeResponse response True world
				// Send the encoded response to the client
				# (currentschannel,world) = send (toByteSeq reply) currentschannel world
				# world = closeRChannel currentrchannel world
				# world = closeChannel currentschannel world
				= loop options handlers listener rchannels schannels requests world		
		
			//We do not have everything we need yet, so continue
			| otherwise = loop options handlers listener [currentrchannel:rchannels] [currentschannel:schannels] [(request,method_done, headers_done, data_done):requests] world
			
		//We lost the connection
		| otherwise
			# world = closeRChannel currentrchannel world
			# world = closeChannel currentschannel world
			= loop options handlers listener rchannels schannels requests world
where
	selectFromList nr list
		# (left,[element:right]) = splitAt nr list
		= (element,left++right)
	
//Add new data to a request
addRequestData :: !HTTPRequest !Bool !Bool !Bool !String -> (HTTPRequest, Bool, Bool, Bool, Bool)
addRequestData req requestline_done headers_done data_done data
	# req	= {req & req_data = req.req_data +++ data}	//Add the new data
	//Parsing of the request line
	| not requestline_done
		# index = text_indexOf "\r\n" req.req_data
		| index == -1	= (req,False,False,False,False)	//The first line is not complete yet
		| otherwise
			# (method,path,query,version,error) = http_parseRequestLine (req.req_data % (0, index - 1))
			| error	= (req,False,False,False,True)		//We failed to parse the request line
			# req = {req & req_method = method, req_path = path, req_query = query, req_version = version, req_data = req.req_data % (index + 2, size req.req_data) }
			= addRequestData req True False False ""	//We are done with the request line but still need to inspect the rest of the data
	//Parsing of headers
	| not headers_done
		# index = text_indexOf "\r\n" req.req_data
		| index == -1	= (req,True,False,False,False)	//We do not have a full line yet
		| index == 0									//We have an empty line, this means we have received all the headers
			# req = {req & req_data = req.req_data % (2, size req.req_data)}
			= addRequestData req True True False ""		//Headers are finished, continue with the data part
		| otherwise
			# (header,error) = http_parseHeader (req.req_data % (0, index - 1))
			| error = (req,True,False,False,True)		//We failed to parse the header
			# req = {req & req_headers = [header:req.req_headers], req_data = req.req_data % (index + 2, size req.req_data)}
			= addRequestData req True False False ""	//We continue to look for more headers
	//Addition of data
	| not data_done
		# datalength	= toInt (http_getValue "Content-Length" req.req_headers "0")
		| (size req.req_data) < datalength	= (req,True,True,False,False)	//We still need more data
											= (req,True,True,True,False)	//We have all data and are done
	//Data is added while we were already done
	= (req,True,True,True,False) 
	
// Calls the request handler for a request and returns the generated response
makeResponse :: [HTTPServerOption] HTTPRequest [((String -> Bool),(HTTPRequest *World -> (HTTPResponse, *World)))] *World -> (HTTPResponse, *World)
makeResponse options request [] world //None of the request handlers matched
	= if (getStaticOption options)
		(http_staticResponse request world) (http_notfoundResponse request world)
makeResponse options request [(pred,handler):rest] world
	| (pred request.req_path)		= handler request world						//Apply virtual page function
									= makeResponse options request rest world	//Search the rest of the list


getPortOption :: [HTTPServerOption] -> Int
getPortOption [] = 80
getPortOption [x:xs] = case x of (HTTPServerOptPort port)	= port
								 _							= getPortOption xs

getStaticOption :: [HTTPServerOption] -> Bool
getStaticOption [] = False
getStaticOption [x:xs] = case x of (HTTPServerOptStaticFallback b) = b
								   _							   = getStaticOption xs

getParseOption :: [HTTPServerOption] -> Bool
getParseOption [] = True
getParseOption [x:xs] = case x of (HTTPServerOptParseArguments b)	= b
								  _									= getParseOption xs



	