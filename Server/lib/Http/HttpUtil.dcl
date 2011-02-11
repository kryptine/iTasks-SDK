definition module HttpUtil

import HTTP, HttpServer

//General utility functions
http_splitMultiPart :: !String !String -> [([(String,String)], String)]

//Incremental construction of a request
http_addRequestData :: !HTTPRequest !Bool !Bool !Bool !String -> (HTTPRequest, Bool, Bool, Bool, Bool)


//Parsing of HTTP Request messages
http_parseRequestLine :: !String -> (!String, !String, !String, !String, !Bool)
http_parseHeader :: !String -> (!(String,String), !Bool)

http_parseArguments :: !HTTPRequest -> HTTPRequest
http_parseGetArguments :: !HTTPRequest -> Map String String
http_parsePostArguments :: !HTTPRequest -> Map String String
http_parseUrlEncodedArguments :: !String -> [(String,String)]
http_parseMultiPartPostArguments :: !HTTPRequest -> (Map String String, Map String HTTPUpload) 

//Construction of HTTP Response messages
http_makeResponse :: !HTTPRequest [((String -> Bool),(HTTPRequest *World -> (HTTPResponse, HTTPServerControl, *World)))] !Bool !*World -> (!HTTPResponse,!HTTPServerControl,!*World)
http_encodeResponse :: !HTTPResponse !Bool !*World -> (!String,!*World)

//Error responses
http_notfoundResponse :: !HTTPRequest !*World -> (!HTTPResponse, !HTTPServerControl, !*World)
http_forbiddenResponse :: !HTTPRequest !*World -> (!HTTPResponse, !HTTPServerControl, !*World)

//Static content
http_staticResponse :: !HTTPRequest !*World -> (!HTTPResponse, !HTTPServerControl, !*World)
http_staticFileContent :: !String !*World -> (!Bool, !String, !*World)
http_staticFileMimeType :: !String !*World -> (!String, !*World)

