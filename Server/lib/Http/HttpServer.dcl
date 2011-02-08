definition module HttpServer
// This module provides a simple embedded HTTP server.
// It allows the creation of a single threaded server which
// is very suitable for small scale testing of web applications.
// It is not meant for use in a production environment.
//
// This module is based upon the original Clean HTTP server by Paul de Mast

import HTTP

:: HTTPServerOption		= HTTPServerOptPort Int				// The port on which the server listens (default is 80)
						| HTTPServerOptStaticFallback Bool	// If all request handlers fail, should the static file handler be tried (default False)
						| HTTPServerOptParseArguments Bool	// Should the query and body of the request be parsed (default True)
						| HTTPServerOptDebug Bool			// Should the server write debug info to the stdout

:: HTTPServerControl	= HTTPServerContinue				// Nothing special, just keep the server alive
						| HTTPServerStop					// Stop the server
															// Restart the server with new options and handlers
						| HTTPServerRestart [HTTPServerOption] [(!(String -> Bool),!(HTTPRequest *World -> *(!HTTPResponse,!HTTPServerControl,!*World)))]

// Start the HTTP server
// The first argument is a list of server options
// The second argument is a list of pairs of predicates and request handlers
// The predicate inspects the requested path (eg. /foo), if the predicate is true the corresponding request handler is invoked 
					
http_startServer :: [HTTPServerOption] [(!(String -> Bool),!(HTTPRequest *World-> (!HTTPResponse,!HTTPServerControl,!*World)))] *World -> *World