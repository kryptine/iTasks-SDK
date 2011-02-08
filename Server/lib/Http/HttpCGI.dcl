definition module HttpCGI

import HTTP
from HttpServer import :: HTTPServerOption, :: HTTPServerControl

:: HTTPCGIOption 	= HTTPCGIOptStaticFallback Bool // If all request handlers fail, should the static file handler be tried (default False)
					| HTTPCGIOptParseArguments Bool	// Should the query and body of the request be parsed (default True)


http_startCGI :: [HTTPCGIOption] [((String -> Bool),(HTTPRequest *World-> (HTTPResponse,HTTPServerControl,*World)))] *World -> *World