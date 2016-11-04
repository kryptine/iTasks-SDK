definition module iTasks.API.Extensions.Web
import iTasks
from Internet.HTTP import :: HTTPRequest, :: HTTPResponse
/**
* This module provides support for building web applications.
*/

//* Uniform resource locators
:: URL = URL !String
instance toString	URL
instance html		URL

derive gEditor    URL
derive gText      URL
derive JSONEncode URL
derive JSONDecode URL
derive gDefault	  URL
derive gEq        URL

//Simple web server task
serveWebService :: Int (HTTPRequest -> Task HTTPResponse) -> Task ()

//Task for serving a static file
serveFile :: [FilePath] HTTPRequest -> Task HTTPResponse
