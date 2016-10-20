definition module iTasks.API.Extensions.Web
import iTasks
from Internet.HTTP import :: HTTPRequest, :: HTTPResponse
/**
* This module provides support for building web applications.
*/

//* Uniform resource locators
:: URL			= URL !String
instance toString	URL
instance html		URL


//Simple web server task
serveWebService :: Int (HTTPRequest -> Task HTTPResponse) -> Task ()

//Task for serving a static file
serveFile :: [FilePath] HTTPRequest -> Task HTTPResponse
