definition module iTasks.API.Extensions.Web
import iTasks
from Internet.HTTP import :: HTTPRequest, :: HTTPResponse
/**
* This module provides support for building web services using tasks
*/

//Simple web server task
serveWebService :: Int (HTTPRequest -> Task HTTPResponse) -> Task ()

//Task for serving a static file
serveFile :: [FilePath] HTTPRequest -> Task HTTPResponse
