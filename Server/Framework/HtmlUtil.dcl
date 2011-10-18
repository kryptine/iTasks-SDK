definition module HtmlUtil
/**
* This module provides rudimentary utilities for generating simple html pages.
*
*/
import HTML, JSON, HTTP

/**
* Defines the simple inline CSS stylesheet
*/
embeddedStyle :: HtmlTag 
/**
* Creates the basic layout for a page with a title, description and body
*/
pageLayout :: !String !String ![HtmlTag] -> HtmlTag

/**
* Creates a simple page for accessing a service
*/
servicePage :: !String !String !String ![(String,String,Bool)] !JSONNode -> HtmlTag

/**
* Creates an HTTP response of a service page
*/
serviceResponse :: !Bool !String !String !String ![(String,String,Bool)] !JSONNode -> HTTPResponse

/**
* Creates a static page with information about the service API.
*/
overviewPage :: HtmlTag

/**
* Creates an HTTP response of a the overview page
*/
overviewResponse :: HTTPResponse

/**
* Creates a start page to load the client framework
*/
appStartPage :: !String -> HtmlTag
/**
* Creates an HTTP response of the start page
*/
appStartResponse :: !String -> HTTPResponse

/**
* Creates a 302 Redirect response
*/
redirectResponse :: !String -> HTTPResponse
/**
* Creates a 404 Not found error page
*/
notFoundPage :: !HTTPRequest -> HtmlTag
/**
* Creates an HTTP 404 response
*/
notFoundResponse :: !HTTPRequest -> HTTPResponse
/**
* Gets the value of a parameter in the GET or POST values.
*/
paramValue :: !String !HTTPRequest -> String
/**
* Convert newlines to br tags.
*/
nl2br :: !String -> HtmlTag
/**
* Convert a html string to pure text.
*/
html2text :: !String -> String
