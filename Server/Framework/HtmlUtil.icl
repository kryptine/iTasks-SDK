implementation module HtmlUtil

import HTML, JSON, Text, HTTP, Map
import StdList, StdBool

embeddedStyle :: HtmlTag
embeddedStyle = StyleTag [TypeAttr "text/css"] [RawText css] 
where
 css = 	"body { background: #fff; font-family: Verdana, Arial, sans-serif; font-size: 12px;} th { text-align: left; } "
	+++ ".field-error em {color: #f00; font-weight: bold} .field-error input {border-color: #f00;} "
	+++ "#main {margin: 20px; background: #d1dded; border: solid 2px #3a81ad; -moz-border-radius: 5px; background: -moz-linear-gradient(bottom,  #d1dded,  #fff);} "
	+++ "#content { padding: 10px; } "
	+++ ".buttons { padding: 5px; background-color: #3a81ad; } "
	+++ ".section { margin: 10px; padding: 5px; overflow: auto;} "
	+++ ".description { margin: 0px 15px 0px 15px; } "
	+++ ".parameters th, { width: 150px; } "
	+++ ".json { font-family: Courier, monotype; font-size: 12px;} "
	+++ ".json ul { padding-left: 15px;} "
	+++ "h1 { margin: 10px 15px 10px 15px; font-weight: normal; font-size: 24px;} "
	+++ "h2 { margin: 5px 5px 5px 0px; font-weight: bold; font-size: 14px;  border: solid #999; border-width: 0px 0px 1px 0px;} "
	+++ "p { margin: 0px 0px 10px 0px; } "
	+++ "button {-moz-border-radius: 3px; }"
	
pageLayout :: !String !String ![HtmlTag] -> HtmlTag
pageLayout title description content = HtmlTag [] [head,body]
where
	head = HeadTag [] [TitleTag [] [Text title], embeddedStyle]
	body = BodyTag [] [DivTag [IdAttr "main"] (header ++ content)]
	
	header = [H1Tag [] [Text title],PTag [] [DivTag [ClassAttr "description"] [RawText description]]]


servicePage :: !String !String !String ![(String,String,Bool)] !JSONNode -> HtmlTag
servicePage title description url params json = pageLayout title description [parameters, message, alternatives]
where
	parameters	= pageSection "Parameters" [FormTag [ActionAttr url,MethodAttr "get"] [TableTag [ClassAttr "parameters"] (rows ++ send)]]
	rows		= [TrTag [] [ThTag [] [Text n : if o [Text "*:"] [Text ":"]], TdTag [] [InputTag [NameAttr n, ValueAttr v]]] \\ (n,v,o) <- params]
	send		= [TrTag [] [TdTag [ColspanAttr "4"] [ButtonTag [TypeAttr "submit"] [Text "Send"]]]]
	message		= pageSection "Data" [DivTag [ClassAttr "json"] (formatJSON json)]
	jsonurl		= replaceSubString "services/html" "services/json" url
	alternatives= pageSection "Alternative representations" [PTag [] [Text "JSON: ", ATag [HrefAttr jsonurl] [Text jsonurl]]]
	
serviceResponse :: !Bool !String !String !String ![(String,String,Bool)] !JSONNode -> HTTPResponse
serviceResponse html title description url params json =
		if html	{newHTTPResponse & rsp_data = toString (servicePage title description url params json)}
				{ newHTTPResponse
				//Content-Type for JSON should be "application/json", see http://www.ietf.org/rfc/rfc4627.txt
				& rsp_headers = put "Content-Type" "application/json" (newHTTPResponse.rsp_headers) 
				, rsp_data = toString json
				}


formatJSON :: !JSONNode -> [HtmlTag]
formatJSON (JSONNull)			= [Text "null"]
formatJSON (JSONBool True)		= [Text "true"]
formatJSON (JSONBool False)		= [Text "false"]
formatJSON (JSONInt i)			= [Text (toString i)]
formatJSON (JSONReal r)			= [Text (toString r)]
formatJSON (JSONString s)		= [Text "\"", Text s, Text "\""]
formatJSON (JSONArray items)	= [UlTag [] [LiTag [] (formatJSON node) \\ node <- items] ]
formatJSON (JSONObject fields)	= [UlTag [] [LiTag [] [Text label,Text ": " :formatJSON node] \\(label,node) <- fields ] ]
formatJSON (JSONRaw r)			= [PreTag [] [Text (toString r)]]
formatJSON _					= []

overviewPage :: HtmlTag
overviewPage = pageLayout "Services" description [application,sessions,workflows,tasks,users,documents,stencils]
where
	description = "This application can be accessed through a RESTful JSON API.<br />Below is an overview of the available service urls."
	
	application = pageSection "application"
		[ATag [HrefAttr "html/application"] [Text "General information information about this application"]]
	sessions	= pageSection "sessions"
		[ATag [HrefAttr "html/sessions"] [Text "Authentication and session management"]]
	workflows	= pageSection "workflows"
		[ATag [HrefAttr "html/workflows"] [Text "A catalogue of available workflows"]]
	tasks		= pageSection "tasks"
		[ATag [HrefAttr "html/tasks"] [Text "Listing of and working on tasks"]]
	users		= pageSection "users"
		[ATag [HrefAttr "html/users"] [Text "User management"]]
	documents	= pageSection "documents"
		[ATag [HrefAttr "html/documents"] [Text "Upload/download of binary files"]]
	stencils	= pageSection "stencils"
		[ATag [HrefAttr "html/stencils"] [Text "Catalogue of stencils for use in graphical workflow diagrams"]]
	
overviewResponse :: HTTPResponse
overviewResponse = {newHTTPResponse & rsp_data = toString overviewPage}

appStartPage :: !String -> HtmlTag
appStartPage appName = HtmlTag [] [head,body]
where
	head = HeadTag [] [TitleTag [] [Text "Loading..."]: styles ++ scripts]
	body = BodyTag [] []

	styles = [LinkTag [RelAttr "stylesheet", HrefAttr file, TypeAttr "text/css"] [] \\ file <- stylefiles]
	scripts = [ScriptTag [SrcAttr file, TypeAttr "text/javascript"] [] \\ file <- scriptfiles]
	
	stylefiles = ["/lib/ext-4.0.2a/resources/css/ext-all-gray.css"
				 ,"/src/css/main.css"
				 ,appName +++ ".css"]
	scriptfiles = ["/lib/ext-4.0.2a/ext-debug.js","src/app.js"]

/**
* Creates an HTTP response of the start page
*/
appStartResponse :: !String -> HTTPResponse
appStartResponse appName = {newHTTPResponse & rsp_data = toString (appStartPage appName)}

redirectResponse :: !String -> HTTPResponse
redirectResponse url
	= {HTTPResponse | rsp_headers = fromList [("Status","302 - Found"),("Location",url)], rsp_data = ""}

notFoundPage :: !HTTPRequest -> HtmlTag
notFoundPage req = pageLayout "404 - Not Found" "" message
where
	message = [DivTag [IdAttr "content"] [Text "The resource you tried to access ",StrongTag [] [Text req.req_path], Text " could not be found."]] 

notFoundResponse :: !HTTPRequest -> HTTPResponse
notFoundResponse req
	= {HTTPResponse | rsp_headers = fromList [("Status","404 - Not Found")], rsp_data = toString (notFoundPage req)}

pageSection :: !String ![HtmlTag] -> HtmlTag
pageSection title content = DivTag [ClassAttr "section"] [H2Tag [] [Text title]:content]

paramValue :: !String !HTTPRequest -> String
paramValue name req
	= case get name req.arg_post of
		Just val	= val
		Nothing		= case get name req.arg_get of
			Just val	= val
			Nothing		= ""

NEWLINE	:== "\n"

nl2br :: !String -> HtmlTag
nl2br str = html [[Text line,BrTag []] \\ line <- split NEWLINE str]

html2text :: !String -> String
html2text s
	# s	= replaceSubString "<br>" NEWLINE s
	# s	= replaceSubString "<BR>" NEWLINE s
	# s	= replaceSubString "<br/>" NEWLINE s
	# s	= replaceSubString "<BR/>" NEWLINE s
	# s	= replaceSubString "</li>" NEWLINE s
	# s	= stripHtmlTags s
	# s = replaceSubString "&nbsp;" " " s
	# s = replaceSubString "&lt;" "<" s
	# s = replaceSubString "&gt;" ">" s
	# s = replaceSubString "&amp;" "&" s
	= s
where
	stripHtmlTags s
		# fstOpen	= indexOf "<" s
		# fstClose	= indexOf ">" s
		| fstOpen <> -1 && fstClose <> -1 && fstOpen < fstClose
			= stripHtmlTags (subString 0 fstOpen s +++ subString (fstClose + 1) (textSize s - fstClose) s)
		| otherwise
			= s

