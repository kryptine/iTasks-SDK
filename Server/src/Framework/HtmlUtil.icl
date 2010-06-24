implementation module HtmlUtil

import Html, JSON, Http
import StdList

embeddedStyle :: HtmlTag
embeddedStyle = StyleTag [TypeAttr "text/css"] [RawText css] 
where
 css = 	"body { background: #d1dded; font-family: Verdana, Arial, sans-serif; font-size: 12px;} th { text-align: left; } "
	+++ ".field-error em {color: #f00; font-weight: bold} .field-error input {border-color: #f00;} "
	+++ "#main {width: 700px; position: absolute; left: 50%; margin-left: -350px; top: 50px; background: #fff; border: solid 2px #3a81ad; -moz-border-radius: 5px; background: -moz-linear-gradient(bottom,  #3a81ad,  #fff);} "
	+++ "#content { padding: 10px; } "
	+++ ".buttons { padding: 5px; background-color: #3a81ad; } "
	+++ ".section { margin: 10px; border: solid 1px #d1dded; -moz-border-radius: 10px; padding: 5px; overflow: auto;} "
	+++ ".parameters th, .parameters td { width: 25%; } "
	+++ ".json { font-family: Courier, monotype; font-size: 12px;} "
	+++ ".json ul { padding-left: 15px;} "
	+++ "h1 { margin: 10px; font-weight: normal; font-size: 24px;} "
	+++ "h2 { margin: 5px; font-weight: bold; font-size: 14px;} "
	+++ "p { margin: 0px 0px 10px 0px; } "
	+++ "button {-moz-border-radius: 3px; }"
	
pageLayout :: !String ![HtmlTag] -> HtmlTag
pageLayout title content = HtmlTag [] [head,body]
where
	head = HeadTag [] [TitleTag [] [Text title], embeddedStyle]
	body = BodyTag [] [DivTag [IdAttr "main"] [header:content]]
	
	header = H1Tag [] [Text title]


servicePage :: !String !String ![(String,String,Bool)] JSONNode -> HtmlTag
servicePage title url params json = pageLayout title [parameters, message]
where
	parameters	= pageSection "Parameters" [FormTag [ActionAttr url,MethodAttr "get"] [TableTag [ClassAttr "parameters"] (rows ++ send)]]
	rows		= [TrTag [] (flatten [ [ThTag [] [Text n: if o [Text "*:"] [Text ":"]], TdTag [] [InputTag [NameAttr n,ValueAttr v] ]] \\ (n,v,o) <- cells]) \\ cells <- segment params]
	send		= [TrTag [] [TdTag [ColspanAttr "4"] [ButtonTag [TypeAttr "submit"] [Text "Send"]]]]
	message		= pageSection "Message" [DivTag [ClassAttr "json"] (formatJSON json)]

	segment []			= []
	segment [x]			= [[x]]
	segment [x1,x2:xs]	= [[x1,x2]:segment xs]

serviceResponse :: !Bool !String !String ![(String,String,Bool)] JSONNode -> HTTPResponse
serviceResponse html title url params json =
		if html	{http_emptyResponse & rsp_data = toString (servicePage title url params json)}
				{http_emptyResponse & rsp_data = toString json}


formatJSON :: JSONNode -> [HtmlTag]
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

notFoundPage :: !HTTPRequest -> HtmlTag
notFoundPage req = pageLayout "404 - Not Found" message
where
	message = [DivTag [IdAttr "content"] [Text "The resource you tried to access ",StrongTag [] [Text req.req_path], Text " could not be found."]] 

notFoundResponse :: !HTTPRequest -> HTTPResponse
notFoundResponse req
	= {HTTPResponse | rsp_headers = [("Status","404 - Not Found")], rsp_data = toString (notFoundPage req)}

pageSection :: !String ![HtmlTag] -> HtmlTag
pageSection title content = DivTag [ClassAttr "section"] [H2Tag [] [Text title]:content]


paramValue :: !String !HTTPRequest -> String
paramValue name req = http_getValue name (req.arg_post ++ req.arg_get) ""
