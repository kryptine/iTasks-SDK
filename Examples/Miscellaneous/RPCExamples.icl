implementation module RPCExamples

import iTasks, XML, Error

rpcExamples :: [Workflow]
rpcExamples = 	[ workflow	"Examples/Miscellaneous/Weather forecast" "Fetches the weather forecast from a remote server" weatherExample
				]

GOOGLE_API = "http://www.google.com/ig/api"

weatherExample :: Task Void
weatherExample = 
	try ( 	  callRPCHTTP GET GOOGLE_API [("weather", "Nijmegen, Netherlands"),("hl","en-GB")] formatResponse
		  >>= wait "Waiting for weather service" True
		  >>= \weather -> showMessageAbout ("Weather", "Weather forecast is:") weather >>| stop
		)
		(\(RPCException message) -> showMessage ("RPC call failed", message) Void)

formatResponse :: !String -> HtmlDisplay
formatResponse xmlStr = case fromString xmlStr of
	Ok xmlDoc 	= toHtmlDisplay (formatXML xmlDoc)
	Error err	= toHtmlDisplay [Text err]
	
formatXML (XMLDoc _ _ e) = [ul [formatElem e]]
formatElem (XMLElem (XMLQName _ n) [XMLAttr (XMLQName _ "data") data] children) = li [formattedData,ul (map formatElem children)]
where
	formattedData
		| n == "icon"	= ImgTag [SrcAttr ("http://www.google.com" +++ data)]
		| otherwise		= Text (n +++ ": " +++ data)
formatElem (XMLElem (XMLQName _ n) attr children)
	# childrenHtml = map formatElem children
	| isMember n showElements	= li [Text n,ul (map formatAttr attr ++ childrenHtml)]
	| otherwise					= ul childrenHtml
formatAttr (XMLAttr (XMLQName _ n) v) = li [Text (n +++ ": " +++ v)]

ul = UlTag [StyleAttr "list-style: disc;padding-left:20 px"]
li = LiTag [StyleAttr "margin-left: 20px"]

showElements = ["forecast_information","current_conditions","forecast_conditions"]
