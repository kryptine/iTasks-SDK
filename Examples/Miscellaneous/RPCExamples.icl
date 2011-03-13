implementation module RPCExamples

import iTasks, XML, Error, Text, GoogleMaps, google_maps_services

rpcExamples :: [Workflow]
rpcExamples = 	[ workflow	"Examples/Miscellaneous/Weather forecast" "Fetches the weather forecast from a remote server" weatherExample
				]

GOOGLE_API = "http://www.google.com/ig/api"

weatherExample :: Task Void
weatherExample = 
	try (					
							enterLocation -||- markLocation
			>>= \location.	callRPCHTTP GET GOOGLE_API [("weather", location),("hl","en-GB")] formatResponse
			>>= 			wait "Waiting for weather service" True
			>>= \weather -> showMessageAbout ("Weather", "Weather forecast is:") weather >>| stop
		)
		(\(RPCException message) -> showMessage ("RPC call failed", message) Void)

enterLocation = enterInformation ("Enter location", "Enter a location you want to retrieve the weather forecast for.")	
markLocation =
										updateInformationA ("Mark location","Mark the locations you want to retrieve the weather forecast for.") (toView,fromView) [(ActionOk,oneLocation)] mkMap
	>>= \(_,Just {markers=ms=:[m:_]}).	reverse_geocoding (toString m.position.lat+++","+++toString m.position.lng) "json" False GOOGLE_API_KEY parseJSON
	>>=									wait ("Address lookup","Address is being retrieved for coordinates: ("+++toString m.position.lat+++", "+++toString m.position.lng+++")") True
where
	toView = id
	fromView map=:{markers} _ = {map & markers = if (isEmpty markers) [] [hd (reverse markers)]}
	
	oneLocation (Valid {markers}) = (length markers) == 1
	oneLocation _ = False

	parseJSON info = case jsonQuery "Placemark/0/address" (fromString info) of
		(Just addr) = replaceSubString ", " "\n" addr
		_			= "Address Unknown"

formatResponse :: !String -> HtmlDisplay
formatResponse xmlStr = case fromString xmlStr of
	Ok xmlDoc 	= toHtmlDisplay (formatXML xmlDoc)
	Error err	= toHtmlDisplay [Text err]
	
formatXML (XMLDoc _ _ e) = [ul [formatElem e]]
formatElem (XMLElem (XMLQName _ n) [XMLAttr (XMLQName _ "data") data] children) = li [formattedData,ul (map formatElem children)]
where
	formattedData
		| n == "icon"	= ImgTag [SrcAttr ("http://www.google.com" +++ data)]
		| otherwise		= Text (formatName n +++ ": " +++ data)
formatElem (XMLElem (XMLQName _ n) attr children)
	# childrenHtml = map formatElem children
	| isMember n showElements	= li [Text (formatName n),ul (map formatAttr attr ++ childrenHtml)]
	| otherwise					= ul childrenHtml
formatAttr (XMLAttr (XMLQName _ n) v) = li [Text (formatName n +++ ": " +++ v)]

ul = UlTag [StyleAttr "list-style: disc;padding-left:20 px"]
li = LiTag [StyleAttr "margin-left: 20px"]

showElements = ["forecast_information","current_conditions","forecast_conditions"]

formatName name = upperCaseFirst (replaceSubString "_" " " name)

