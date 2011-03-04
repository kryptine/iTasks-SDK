implementation module RPCExamples

import iTasks

rpcExamples :: [Workflow]
rpcExamples = 	[ workflow	"Examples/Miscellaneous/Weather forecast" "Fetches the weather forecast from a remote server" weatherExample
				]

GOOGLE_API = "http://www.google.com/ig/api"

weatherExample :: Task Void
weatherExample = 
	try ( 	  callRPCHTTP GET GOOGLE_API [("weather", "Nijmegen, Netherlands")] id
		  >>= wait "Waiting for weather service" True
		  >>= \weather -> showMessageAbout ("Weather", "Weather forecast is:") weather >>| stop
		)
		(\(RPCException message) -> showMessage ("RPC call failed", message) Void)
	