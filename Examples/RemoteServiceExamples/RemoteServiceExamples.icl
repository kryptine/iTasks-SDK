module RemoteServiceExamples

import iTasks
import Internet.HTTP

import Data.Either

:: OpenWeatherRequest =
	{ apiKey :: String
	, type :: OpenWeatherRequestType
	}

:: OpenWeatherRequestType = ByCityName String | ByCoordinates Real Real

:: OpenWeatherResponse =
	{ id :: Int
	, main :: String
	, description :: String
	, icon :: String }

derive class iTask OpenWeatherResponse

Start world = startEngine serviceTask world
where
	serviceTask = get weatherService >>= viewInformation "Current weather" []

// api.openweathermap.org/data/2.5/weather?q=London,uk
weatherOptions :: OpenWeatherRequest -> WebServiceShareOptions (Either String OpenWeatherResponse)
weatherOptions owr = HttpShareOptions (toRequest owr) fromResp
where
	toRequest {OpenWeatherRequest|apiKey, type}
	# r = newHTTPRequest
	= {HTTPRequest|r & server_name = "api.openweathermap.org", server_port = 80, req_path = "/data/2.5/weather", req_query = query type +++ "&APPID=" +++ apiKey}

	fromResp response = case jsonQuery "weather/0" (fromString response.rsp_data) of
		Nothing = Right (Left ("Could not select JSON"))
		(Just selected) = case fromJSON selected of
			Nothing = Right (Left ("Could not transform JSON"))
			(Just v) = Right (Right v)

	query (ByCityName name) 		= "?q=" +++ name
	query (ByCoordinates lat long) 	= "?lat=" +++ toString lat +++ "&lon=" +++ toString long

weatherService :: SDSRemoteService () (Either String OpenWeatherResponse) ()
weatherService = remoteService (weatherOptions {apiKey = "1160ac287072c67ae44708dee89f9a8b" , type = ByCityName "Nijmegen"})