module AsyncShareTest

import iTasks
import iTasks.Internal.Distributed.Instance

import Data.Func
import Data.Tuple
import Data.Maybe
import Data.Functor
import Data.Either
import StdMisc

import Internet.HTTP

derive class iTask TestRecord, OpenWeatherResponse

:: TestRecord = {number :: Int, numbers :: [Int], text :: String, texts :: [String]}

testShare = sharedStore "sharedStoreNamebla" {number = 37, numbers = [1, 2, 3], text = "Test", texts = ["een", "twee", "drie", "vier"]}
remoteTestShare = remoteShare testShare {domain = "TEST", port = 8080}

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

// api.openweathermap.org/data/2.5/weather?q=London,uk
weatherOptions :: OpenWeatherRequest -> WebServiceShareOptions OpenWeatherResponse
weatherOptions owr = HttpShareOptions (toRequest owr) fromResp
where
	toRequest {OpenWeatherRequest|apiKey, type}
	# r = newHTTPRequest
	= {HTTPRequest|r & server_name = "api.openweathermap.org", server_port = 80, req_path = "/data/2.5/weather", req_query = query type +++ "&APPID=" +++ apiKey}

	fromResp response = case jsonQuery "weather/0" (fromString response.rsp_data) of
		Nothing = Left "Could not select JSON"
		(Just selected) = case fromJSON selected of
			Nothing = Left "Could not transform JSON"
			(Just v) = Right v

	query (ByCityName name) 		= "?q=" +++ name 
	query (ByCoordinates lat long) 	= "?lat=" +++ toString lat +++ "&lon=" +++ toString long

weatherService = remoteService (weatherOptions {apiKey = "1160ac287072c67ae44708dee89f9a8b" , type = ByCityName "Nijmegen"})

Start world	= startEngineWithOptions opts maintask world
where
	opts [] = \op->(Just {op&distributed=True}, ["Started server on port: " +++ toString op.serverPort])
	opts ["-p",p:as] = appFst (fmap (\o->{o & serverPort=toInt p})) o opts as
	opts [a:as] = opts as

maintask = viewInformation "Choose your role" [] ()
	>>* [OnAction (Action "Domain server") (always domainServer)
		,OnAction (Action "Client") (always client)
		,OnAction (Action "Weather") (always weather)
		]
where
	domainServer = updateSharedInformation "This share is shared in the domain" [] testShare

	client = updateSharedInformation "This share is stored somewhere else" [] remoteTestShare

	weather = get weatherService >>- viewInformation "This is the current weather in Nijmegen" [] >!| client
