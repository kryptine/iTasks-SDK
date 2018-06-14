module RemoteShareExamples

import iTasks
import iTasks.Internal.Distributed.Instance

import Data.Func
import Data.Tuple
import Data.Maybe
import Data.Functor
import Data.Either

derive class iTask TestRecord

:: TestRecord = {number :: Int, numbers :: [Int], text :: String, texts :: [String]}

testShare = sharedStore "sharedStoreNamebla" {number = 37, numbers = [1, 2, 3], text = "Test", texts = ["een", "twee", "drie", "vier"]}
remoteTestShare = remoteShare testShare {domain = "TEST", port = 8080}

leftShare = sharedStore "leftShare" (1, 2, 3)
rightShare = sharedStore "rightShare" (10, 20, 30)

parallelShare = leftShare >*< rightShare
remoteParallelShare = remoteShare parallelShare {domain = "TEST", port = 8080}

parallelWithLeftRemote = (remoteShare leftShare {domain = "TEST", port = 8080}) >*< rightShare
parallelWithRightRemote = leftShare >*< (remoteShare rightShare {domain = "TEST", port = 8080})

intShare = sharedStore "intShare" 15
simpleShare = remoteShare intShare {domain="TEST", port=8080}
projectedRemote = sdsProject (SDSLensRead (\r. Ok (r + 2))) (SDSLensWrite (\_ r. Ok (DoWrite (r - 2)))) (\_ ws. Ok (ws + 2))  simpleShare
projectedLocal = sdsProject (SDSLensRead (\r. Ok (r + 2))) (SDSLensWrite (\_ r. Ok (DoWrite (r - 2)))) (\_ ws. Ok (ws + 2))  intShare

selectShare = sdsSelect "testSelect" param (SDSNotifyConst (\_ _ _ _-> False)) (SDSNotifyConst (\_ _ _ _-> False))
		(remoteShare leftShare {domain="TEST", port=8080}) rightShare
where
	param i
	| i == 0 = Left ()
	= Right ()

Start world	= startEngineWithOptions opts tests world
where
	opts [] = \op->(Just {op&distributed=True}, ["Started server on port: " +++ toString op.serverPort])
	opts ["-p",p:as] = appFst (fmap (\o->{o & serverPort=toInt p})) o opts as
	opts [a:as] = opts as

	tests = [ publish "/SDSSource" (const sdsSourceTest) 
			, publish "/SDSRemote" (const sdsRemoteTest)
			, publish "/SDSLens"  (const sdsLensTest)
			, publish "/SDSLens/remote"  (const sdsLensRemoteTest)
			, publish "/SDSParallel"  (const sdsParallelTest)
			, publish "/SDSParallel/remoteleft"  (const sdsParallelRemoteLeftTest)
			, publish "/SDSParallel/remoteright"  (const sdsParallelRemoteRightTest)
			, publish "/SDSRemoteService"  (const sdsRemoteServiceTest)
			, publish "/SDSSelect"  (const sdsSelectTest)
			, publish "/SDSSelectRemote"  (const  sdsSelectRemoteTest)
			, publish "/all" (\_. viewAll)]

	sdsSelectRemoteTest = ((enterInformation "Enter the value to be SET for SDSSelect" [] >>= \v. set v (sdsFocus 0 selectShare))
		-&&-
		(get (sdsFocus 0 selectShare) >>= viewInformation "View the value gotten for SDSSelect by GET" []))
		-&&-
		((enterInformation "Enter the new value for the lens" [] >>= \n. upd (\_. n) (sdsFocus 0 selectShare))
		-&&-
		(viewSharedInformation "View value by viewSharedInformation" [] (sdsFocus 0 selectShare)))
		@! ()

	sdsSelectTest = ((enterInformation "Enter the value to be SET for SDSSelect" [] >>= \v. set v (sdsFocus 1 selectShare))
		-&&-
		(get  (sdsFocus 1 selectShare) >>= viewInformation "View the value gotten for SDSSelect by GET" []))
		-&&-
		((enterInformation "Enter the new value for the lens" [] >>= \n. upd (\_. n)  (sdsFocus 1 selectShare))
		-&&-
		(viewSharedInformation "View value by viewSharedInformation" []  (sdsFocus 1 selectShare)))
		@! ()

	sdsRemoteServiceTest = get weatherService >>= viewInformation "Current weather" []

	sdsParallelRemoteRightTest = ((enterInformation "Enter the value to be SET for SDSParallelRemoteRight" [] >>= \v. set v parallelWithRightRemote)
		-&&-
		(get parallelWithRightRemote >>= viewInformation "View the value gotten for SDSParallelRemoteRight by GET" []))
		-&&-
		((enterInformation "Enter the new value for the lens" [] >>= \n. upd (\_. n) parallelWithRightRemote)
		-&&-
		(viewSharedInformation "View value by viewSharedInformation" [] parallelWithRightRemote))
		@! ()

	sdsParallelRemoteLeftTest = ((enterInformation "Enter the value to be SET for SDSParallelRemoteLeft" [] >>= \v. set v parallelWithLeftRemote)
		-&&-
		(get parallelWithLeftRemote >>= viewInformation "View the value gotten for SDSParallelRemoteLeft by GET" []))
		-&&-
		((enterInformation "Enter the new value for the lens" [] >>= \n. upd (\_. n) parallelWithLeftRemote)
		-&&-
		(viewSharedInformation "View value by viewSharedInformation" [] parallelWithLeftRemote))
		@! ()

	sdsParallelTest = ((enterInformation "Enter the value to be SET for SDSParallel" [] >>= \v. set v parallelShare)
		-&&-
		(get parallelShare >>= viewInformation "View the value gotten for SDSParallel by GET" []))
		-&&-
		((enterInformation "Enter the new value for the lens" [] >>= \n. upd (\_. n) parallelShare)
		-&&-
		(viewSharedInformation "View value by viewSharedInformation" [] parallelShare))
		@! ()

	sdsLensTest = ((enterInformation "Enter the value to be SET for SDSLensLocal" [] >>= \v. set v projectedLocal)
		-&&-
		(get projectedLocal >>= viewInformation "View the value gotten for SDSLensLocal by GET" []))
		-&&-
		((enterInformation "Enter the new value for the lens" [] >>= \n. upd (\_. n) projectedLocal)
		-&&-
		(viewSharedInformation "View value by viewSharedInformation" [] projectedLocal))
		@! ()

	sdsLensRemoteTest = ((enterInformation "Enter the value to be SET for SDSLensRemote" [] >>= \v. set v projectedRemote)
		-&&-
		(get projectedRemote >>= viewInformation "View the value gotten for SDSLensRemote by GET" []))
		-&&-
		((enterInformation "Enter the new value for the lens" [] >>= \n. upd (\_. n) projectedRemote)
		-&&-
		(viewSharedInformation "View value by viewSharedInformation" [] projectedRemote))
		@! ()

	sdsSourceTest = ((enterInformation "Enter the value to be SET for SDSSource" [] >>= \v. set v testShare)
		-&&-
		(get testShare >>= viewInformation "View the value gotten for SDSSource by GET" []))
		-&&-
		((enterInformation "Enter the new value for the number" [] >>= \n. upd (\tr. {tr & number = n}) testShare)
		-&&-
		(viewSharedInformation "View value by viewSharedInformation" [] testShare))
		@! ()

	// We can get, set, and upd the value of a top-level remote source,
	sdsRemoteTest = 
		((enterInformation "Enter the value to be SET for SDSRemote" [] >>= \v. set v remoteTestShare)
		-&&-
		(get remoteTestShare >>= viewInformation "View the value gotten for SDSRemote by GET" []))
		-&&-
		((enterInformation "Enter the new value for the number" [] >>= \n. upd (\tr. {tr & number = n}) remoteTestShare)
		-&&-
		(viewSharedInformation "View value by viewSharedInformation" [] remoteTestShare))
		@! ()


	viewAll = forever ((viewSharedInformation "Value of testShare" [] testShare
		-&&- viewSharedInformation "Value of leftShare" [] leftShare
		-&&- viewSharedInformation "Value of rightShare" [] rightShare
		-&&- viewSharedInformation "Value of intShare" [] intShare)
		@! ())

// ======= Definitions required for defining a remote service =======
// TODO: Create HTTP request by focussing the parameter

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