module RemoteShareExamples

import iTasks

import Data.Func
import Data.Tuple
import Data.Maybe
import Data.Functor
import Data.Either

derive class iTask TestRecord

:: TestRecord = {number :: Int, numbers :: [Int], text :: String, texts :: [String]}

testShare = sharedStore "sharedStoreNamebla" {number = 37, numbers = [1, 2, 3], text = "Test", texts = ["een", "twee", "drie", "vier"]}
remoteTestShare = remoteShare testShare {domain = "localhost", port = 9999}

leftShare = sharedStore "leftShare" (1, 2, 3)
rightShare = sharedStore "rightShare" (10, 20, 30)

parallelShare = leftShare >*< rightShare
remoteParallelShare = remoteShare parallelShare {domain = "localhost", port = 9999}

parallelWithLeftRemote = (remoteShare leftShare {domain = "localhost", port = 9999}) >*< rightShare
parallelWithRightRemote = leftShare >*< (remoteShare rightShare {domain = "localhost", port = 9999})

intShare = sharedStore "intShare" 15
simpleShare = remoteShare intShare {domain="localhost", port=9999}
projectedRemote = sdsProject (SDSLensRead (\r. Ok (r + 2))) (SDSLensWrite (\_ r. Ok (Just (r - 2)))) (Just \_ ws. Ok (ws + 2))  simpleShare
projectedLocal = sdsProject (SDSLensRead (\r. Ok (r + 2))) (SDSLensWrite (\_ r. Ok (Just (r - 2)))) (Just \_ ws. Ok (ws + 2))  intShare

selectShare = sdsSelect "testSelect" param (SDSNotifyConst (\_ _ _ _-> False)) (SDSNotifyConst (\_ _ _ _-> False))
		(remoteShare leftShare {domain="localhost", port=9999}) rightShare
where
	param i
	| i == 0 = Left ()
	= Right ()

doubleRemote = remoteShare simpleShare {domain="localhost", port=9998}

Start world	= doTasks tests world
where

	tests = [ publish "/SDSSource" (const sdsSourceTest)
			, publish "/SDSRemote" (const sdsRemoteTest)
			, publish "/SDSLens"  (const sdsLensTest)
			, publish "/SDSLens/remote"  (const sdsLensRemoteTest)
			, publish "/SDSParallel"  (const sdsParallelTest)
			, publish "/SDSParallel/remoteleft"  (const sdsParallelRemoteLeftTest)
			, publish "/SDSParallel/remoteright"  (const sdsParallelRemoteRightTest)
			, publish "/SDSSelect"  (const sdsSelectTest)
			, publish "/SDSSelectRemote"  (const  sdsSelectRemoteTest)
			, publish "/all" (\_. viewAll)
			, publish "/doubleRemote" (const doubleRemoteTest)
			, publish "/singleRemote" (const singleRemoteTest)]

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


	viewAll = forever ((updateSharedInformation "Value of testShare" [] testShare
		-&&- updateSharedInformation "Value of leftShare" [] leftShare
		-&&- updateSharedInformation "Value of rightShare" [] rightShare
		-&&- updateSharedInformation "Value of intShare" [] intShare)
		@! ())

	singleRemoteTest = updateSharedInformation "Update value by viewSharedInformation" [] simpleShare @! ()

	doubleRemoteTest
	//# setV = enterInformation "Enter the value to be SET for double remote" [] >>= \v. set v doubleRemote >>= viewInformation "Set value" []
	//# getV = get doubleRemote >>= viewInformation "View the value gotten for double remote by GET" []
	//# updV = enterInformation "Enter the new value for the number" [] >>= \n. upd (\_. n) doubleRemote >>= viewInformation "Updated value" []
	# shaV = updateSharedInformation "Update value by viewSharedInformation" [] doubleRemote
 	= shaV @! ()
