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

	sdsSelectRemoteTest = ((Hint "Enter the value to be SET for SDSSelect" @>> enterInformation [] >>! \v. set v (sdsFocus 0 selectShare))
		-&&-
		(get (sdsFocus 0 selectShare) >>- \value -> Hint "View the value gotten for SDSSelect by GET" @>> viewInformation [] value))
		-&&-
		((Hint "Enter the new value for the lens" @>> enterInformation [] >>! \n. upd (\_. n) (sdsFocus 0 selectShare))
		-&&-
		(Hint "View value by viewSharedInformation" @>> viewSharedInformation [] (sdsFocus 0 selectShare)))
		@! ()

	sdsSelectTest = ((Hint "Enter the value to be SET for SDSSelect" @>> enterInformation [] >>! \v. set v (sdsFocus 1 selectShare))
		-&&-
		(get  (sdsFocus 1 selectShare) >>- \value -> Hint "View the value gotten for SDSSelect by GET" @>> viewInformation [] value))
		-&&-
		((Hint "Enter the new value for the lens" @>> enterInformation [] >>! \n. upd (\_. n)  (sdsFocus 1 selectShare))
		-&&-
		(Hint "View value by viewSharedInformation" @>> viewSharedInformation []  (sdsFocus 1 selectShare)))
		@! ()

	sdsParallelRemoteRightTest = ((Hint "Enter the value to be SET for SDSParallelRemoteRight" @>> enterInformation  [] >>! \v. set v parallelWithRightRemote)
		-&&-
		(get parallelWithRightRemote >>- \value -> Hint "View the value gotten for SDSParallelRemoteRight by GET" @>> viewInformation [] value))
		-&&-
		((Hint "Enter the new value for the lens" @>> enterInformation [] >>! \n. upd (\_. n) parallelWithRightRemote)
		-&&-
		(Hint "View value by viewSharedInformation" @>> viewSharedInformation [] parallelWithRightRemote))
		@! ()

	sdsParallelRemoteLeftTest = ((Hint "Enter the value to be SET for SDSParallelRemoteLeft" @>> enterInformation [] >>! \v. set v parallelWithLeftRemote)
		-&&-
		(get parallelWithLeftRemote >>- \value -> Hint "View the value gotten for SDSParallelRemoteLeft by GET" @>> viewInformation [] value))
		-&&-
		((Hint "Enter the new value for the lens" @>> enterInformation [] >>! \n. upd (\_. n) parallelWithLeftRemote)
		-&&-
		(Hint "View value by viewSharedInformation" @>> viewSharedInformation [] parallelWithLeftRemote))
		@! ()

	sdsParallelTest = ((Hint "Enter the value to be SET for SDSParallel" @>> enterInformation [] >>! \v. set v parallelShare)
		-&&-
		(get parallelShare >>- \value -> Hint "View the value gotten for SDSParallel by GET" @>> viewInformation [] value))
		-&&-
		((Hint "Enter the new value for the lens" @>> enterInformation  [] >>! \n. upd (\_. n) parallelShare)
		-&&-
		(Hint "View value by viewSharedInformation" @>> viewSharedInformation  [] parallelShare))
		@! ()

	sdsLensTest = ((Hint "Enter the value to be SET for SDSLensLocal" @>> enterInformation [] >>! \v. set v projectedLocal)
		-&&-
		(get projectedLocal >>- \value -> Hint "View the value gotten for SDSLensLocal by GET" @>> viewInformation [] value))
		-&&-
		((Hint "Enter the new value for the lens" @>> enterInformation [] >>! \n. upd (\_. n) projectedLocal)
		-&&-
		(Hint "View value by viewSharedInformation" @>> viewSharedInformation [] projectedLocal))
		@! ()

	sdsLensRemoteTest = ((Hint "Enter the value to be SET for SDSLensRemote" @>> enterInformation [] >>! \v. set v projectedRemote)
		-&&-
		(get projectedRemote >>- \value -> Hint "View the value gotten for SDSLensRemote by GET" @>> viewInformation [] value))
		-&&-
		((Hint "Enter the new value for the lens" @>> enterInformation [] >>! \n. upd (\_. n) projectedRemote)
		-&&-
		(Hint "View value by viewSharedInformation" @>> viewSharedInformation [] projectedRemote))
		@! ()

	sdsSourceTest = ((Hint "Enter the value to be SET for SDSSource" @>> enterInformation [] >>! \v. set v testShare)
		-&&-
		(get testShare >>- \value -> Hint "View the value gotten for SDSSource by GET" @>> viewInformation [] value))
		-&&-
		((Hint "Enter the new value for the number" @>> enterInformation [] >>! \n. upd (\tr. {tr & number = n}) testShare)
		-&&-
		(Hint "View value by viewSharedInformation" @>> viewSharedInformation [] testShare))
		@! ()

	// We can get, set, and upd the value of a top-level remote source,
	sdsRemoteTest =
		((Hint "Enter the value to be SET for SDSRemote" @>> enterInformation [] >>! \v. set v remoteTestShare)
		-&&-
		(get remoteTestShare >>- \value -> Hint "View the value gotten for SDSRemote by GET" @>> viewInformation [] value))
		-&&-
		((Hint "Enter the new value for the number" @>> enterInformation [] >>! \n. upd (\tr. {tr & number = n}) remoteTestShare)
		-&&-
		(Hint "View value by viewSharedInformation" @>> viewSharedInformation [] remoteTestShare))
		@! ()


	viewAll = forever (((Hint "Value of testShare" @>> updateSharedInformation [] testShare)
		-&&- (Hint "Value of leftShare" @>> updateSharedInformation  [] leftShare)
		-&&- (Hint "Value of rightShare" @>> updateSharedInformation  [] rightShare)
		-&&- (Hint "Value of intShare" @>> updateSharedInformation [] intShare))
		@! ())

	singleRemoteTest = Hint "Update value by viewSharedInformation" @>> updateSharedInformation [] simpleShare @! ()

	doubleRemoteTest
	//# setV = enterInformation "Enter the value to be SET for double remote" [] >>! \v. set v doubleRemote >>- viewInformation "Set value" []
	//# getV = get doubleRemote >>- viewInformation "View the value gotten for double remote by GET" []
	//# updV = enterInformation "Enter the new value for the number" [] >>! \n. upd (\_. n) doubleRemote >>? viewInformation "Updated value" []
	# shaV = Hint "Update value by viewSharedInformation" @>> updateSharedInformation  [] doubleRemote
 	= shaV @! ()
