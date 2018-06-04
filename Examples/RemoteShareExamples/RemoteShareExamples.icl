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

Start world	= startEngineWithOptions opts [publish "/" (\_ ->  loginAndManageWorkList "Hello!" flows)] world
where
	opts [] = \op->(Just {op&distributed=True}, ["Started server on port: " +++ toString op.serverPort])
	opts ["-p",p:as] = appFst (fmap (\o->{o & serverPort=toInt p})) o opts as
	opts [a:as] = opts as

	title = "Remote share test"

	flows = [ workflow "Tests/1" "Test 1: Get remote share" case1
			, workflow "Tests/2" "Test 2: Get and set remote share" case2
			, workflow "Tests/3" "Test 3: Update remote share (updateSharedInformation)" case3
			, workflow "Tests/4" "Test 4: Update parallel with left remote (updateSharedInformation)" case4
			, workflow "Tests/5" "Test 5: Update parallel with right remote (updateSharedInformation)" case5
			, workflow "Tests/6" "Test 6: Update parallel with left remote in parallel (updateSharedInformation)" case6
			, workflow "Tests/7" "Test 7: Update lens with remote sds (updateSharedInformation)" case7
			, workflow "View" "View local shares" localSdss]

	// 1. We can read from a remote share
	localSdss =  (viewSharedInformation "sharedStoreNamebla" [] testShare 
		-&&- viewSharedInformation "leftShare" [] leftShare
		-&&- viewSharedInformation "rightShare" [] rightShare
		-&&- viewSharedInformation "intShare" [] intShare) >>| return ()

	case1 = get simpleShare
		>>= viewInformation "Remote int share" []
		>>| return ()

	// 2. We can write to a remote share and retrieve the result, which should be the same
	case2 = enterInformation "Enter the new remote state" [] 
		>>= \i. set i remoteTestShare
		>>| get remoteTestShare
		>>= viewInformation "Retrieved remote state" []
		>>| return ()

	// 3. We can update a remote share
	case3 = updateSharedInformation "Updating remote state" [] remoteTestShare >>| return ()

	// 4. We can update a parallel share with a left remote part
	case4 = updateSharedInformation "Update share with left remote part" [] parallelWithLeftRemote >>| return ()

	// 5. We can update a parallel share with a right remote part 
	case5 = updateSharedInformation "Update share with right remote part" []  parallelWithRightRemote >>| return ()

	// 6. We can update the share at the same time and see the changes
	case6 = (updateSharedInformation "Update share with left remote part" [] parallelWithLeftRemote -||- updateSharedInformation "Update share with left remote part" [] parallelWithLeftRemote)
		>>| return ()
	// 7. We can translate a remote share
	case7 = updateSharedInformation "Update a translated remote share" [] projectedRemote >>| return ()

