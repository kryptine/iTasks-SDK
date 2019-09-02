module TestShareBetweenSessions
/*
* You should be able to start t0 and t1 that share a value in separate sessions (browser windows)
* editing in either one should be reflected in the other
*/
import iTasks

Start world = doTasks
	[ onReqeust "/t0" (const t0)
    , onRequest "/t1" (const t1)
	] world

myShare :: Shared Int
myShare = sharedStore "myShare" 0

t0 :: Task Int
t0 = Title "t0" @>> updateSharedInformation [] myShare

t1 :: Task Int
t1 = Title "t1" @>> updateSharedInformation [] myShare

