module TestShareBetweenSessions
/*
* You should be able to start t0 and t1 that share a value in separate sessions (browser windows)
* editing in either one should be reflected in the other
*/
import iTasks

Start world = startEngine [ publish "/t0" (const t0)
                          , publish "/t1" (const t1)
                          ] world

myShare :: Shared Int
myShare = sharedStore "myShare" 0

t0 :: Task Int
t0 = updateSharedInformation "t0" [] myShare

t1 :: Task Int
t1 = updateSharedInformation "t1" [] myShare

