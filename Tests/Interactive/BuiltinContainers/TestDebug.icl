module TestDebug
import iTasks

test :: Task String
test =   viewInformation [] "This is the content of the container"
     <<@ ApplyLayout (wrapUI UIDebug)

Start world = doTasks test world
