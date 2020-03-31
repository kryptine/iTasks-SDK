module TestButtonBar
import iTasks

test :: Task String
test =   viewInformation [] "This is the content of the container"
     <<@ ApplyLayout (wrapUI UIButtonBar) 

Start world = doTasks test world
