module TestWindow
import iTasks

test :: Task String
test =   viewInformation [] "This is the content of the container"
     <<@ ApplyLayout (wrapUI UIWindow)
     <<@ ApplyLayout (setUIAttributes (titleAttr "Window with title"))

Start world = doTasks test world
