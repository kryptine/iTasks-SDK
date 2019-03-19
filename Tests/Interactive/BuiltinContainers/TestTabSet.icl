module TestTabSet
import iTasks

test :: Task String
test =   viewInformation () [] "This is the content of the container"
     <<@ ApplyLayout (setUIAttributes (titleAttr "Tab title"))
     <<@ ApplyLayout (wrapUI UITabSet)

Start world = doTasks test world
