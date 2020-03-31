module TestPanel
import iTasks

test :: Task String
test =   viewInformation [] "This is the content of the container"
        <<@ ApplyLayout (wrapUI UIPanel)
        <<@ ApplyLayout (setUIAttributes (titleAttr "Panel with title"))

Start world = doTasks test world
