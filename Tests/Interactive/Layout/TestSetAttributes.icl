module TestSetAttributes
import iTasks

test :: Task ()
test = (updateInformation [] "Test for setting an attribute" @! () >>! return) <<@ ApplyLayout layout
where
    layout = setUIAttributes (styleAttr "background: #f0f")

Start world = doTasks test world
