module TestButton
import iTasks, iTasks.Util.Testing

test :: Task Bool
test = testEditor (button <<@ (textAttr "Click")) False Update

Start world = doTasks test world
