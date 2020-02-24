module TestButton
import iTasks, iTasks.Util.Testing

test :: Task Bool
test = testEditor (button <<@ (textAttr "Click")) (Update False)

Start world = doTasks test world
