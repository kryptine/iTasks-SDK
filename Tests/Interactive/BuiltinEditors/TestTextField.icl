module TestTextField
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor textField (Update "Hello world")

Start world = doTasks test world

