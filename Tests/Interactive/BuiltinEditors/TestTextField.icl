module TestTextField
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor textField "Hello world" Update

Start world = doTasks test world

