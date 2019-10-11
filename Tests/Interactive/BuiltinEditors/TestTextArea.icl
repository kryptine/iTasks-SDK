module TestTextArea
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor textArea "Hello world" Update

Start world = doTasks test world

