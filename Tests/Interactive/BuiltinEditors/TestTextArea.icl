module TestTextArea
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor textArea (Update "Hello world")

Start world = doTasks test world

