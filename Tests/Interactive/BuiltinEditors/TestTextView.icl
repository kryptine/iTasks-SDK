module TestTextView
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor textView "Hello World" Update

Start world = doTasks test world
