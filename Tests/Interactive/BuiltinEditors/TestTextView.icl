module TestTextView
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor textView (Update "Hello World")

Start world = doTasks test world
