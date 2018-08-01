module TestTextView
import iTasks, iTasks.Internal.Test.Definition 

test :: Task String
test = testEditor textView "Hello World" Update

Start world = doTasks test world
