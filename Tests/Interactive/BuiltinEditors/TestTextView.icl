module TestTextView
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor (mapEditorWrite Just textView) (Update "Hello World") 

Start world = doTasks test world
