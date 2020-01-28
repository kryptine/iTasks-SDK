module TestTextField
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor (mapEditorWrite Just textField) (Update "Hello world")

Start world = doTasks test world

