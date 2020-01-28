module TestPasswordField
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor (mapEditorWrite Just passwordField) (Update "Hello world")

Start world = doTasks test world

