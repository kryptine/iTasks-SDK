module TestPasswordField
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor passwordField "Hello world" Update

Start world = doTasks test world

