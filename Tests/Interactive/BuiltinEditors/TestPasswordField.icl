module TestPasswordField
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor passwordField (Update "Hello world") 

Start world = doTasks test world

