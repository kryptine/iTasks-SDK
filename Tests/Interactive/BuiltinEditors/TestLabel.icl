module TestLabel
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor label (Update "Hello world")

Start world = doTasks test world
