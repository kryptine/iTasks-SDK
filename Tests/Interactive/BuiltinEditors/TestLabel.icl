module TestLabel
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor label "Hello world" Update

Start world = doTasks test world
