module TestChar
import iTasks, iTasks.Util.Testing

test :: Task Char
test = testCommonInteractions "Char"

Start world = doTasks test world
