module TestInt
import iTasks, iTasks.Util.Testing

test :: Task Int
test = testCommonInteractions "Int"

Start world = doTasks test world
