module TestIntList
import iTasks, iTasks.Util.Testing

test :: Task [Int]
test = testCommonInteractions "Int list"

Start world = doTasks test world

