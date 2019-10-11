module TestIntegerField
import iTasks, iTasks.Util.Testing

test :: Task Int
test = testEditor integerField 42 Update

Start world = doTasks test world

