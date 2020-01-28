module TestIntegerField
import iTasks, iTasks.Util.Testing

test :: Task Int
test = testEditor integerField (Update 42)

Start world = doTasks test world

