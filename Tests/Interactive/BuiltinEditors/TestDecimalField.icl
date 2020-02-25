module TestDecimalField
import iTasks, iTasks.Util.Testing

test :: Task Real
test = testEditor decimalField (Update 3.14)

Start world = doTasks test world
