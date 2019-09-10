module TestDecimalField
import iTasks, iTasks.Util.Testing

test :: Task Real
test = testEditor decimalField 3.14 Update

Start world = doTasks test world
