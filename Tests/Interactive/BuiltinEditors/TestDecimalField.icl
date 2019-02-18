module TestDecimalField
import iTasks, iTasks.Internal.Test.Definition 

test :: Task Real
test = testEditor decimalField 3.14 Update

Start world = doTasks test world
