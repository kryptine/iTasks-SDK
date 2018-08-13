module TestIntegerField
import iTasks, iTasks.Internal.Test.Definition 

test :: Task Int
test = testEditor integerField 42 Update

Start world = doTasks test world

