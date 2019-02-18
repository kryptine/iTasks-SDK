module TestLabel
import iTasks, iTasks.Internal.Test.Definition 

test :: Task String
test = testEditor label "Hello world" Update

Start world = doTasks test world
