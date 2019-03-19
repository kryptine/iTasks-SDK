module TestCombinedTextFields
import iTasks, iTasks.Internal.Test.Definition

test :: Task (String,String)
test = testEditor (container2 textField textField) ("Hello","world") Update

Start world = doTasks test world
