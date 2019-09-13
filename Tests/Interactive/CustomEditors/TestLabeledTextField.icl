module TestLabeledTextField
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor (withLabelAttr "Foo" textField) "Hello world" Update

Start world = doTasks test world
