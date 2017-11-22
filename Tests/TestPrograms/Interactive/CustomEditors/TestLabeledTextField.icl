module TestLabeledTextField
import iTasks, iTasks.Internal.Test.Definition

test :: Task String
test = testEditor (withLabelAttr "Foo" textField) "Hello world" Update

Start world = startEngine test world
