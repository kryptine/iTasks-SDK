module TestTextField
import iTasks, iTasks.Internal.Test.Definition 

test :: Task String
test = testEditor textField "Hello world" Update

Start world = startEngine test world

