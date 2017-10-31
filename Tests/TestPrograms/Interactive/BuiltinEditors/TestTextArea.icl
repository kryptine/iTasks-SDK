module TestTextArea
import iTasks, iTasks.Internal.Test.Definition 

test :: Task String
test = testEditor textArea "Hello world" Update

Start world = startEngine test world

