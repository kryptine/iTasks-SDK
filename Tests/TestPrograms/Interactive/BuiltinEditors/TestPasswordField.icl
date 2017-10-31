module TestPasswordField
import iTasks, iTasks.Internal.Test.Definition 

test :: Task String
test = testEditor passwordField "Hello world" Update

Start world = startEngine test world

