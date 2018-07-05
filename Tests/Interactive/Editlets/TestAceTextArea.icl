module TestAceTextArea

import iTasks, iTasks.Internal.Test.Definition

test :: Task String
test = testEditor aceTextArea "Hello world" Update

Start world = startEngine test world
