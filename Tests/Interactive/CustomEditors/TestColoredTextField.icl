module TestColoredTextField

import iTasks, iTasks.Internal.Test.Definition

test :: Task String
test = testEditor (withAttributes (styleAttr "background-color: pink") textField) "Hello world" Update

Start world = doTasks test world
