module TestAceTextAreaWithShare

import iTasks, iTasks.Internal.Test.Definition

test :: Task String
test = testEditorWithShare aceTextArea "Hello world" Update

Start world = doTasks test world
