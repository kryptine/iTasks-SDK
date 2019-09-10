module TestAceTextAreaWithShare

import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditorWithShare aceTextArea "Hello world" Update

Start world = doTasks test world
