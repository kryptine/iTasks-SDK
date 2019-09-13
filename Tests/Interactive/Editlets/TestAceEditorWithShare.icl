module TestAceEditorWithShare

import iTasks, iTasks.Util.Testing

test :: Task (AceOptions,AceState)
test = testEditorWithShare aceEditor defaultValue Update

Start world = doTasks test world
