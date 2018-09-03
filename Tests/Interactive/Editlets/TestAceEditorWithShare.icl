module TestAceEditorWithShare

import iTasks, iTasks.Internal.Test.Definition

test :: Task (AceOptions,AceState)
test = testEditorWithShare aceEditor defaultValue Update

Start world = doTasks test world
