module TestAceTextArea

import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor aceTextArea "Hello world" Update

Start world = doTasks test world
