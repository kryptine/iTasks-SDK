module TestCheckbox

import iTasks, iTasks.Util.Testing

test :: Task Bool
test = testEditor checkBox False Update

Start world = doTasks test world

