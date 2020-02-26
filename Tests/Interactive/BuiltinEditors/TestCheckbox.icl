module TestCheckbox

import iTasks, iTasks.Util.Testing

test :: Task Bool
test = testEditor checkBox (Update False)

Start world = doTasks test world

