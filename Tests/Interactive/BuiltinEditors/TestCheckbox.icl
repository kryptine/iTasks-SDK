module TestCheckbox

import iTasks, iTasks.Util.Testing

test :: Task Bool
test = testEditor (mapEditorWrite Just checkBox) (Update False)

Start world = doTasks test world

