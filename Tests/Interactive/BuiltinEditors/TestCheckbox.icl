module TestCheckbox

import iTasks, iTasks.Internal.Test.Definition 

test :: Task Bool
test = testEditor checkBox False Update

Start world = doTasks test world

