module TestIcon
import iTasks, iTasks.Util.Testing

test :: Task (String,Maybe String)
test = testEditor icon ("icon-valid",Just "Icon with a tooltip!") Update

Start world = doTasks test world
