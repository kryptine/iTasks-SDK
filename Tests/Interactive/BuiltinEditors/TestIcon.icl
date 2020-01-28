module TestIcon
import iTasks, iTasks.Util.Testing

test :: Task (String,Maybe String)
test = testEditor (mapEditorWrite Just icon) (Update ("icon-valid",Just "Icon with a tooltip!"))

Start world = doTasks test world
