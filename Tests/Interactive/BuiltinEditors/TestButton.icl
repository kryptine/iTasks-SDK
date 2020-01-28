module TestButton
import iTasks, iTasks.Util.Testing

test :: Task Bool
test = testEditor ((mapEditorWrite Just button) <<@ (textAttr "Click")) (Update False)

Start world = doTasks test world
