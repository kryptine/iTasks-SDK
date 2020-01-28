module TestLabel
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor (ignoreEditorWrites label) (Update "Hello world")

Start world = doTasks test world
