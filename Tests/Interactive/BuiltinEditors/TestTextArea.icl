module TestTextArea
import iTasks, iTasks.Util.Testing

test :: Task String
test = testEditor (mapEditorWrite Just textArea) (Update "Hello world")

Start world = doTasks test world

