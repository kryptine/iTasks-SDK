module TestList
import iTasks, iTasks.Util.Testing
import iTasks.UI.Editor.Common

test :: Task [String]
test = testEditor (mapEditorWrite Just (listEditor (Just (const (Just "New item"))) True True (Just (\items -> length items +++> " items")) textField)) (Update [])

Start world = doTasks test world
