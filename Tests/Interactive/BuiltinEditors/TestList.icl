module TestList
import iTasks, iTasks.Internal.Test.Definition 

test :: Task [String]
test = testEditor (listEditor (Just (const (Just "New item"))) True True (Just (\items -> length items +++> " items")) textField) [] Update

Start world = doTasks test world
