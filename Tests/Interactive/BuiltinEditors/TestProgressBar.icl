module TestProgressBar
import iTasks, iTasks.Util.Testing

test :: Task (Maybe Int,Maybe String)
test = testEditor (mapEditorWrite Just progressBar) (Update (Just 90,Just "Almost done"))

Start world = doTasks test world
