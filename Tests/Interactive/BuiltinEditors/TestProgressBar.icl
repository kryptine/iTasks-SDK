module TestProgressBar
import iTasks, iTasks.Internal.Test.Definition 

test :: Task (Maybe Int,Maybe String)
test = testEditor progressBar (Just 90,Just "Almost done") Update

Start world = doTasks test world
