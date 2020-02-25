module TestDocumentField
import iTasks, iTasks.Util.Testing

test :: Task (!String,!String,!String,!String,!Int)
test = testEditor documentField Enter

Start world = doTasks test world

