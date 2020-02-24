module TestHtmlView
import iTasks, iTasks.Util.Testing, Text.HTML

test :: Task HtmlTag
test = testEditor htmlView (Update (H2Tag [] [Text "Hello World"])) 

Start world = doTasks test world
