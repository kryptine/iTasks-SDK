module TestHtmlView
import iTasks, iTasks.Util.Testing

test :: Task HtmlTag
test = testEditor htmlView (H2Tag [] [Text "Hello World"]) Update

Start world = doTasks test world
