module TestHtmlView
import iTasks, iTasks.Util.Testing, Text.HTML

test :: Task HtmlTag
test = testEditor (ignoreEditorWrites htmlView) (View (H2Tag [] [Text "Hello World"]))

Start world = doTasks test world
