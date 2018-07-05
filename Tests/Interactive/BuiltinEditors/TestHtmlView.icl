module TestHtmlView
import iTasks, iTasks.Internal.Test.Definition 

test :: Task HtmlTag
test = testEditor htmlView (H2Tag [] [Text "Hello World"]) Update

Start world = startEngine test world
