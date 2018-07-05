module TestDocumentField
import iTasks, iTasks.Internal.Test.Definition 

test :: Task (!String,!String,!String,!String,!Int)
test = testEditor documentField defaultValue Enter

Start world = startEngine test world

