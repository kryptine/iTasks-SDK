module TestMixedCombinedTextFields
import iTasks, iTasks.Internal.Test.Definition

test :: Task (String,String)
test = testEditor editor ("Hello","world") Update
where   
    editor = container2 username password
    username = pink (withLabelAttr "Username" textField)
    password = pink (withLabelAttr "Password" passwordField)
    pink e = withAttributes (styleAttr "background-color: pink") e

Start world = startEngine test world
