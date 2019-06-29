module TestModifyAttributes
import iTasks

test :: Task ()
test = (updateInformation [] "Test for modifying attributes" @! () >>= return) <<@ ApplyLayout layout
where
    layout = modifyUIAttributes (SelectKeys ["direction"]) f
    f attr = maybe 'DM'.newMap (\(JSONString dir) -> optionalAttr (dir == "horizontal")) ('DM'.get "direction" attr)

Start world = doTasks test world
