module TestModifyAttributesTwice
import iTasks
import qualified Data.Map as DM

test :: Task ()
test = (updateInformation [] "Test for modifying attributes" @! () >>= return) <<@ ApplyLayout layout1 <<@ ApplyLayout layout2
where
    layout1 = modifyUIAttributes (SelectKeys ["class"]) (addClass "A")
    layout2 = modifyUIAttributes (SelectKeys ["class"]) (addClass "B")

	addClass name attr = 'DM'.put "class" (maybe
 		(JSONArray [JSONString name])
		(\(JSONArray names) -> JSONArray (names ++ [JSONString name]))
		('DM'.get "class" attr)) attr

Start world = doTasks test world
