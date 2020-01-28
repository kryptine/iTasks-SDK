module TestSlider
import iTasks, iTasks.Util.Testing
import qualified Data.Map as DM

test :: Task Int
test = testEditor (mapEditorWrite Just slider <<@ ('DM'.union (minAttr 1) (maxAttr 5))) (Update 3)

Start world = doTasks test world
