module TestTree
import iTasks, iTasks.Util.Testing

test :: Task ([ChoiceNode],[Int])
test = testEditor (tree <<@ multipleAttr True)
        ([{ChoiceNode|id=1,label="A",icon=Nothing,expanded=False,children=[]}
        ,{ChoiceNode|id=2,label="B",icon=Nothing,expanded=False,children=[]}
        ,{ChoiceNode|id=3,label="C",icon=Nothing,expanded=False,children=[]}
        ],[]) Update

Start world = doTasks test world
