module TestTree
import iTasks, iTasks.Util.Testing
derive class iTask ChoiceNode

test :: Task ([ChoiceNode],[Int])
test = testEditor (tree <<@ multipleAttr False)
        (Update ([{ChoiceNode|id=1,label="A",icon=Nothing,expanded=False,children=[]}
        ,{ChoiceNode|id=2,label="B",icon=Nothing,expanded=False,children=[]}
        ,{ChoiceNode|id=3,label="C",icon=Nothing,expanded=False,children=[]}
        ],[])) 

Start world = doTasks test world

