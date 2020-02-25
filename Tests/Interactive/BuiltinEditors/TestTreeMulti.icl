module TestTreeMulti
import iTasks, iTasks.Util.Testing
derive class iTask ChoiceNode

derive class iTask ChoiceNode

test :: Task ([ChoiceNode],[Int])
test = testEditor (lensEditor (\_ x -> x) (\mbcs sel -> Just (Just (maybe [] fst mbcs,sel))) tree <<@ multipleAttr True)
	(Update        
		([{ChoiceNode|id=1,label="A",icon=Nothing,expanded=False,children=[]}
        ,{ChoiceNode|id=2,label="B",icon=Nothing,expanded=False,children=[]}
        ,{ChoiceNode|id=3,label="C",icon=Nothing,expanded=False,children=[]}
        ],[])) 

Start world = doTasks test world
