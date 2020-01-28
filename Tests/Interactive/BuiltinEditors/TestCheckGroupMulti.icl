module TestCheckGroupMulti
import iTasks, iTasks.Util.Testing
derive class iTask ChoiceText

test :: Task ([ChoiceText],[Int])
test = testEditor (lensEditor id (\mbcs sel -> Just (Just (maybe [] fst mbcs,sel))) checkGroup <<@ multipleAttr True)
	(Update ([{ChoiceText|id=0,text="A"},{ChoiceText|id=1,text="B"},{ChoiceText|id=2,text="C"}],[]))

Start world = doTasks test world
