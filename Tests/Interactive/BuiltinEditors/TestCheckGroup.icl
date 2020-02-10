module TestCheckGroup
import iTasks, iTasks.Util.Testing

derive class iTask ChoiceText

test :: Task ([ChoiceText],[Int])
test = testEditor (lensEditor (\_ x -> x) (\mbcs sel -> Just (Just (maybe [] fst mbcs,sel))) checkGroup)
	(Update ([{ChoiceText|id=0,text="A"},{ChoiceText|id=1,text="B"},{ChoiceText|id=2,text="C"}],[]))

Start world = doTasks test world

