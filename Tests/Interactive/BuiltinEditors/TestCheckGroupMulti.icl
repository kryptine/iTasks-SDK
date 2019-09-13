module TestCheckGroupMulti
import iTasks, iTasks.Util.Testing

test :: Task ([ChoiceText],[Int])
test = testEditor (checkGroup <<@ multipleAttr True) ([{ChoiceText|id=0,text="A"},{ChoiceText|id=1,text="B"},{ChoiceText|id=2,text="C"}],[]) Update

Start world = doTasks test world
