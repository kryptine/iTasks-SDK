module TestChoiceListMulti
import iTasks, iTasks.Internal.Test.Definition 

test :: Task ([ChoiceText],[Int])
test = testEditor (choiceList <<@ multipleAttr True) ([{ChoiceText|id=0,text="A"},{ChoiceText|id=1,text="B"},{ChoiceText|id=2,text="C"}],[]) Update

Start world = startEngine test world
