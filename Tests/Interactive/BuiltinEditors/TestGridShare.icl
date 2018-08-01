module TestGridShare
import iTasks, iTasks.Internal.Test.Definition 

test :: Task (ChoiceGrid,[Int])
test = (testEditorWithShare (grid <<@ multipleAttr True) ({ChoiceGrid|header=["Key","Value"],rows=rows},[]) Update)
where
    rows = [{ChoiceRow|id=1,cells=[Text "A",Text "1"]},{ChoiceRow|id=2,cells=[Text "B",Text "2"]},{ChoiceRow|id=3,cells=[Text "C",Text "3"]}]

Start world = doTasks test world
