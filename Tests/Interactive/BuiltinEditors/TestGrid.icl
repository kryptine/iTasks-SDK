module TestGrid
import iTasks, iTasks.Util.Testing, Text.HTML
derive class iTask ChoiceGrid, ChoiceRow

test :: Task (ChoiceGrid,[Int])
test = testEditor (grid <<@ multipleAttr False) (Update ({ChoiceGrid|header=["Key","Value"],rows=rows},[]))
where
    rows = [{ChoiceRow|id=1,cells=[Text "A",Text "1"]},{ChoiceRow|id=2,cells=[Text "B",Text "2"]},{ChoiceRow|id=3,cells=[Text "C",Text "3"]}]

Start world = doTasks test world
