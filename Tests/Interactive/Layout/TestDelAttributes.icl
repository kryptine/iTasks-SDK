module TestDelAttributes
import iTasks

test :: Task ()
test = (updateInformation [] "Test for deleting an attribute" @! () >> return) <<@ ApplyLayout layout
where
    layout = delUIAttributes (SelectKeys ["direction"])

Start world = doTasks test world
