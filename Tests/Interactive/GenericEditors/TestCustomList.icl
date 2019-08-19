module TestCustomList
import iTasks, iTasks.Util.Testing

:: List = Nil | Cons Int List
derive class iTask List
derive gDefault List

test :: Task List
test = testCommonInteractions "Custom list"

Start world = doTasks test world
