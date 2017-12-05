module TestCustomList
import iTasks, iTasks.Internal.Test.Definition 

:: List = Nil | Cons Int List
derive class iTask List

test :: Task List
test = testCommonInteractions "Custom list"

Start world = startEngine test world
