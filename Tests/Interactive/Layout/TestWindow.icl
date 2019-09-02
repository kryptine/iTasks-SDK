module TestWindow

import iTasks

test = Hint "Press the button to open a window" @>> viewInformation  [] ()
     >>| taskInWindow
     >>| Title "Done" @>> viewInformation [] ()
where
    taskInWindow = (Title "Test window" @>> viewInformation [] "Hello!" >>* [OnAction ActionClose (always (return ()))]) <<@ InWindow

Start world = doTasks test world
